//===------- X86ExpandPseudo.cpp - Expand pseudo instructions -------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains a pass that expands pseudo instructions into target
// instructions to allow proper scheduling, if-conversion, other late
// optimizations, or simply the encoding of the instructions.
//
//===----------------------------------------------------------------------===//

#include "X86.h"
#include "X86FrameLowering.h"
#include "X86InstrBuilder.h"
#include "X86InstrInfo.h"
#include "X86MachineFunctionInfo.h"
#include "X86Subtarget.h"
#include "llvm/Analysis/EHPersonalities.h"
#include "llvm/CodeGen/LivePhysRegs.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/Passes.h" // For IDs of passes that are preserved.
#include "llvm/CodeGen/StackMaps.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCDwarf.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Target/TargetMachine.h"
using namespace llvm;

#define DEBUG_TYPE "x86-pseudo"
#define X86_EXPAND_PSEUDO_NAME "X86 pseudo instruction expansion pass"

static constexpr int64_t OxCamlDomainExnHandlerOffset = 48;

static bool isOxCamlCallingConv(CallingConv::ID CC) {
  switch (CC) {
  case CallingConv::OxCaml_WithFP:
  case CallingConv::OxCaml_WithoutFP:
  case CallingConv::OxCaml_C_Call:
  case CallingConv::OxCaml_C_Call_StackArgs:
  case CallingConv::OxCaml_C_Direct_Call:
  case CallingConv::OxCaml_Alloc:
    return true;
  default:
    return false;
  }
}

static void buildCFIAdjustCfaOffset(MachineBasicBlock &MBB,
                                    MachineBasicBlock::iterator MBBI,
                                    const DebugLoc &DL, const X86InstrInfo *TII,
                                    int Offset) {
  MachineFunction &MF = *MBB.getParent();
  unsigned CFIIndex =
      MF.addFrameInst(MCCFIInstruction::createAdjustCfaOffset(nullptr, Offset));
  BuildMI(MBB, MBBI, DL, TII->get(TargetOpcode::CFI_INSTRUCTION))
      .addCFIIndex(CFIIndex);
}

static bool isCamlRaiseExnLikeName(StringRef Name) {
  Name.consume_front("\01");
  return Name == "caml_raise_exn" || Name == "caml_reraise_exn";
}

static bool isCamlRaiseExnLikeStatepoint(const MachineInstr &MI) {
  if (MI.getOpcode() != TargetOpcode::STATEPOINT)
    return false;
  const MachineOperand &Target = StatepointOpers(&MI).getCallTarget();
  if (Target.isGlobal())
    return isCamlRaiseExnLikeName(Target.getGlobal()->getName());
  if (Target.isSymbol())
    return isCamlRaiseExnLikeName(Target.getSymbolName());
  return false;
}

static bool needsDwarfCFI(const MachineFunction &MF) {
  return !MF.getTarget().getMCAsmInfo()->usesWindowsCFI() &&
         MF.needsFrameMoves();
}

namespace {
class X86ExpandPseudo : public MachineFunctionPass {
public:
  static char ID;
  X86ExpandPseudo() : MachineFunctionPass(ID) {}

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.setPreservesCFG();
    AU.addPreservedID(MachineLoopInfoID);
    AU.addPreservedID(MachineDominatorsID);
    MachineFunctionPass::getAnalysisUsage(AU);
  }

  const X86Subtarget *STI = nullptr;
  const X86InstrInfo *TII = nullptr;
  const X86RegisterInfo *TRI = nullptr;
  const X86MachineFunctionInfo *X86FI = nullptr;
  const X86FrameLowering *X86FL = nullptr;

  bool runOnMachineFunction(MachineFunction &Fn) override;

  MachineFunctionProperties getRequiredProperties() const override {
    return MachineFunctionProperties().set(
        MachineFunctionProperties::Property::NoVRegs);
  }

  StringRef getPassName() const override {
    return "X86 pseudo instruction expansion pass";
  }

private:
  void ExpandICallBranchFunnel(MachineBasicBlock *MBB,
                               MachineBasicBlock::iterator MBBI);
  void expandCALL_RVMARKER(MachineBasicBlock &MBB,
                           MachineBasicBlock::iterator MBBI);
  void expandOXCAML_C_DIRECT_CALL(MachineBasicBlock &MBB,
                                  MachineBasicBlock::iterator MBBI);
  void expandOXCAML_PUSH_TRAP(MachineBasicBlock &MBB,
                              MachineBasicBlock::iterator MBBI);
  void expandOXCAML_PUSH_TRAP_DEAD(MachineBasicBlock &MBB,
                                   MachineBasicBlock::iterator MBBI);
  void expandOXCAML_TRAP_RECOVER(MachineBasicBlock &MBB,
                                 MachineBasicBlock::iterator MBBI);
  void expandOXCAML_POP_TRAP(MachineBasicBlock &MBB,
                             MachineBasicBlock::iterator MBBI);
  void expandOXCAML_RAISE_NOTRACE(MachineBasicBlock &MBB,
                                  MachineBasicBlock::iterator MBBI);
  bool ExpandMI(MachineBasicBlock &MBB, MachineBasicBlock::iterator MBBI);
  bool ExpandMBB(MachineBasicBlock &MBB);
  bool useOxCamlRspBasedCFI(const MachineFunction &MF) const;

  /// This function expands pseudos which affects control flow.
  /// It is done in separate pass to simplify blocks navigation in main
  /// pass(calling ExpandMBB).
  bool ExpandPseudosWhichAffectControlFlow(MachineFunction &MF);

  /// Expand X86::VASTART_SAVE_XMM_REGS into set of xmm copying instructions,
  /// placed into separate block guarded by check for al register(for SystemV
  /// abi).
  void ExpandVastartSaveXmmRegs(
      MachineBasicBlock *MBB,
      MachineBasicBlock::iterator VAStartPseudoInstr) const;
};
char X86ExpandPseudo::ID = 0;

} // End anonymous namespace.

INITIALIZE_PASS(X86ExpandPseudo, DEBUG_TYPE, X86_EXPAND_PSEUDO_NAME, false,
                false)

void X86ExpandPseudo::ExpandICallBranchFunnel(
    MachineBasicBlock *MBB, MachineBasicBlock::iterator MBBI) {
  MachineBasicBlock *JTMBB = MBB;
  MachineInstr *JTInst = &*MBBI;
  MachineFunction *MF = MBB->getParent();
  const BasicBlock *BB = MBB->getBasicBlock();
  auto InsPt = MachineFunction::iterator(MBB);
  ++InsPt;

  std::vector<std::pair<MachineBasicBlock *, unsigned>> TargetMBBs;
  const DebugLoc &DL = JTInst->getDebugLoc();
  MachineOperand Selector = JTInst->getOperand(0);
  const GlobalValue *CombinedGlobal = JTInst->getOperand(1).getGlobal();

  auto CmpTarget = [&](unsigned Target) {
    if (Selector.isReg())
      MBB->addLiveIn(Selector.getReg());
    BuildMI(*MBB, MBBI, DL, TII->get(X86::LEA64r), X86::R11)
        .addReg(X86::RIP)
        .addImm(1)
        .addReg(0)
        .addGlobalAddress(CombinedGlobal,
                          JTInst->getOperand(2 + 2 * Target).getImm())
        .addReg(0);
    BuildMI(*MBB, MBBI, DL, TII->get(X86::CMP64rr))
        .add(Selector)
        .addReg(X86::R11);
  };

  auto CreateMBB = [&]() {
    auto *NewMBB = MF->CreateMachineBasicBlock(BB);
    MBB->addSuccessor(NewMBB);
    if (!MBB->isLiveIn(X86::EFLAGS))
      MBB->addLiveIn(X86::EFLAGS);
    return NewMBB;
  };

  auto EmitCondJump = [&](unsigned CC, MachineBasicBlock *ThenMBB) {
    BuildMI(*MBB, MBBI, DL, TII->get(X86::JCC_1)).addMBB(ThenMBB).addImm(CC);

    auto *ElseMBB = CreateMBB();
    MF->insert(InsPt, ElseMBB);
    MBB = ElseMBB;
    MBBI = MBB->end();
  };

  auto EmitCondJumpTarget = [&](unsigned CC, unsigned Target) {
    auto *ThenMBB = CreateMBB();
    TargetMBBs.push_back({ThenMBB, Target});
    EmitCondJump(CC, ThenMBB);
  };

  auto EmitTailCall = [&](unsigned Target) {
    BuildMI(*MBB, MBBI, DL, TII->get(X86::TAILJMPd64))
        .add(JTInst->getOperand(3 + 2 * Target));
  };

  std::function<void(unsigned, unsigned)> EmitBranchFunnel =
      [&](unsigned FirstTarget, unsigned NumTargets) {
    if (NumTargets == 1) {
      EmitTailCall(FirstTarget);
      return;
    }

    if (NumTargets == 2) {
      CmpTarget(FirstTarget + 1);
      EmitCondJumpTarget(X86::COND_B, FirstTarget);
      EmitTailCall(FirstTarget + 1);
      return;
    }

    if (NumTargets < 6) {
      CmpTarget(FirstTarget + 1);
      EmitCondJumpTarget(X86::COND_B, FirstTarget);
      EmitCondJumpTarget(X86::COND_E, FirstTarget + 1);
      EmitBranchFunnel(FirstTarget + 2, NumTargets - 2);
      return;
    }

    auto *ThenMBB = CreateMBB();
    CmpTarget(FirstTarget + (NumTargets / 2));
    EmitCondJump(X86::COND_B, ThenMBB);
    EmitCondJumpTarget(X86::COND_E, FirstTarget + (NumTargets / 2));
    EmitBranchFunnel(FirstTarget + (NumTargets / 2) + 1,
                  NumTargets - (NumTargets / 2) - 1);

    MF->insert(InsPt, ThenMBB);
    MBB = ThenMBB;
    MBBI = MBB->end();
    EmitBranchFunnel(FirstTarget, NumTargets / 2);
  };

  EmitBranchFunnel(0, (JTInst->getNumOperands() - 2) / 2);
  for (auto P : TargetMBBs) {
    MF->insert(InsPt, P.first);
    BuildMI(P.first, DL, TII->get(X86::TAILJMPd64))
        .add(JTInst->getOperand(3 + 2 * P.second));
  }
  JTMBB->erase(JTInst);
}

void X86ExpandPseudo::expandCALL_RVMARKER(MachineBasicBlock &MBB,
                                          MachineBasicBlock::iterator MBBI) {
  // Expand CALL_RVMARKER pseudo to call instruction, followed by the special
  //"movq %rax, %rdi" marker.
  MachineInstr &MI = *MBBI;

  MachineInstr *OriginalCall;
  assert((MI.getOperand(1).isGlobal() || MI.getOperand(1).isReg()) &&
         "invalid operand for regular call");
  unsigned Opc = -1;
  if (MI.getOpcode() == X86::CALL64m_RVMARKER)
    Opc = X86::CALL64m;
  else if (MI.getOpcode() == X86::CALL64r_RVMARKER)
    Opc = X86::CALL64r;
  else if (MI.getOpcode() == X86::CALL64pcrel32_RVMARKER)
    Opc = X86::CALL64pcrel32;
  else
    llvm_unreachable("unexpected opcode");

  OriginalCall = BuildMI(MBB, MBBI, MI.getDebugLoc(), TII->get(Opc)).getInstr();
  bool RAXImplicitDead = false;
  for (MachineOperand &Op : llvm::drop_begin(MI.operands())) {
    // RAX may be 'implicit dead', if there are no other users of the return
    // value. We introduce a new use, so change it to 'implicit def'.
    if (Op.isReg() && Op.isImplicit() && Op.isDead() &&
        TRI->regsOverlap(Op.getReg(), X86::RAX)) {
      Op.setIsDead(false);
      Op.setIsDef(true);
      RAXImplicitDead = true;
    }
    OriginalCall->addOperand(Op);
  }

  // Emit marker "movq %rax, %rdi".  %rdi is not callee-saved, so it cannot be
  // live across the earlier call. The call to the ObjC runtime function returns
  // the first argument, so the value of %rax is unchanged after the ObjC
  // runtime call. On Windows targets, the runtime call follows the regular
  // x64 calling convention and expects the first argument in %rcx.
  auto TargetReg = STI->getTargetTriple().isOSWindows() ? X86::RCX : X86::RDI;
  auto *Marker = BuildMI(MBB, MBBI, MI.getDebugLoc(), TII->get(X86::MOV64rr))
                     .addReg(TargetReg, RegState::Define)
                     .addReg(X86::RAX)
                     .getInstr();
  if (MI.shouldUpdateCallSiteInfo())
    MBB.getParent()->moveCallSiteInfo(&MI, Marker);

  // Emit call to ObjC runtime.
  const uint32_t *RegMask =
      TRI->getCallPreservedMask(*MBB.getParent(), CallingConv::C);
  MachineInstr *RtCall =
      BuildMI(MBB, MBBI, MI.getDebugLoc(), TII->get(X86::CALL64pcrel32))
          .addGlobalAddress(MI.getOperand(0).getGlobal(), 0, 0)
          .addRegMask(RegMask)
          .addReg(X86::RAX,
                  RegState::Implicit |
                      (RAXImplicitDead ? (RegState::Dead | RegState::Define)
                                       : RegState::Define))
          .getInstr();
  MI.eraseFromParent();

  auto &TM = MBB.getParent()->getTarget();
  // On Darwin platforms, wrap the expanded sequence in a bundle to prevent
  // later optimizations from breaking up the sequence.
  if (TM.getTargetTriple().isOSDarwin())
    finalizeBundle(MBB, OriginalCall->getIterator(),
                   std::next(RtCall->getIterator()));
}

void X86ExpandPseudo::expandOXCAML_C_DIRECT_CALL(
    MachineBasicBlock &MBB, MachineBasicBlock::iterator MBBI) {
  MachineInstr &MI = *MBBI;
  const DebugLoc &DL = MI.getDebugLoc();
  MachineFunction &MF = *MBB.getParent();

  unsigned CallOpc;
  unsigned FirstCallExtraOp;
  switch (MI.getOpcode()) {
  case X86::CALL64m_OXCAML_C_DIRECT:
  {
    MachineInstrBuilder MIB =
        BuildMI(MBB, MBBI, DL, TII->get(X86::MOV64rm), X86::R11);
    for (unsigned I = 0; I != X86::AddrNumOperands; ++I)
      MIB.add(MI.getOperand(I));
    CallOpc = X86::CALL64r;
    FirstCallExtraOp = X86::AddrNumOperands;
    break;
  }
  case X86::CALL64r_OXCAML_C_DIRECT:
    BuildMI(MBB, MBBI, DL, TII->get(X86::MOV64rr), X86::R11)
        .add(MI.getOperand(0));
    CallOpc = X86::CALL64r;
    FirstCallExtraOp = 1;
    break;
  case X86::CALL64pcrel32_OXCAML_C_DIRECT:
    CallOpc = X86::CALL64pcrel32;
    FirstCallExtraOp = 1;
    break;
  default:
    llvm_unreachable("unexpected opcode");
  }

  MachineInstr *First =
      BuildMI(MBB, MBBI, DL, TII->get(X86::MOV64rr), X86::R13)
          .addReg(X86::RSP)
          .getInstr();

  unsigned RememberState =
      MF.addFrameInst(MCCFIInstruction::createRememberState(nullptr));
  BuildMI(MBB, MBBI, DL, TII->get(TargetOpcode::CFI_INSTRUCTION))
      .addCFIIndex(RememberState);

  unsigned DwarfR13 = TRI->getDwarfRegNum(X86::R13, true);
  unsigned DefCfaR13 =
      MF.addFrameInst(MCCFIInstruction::createDefCfaRegister(nullptr, DwarfR13));
  BuildMI(MBB, MBBI, DL, TII->get(TargetOpcode::CFI_INSTRUCTION))
      .addCFIIndex(DefCfaR13);

  // Domain_c_stack is field 13 in runtime/caml/domain_state.tbl.
  addRegOffset(BuildMI(MBB, MBBI, DL, TII->get(X86::MOV64rm), X86::RSP),
               X86::R14, false, 13 * 8);

  MachineInstr *Call = BuildMI(MBB, MBBI, DL, TII->get(CallOpc)).getInstr();
  if (MI.getOpcode() == X86::CALL64m_OXCAML_C_DIRECT ||
      MI.getOpcode() == X86::CALL64r_OXCAML_C_DIRECT)
    Call->addOperand(MachineOperand::CreateReg(X86::R11, false));
  else
    Call->addOperand(MI.getOperand(0));
  for (unsigned I = FirstCallExtraOp, E = MI.getNumOperands(); I != E; ++I) {
    const MachineOperand &MO = MI.getOperand(I);
    if (MO.isReg() && MO.isDef() && MO.isImplicit() && MO.getReg() == X86::R13)
      continue;
    Call->addOperand(MI.getOperand(I));
  }
  Call->setCFIType(MF, MI.getCFIType());

  MachineInstr *Last =
      BuildMI(MBB, MBBI, DL, TII->get(X86::MOV64rr), X86::RSP)
          .addReg(X86::R13)
          .getInstr();

  unsigned RestoreState =
      MF.addFrameInst(MCCFIInstruction::createRestoreState(nullptr));
  BuildMI(MBB, MBBI, DL, TII->get(TargetOpcode::CFI_INSTRUCTION))
      .addCFIIndex(RestoreState);

  if (MI.shouldUpdateCallSiteInfo())
    MBB.getParent()->moveCallSiteInfo(&MI, Call);

  MI.eraseFromParent();
  finalizeBundle(MBB, First->getIterator(), std::next(Last->getIterator()));
}

bool X86ExpandPseudo::useOxCamlRspBasedCFI(const MachineFunction &MF) const {
  const MachineFrameInfo &MFI = MF.getFrameInfo();
  return isOxCamlCallingConv(MF.getFunction().getCallingConv()) &&
         needsDwarfCFI(MF) && X86FL->hasFP(MF) &&
         !TRI->hasStackRealignment(MF) && !MFI.hasVarSizedObjects();
}

void X86ExpandPseudo::expandOXCAML_PUSH_TRAP(
    MachineBasicBlock &MBB, MachineBasicBlock::iterator MBBI) {
  MachineInstr &MI = *MBBI;
  const DebugLoc &DL = MI.getDebugLoc();
  MachineBasicBlock *RecoveryMBB = MI.getOperand(0).getMBB();
  bool NeedsTrapCFI = useOxCamlRspBasedCFI(*MBB.getParent());

  BuildMI(MBB, MBBI, DL, TII->get(X86::LEA64r), X86::R11)
      .addReg(X86::RIP)
      .addImm(1)
      .addReg(0)
      .addMBB(RecoveryMBB)
      .addReg(0);
  BuildMI(MBB, MBBI, DL, TII->get(X86::PUSH64r)).addReg(X86::R11);
  if (NeedsTrapCFI)
    buildCFIAdjustCfaOffset(MBB, MBBI, DL, TII, 8);
  addRegOffset(BuildMI(MBB, MBBI, DL, TII->get(X86::PUSH64rmm)), X86::R14,
               false, OxCamlDomainExnHandlerOffset);
  if (NeedsTrapCFI)
    buildCFIAdjustCfaOffset(MBB, MBBI, DL, TII, 8);
  addRegOffset(BuildMI(MBB, MBBI, DL, TII->get(X86::MOV64mr)), X86::R14,
               false, OxCamlDomainExnHandlerOffset)
      .addReg(X86::RSP);

  MI.eraseFromParent();
}

void X86ExpandPseudo::expandOXCAML_PUSH_TRAP_DEAD(
    MachineBasicBlock &MBB, MachineBasicBlock::iterator MBBI) {
  MachineInstr &MI = *MBBI;
  const DebugLoc &DL = MI.getDebugLoc();
  bool NeedsTrapCFI = useOxCamlRspBasedCFI(*MBB.getParent());

  BuildMI(MBB, MBBI, DL, TII->get(X86::PUSH64i8)).addImm(0);
  if (NeedsTrapCFI)
    buildCFIAdjustCfaOffset(MBB, MBBI, DL, TII, 8);
  addRegOffset(BuildMI(MBB, MBBI, DL, TII->get(X86::PUSH64rmm)), X86::R14,
               false, OxCamlDomainExnHandlerOffset);
  if (NeedsTrapCFI)
    buildCFIAdjustCfaOffset(MBB, MBBI, DL, TII, 8);
  addRegOffset(BuildMI(MBB, MBBI, DL, TII->get(X86::MOV64mr)), X86::R14,
               false, OxCamlDomainExnHandlerOffset)
      .addReg(X86::RSP);

  MI.eraseFromParent();
}

void X86ExpandPseudo::expandOXCAML_TRAP_RECOVER(
    MachineBasicBlock &MBB, MachineBasicBlock::iterator MBBI) {
  MachineInstr &MI = *MBBI;
  const DebugLoc &DL = MI.getDebugLoc();
  MachineFunction &MF = *MBB.getParent();
  uint64_t OuterTrapBytes = MI.getOperand(0).getImm();
  uint64_t StackSize = MF.getFrameInfo().getStackSize();
  if (StackSize < 8)
    report_fatal_error("OxCaml trap recovery requires an AMD64 frame");
  uint64_t RestoreOffset = StackSize - 8 + OuterTrapBytes;

  BuildMI(MBB, MBBI, DL, TII->get(X86::LEA64r), X86::RBP)
      .addReg(X86::RSP)
      .addImm(1)
      .addReg(0)
      .addImm(RestoreOffset)
      .addReg(0);

  MI.eraseFromParent();
}

void X86ExpandPseudo::expandOXCAML_POP_TRAP(
    MachineBasicBlock &MBB, MachineBasicBlock::iterator MBBI) {
  MachineInstr &MI = *MBBI;
  const DebugLoc &DL = MI.getDebugLoc();
  bool NeedsTrapCFI = useOxCamlRspBasedCFI(*MBB.getParent());

  addRegOffset(BuildMI(MBB, MBBI, DL, TII->get(X86::POP64rmm)), X86::R14,
               false, OxCamlDomainExnHandlerOffset);
  if (NeedsTrapCFI)
    buildCFIAdjustCfaOffset(MBB, MBBI, DL, TII, -8);
  BuildMI(MBB, MBBI, DL, TII->get(X86::POP64r), X86::R11);
  if (NeedsTrapCFI)
    buildCFIAdjustCfaOffset(MBB, MBBI, DL, TII, -8);

  MI.eraseFromParent();
}

void X86ExpandPseudo::expandOXCAML_RAISE_NOTRACE(
    MachineBasicBlock &MBB, MachineBasicBlock::iterator MBBI) {
  MachineInstr &MI = *MBBI;
  const DebugLoc &DL = MI.getDebugLoc();
  Register ExnBucket = MI.getOperand(0).getReg();

  if (ExnBucket != X86::RAX)
    BuildMI(MBB, MBBI, DL, TII->get(X86::MOV64rr), X86::RAX).addReg(ExnBucket);
  addRegOffset(BuildMI(MBB, MBBI, DL, TII->get(X86::MOV64rm), X86::RSP),
               X86::R14, false, OxCamlDomainExnHandlerOffset);
  addRegOffset(BuildMI(MBB, MBBI, DL, TII->get(X86::POP64rmm)), X86::R14,
               false, OxCamlDomainExnHandlerOffset);
  BuildMI(MBB, MBBI, DL, TII->get(X86::POP64r), X86::R11);
  BuildMI(MBB, MBBI, DL, TII->get(X86::JMP64r)).addReg(X86::R11);

  MI.eraseFromParent();
}

/// If \p MBBI is a pseudo instruction, this method expands
/// it to the corresponding (sequence of) actual instruction(s).
/// \returns true if \p MBBI has been expanded.
bool X86ExpandPseudo::ExpandMI(MachineBasicBlock &MBB,
                               MachineBasicBlock::iterator MBBI) {
  MachineInstr &MI = *MBBI;
  unsigned Opcode = MI.getOpcode();
  const DebugLoc &DL = MBBI->getDebugLoc();
  switch (Opcode) {
  default:
    return false;
  case X86::CALL64m_OXCAML_C_DIRECT:
  case X86::CALL64r_OXCAML_C_DIRECT:
  case X86::CALL64pcrel32_OXCAML_C_DIRECT:
    expandOXCAML_C_DIRECT_CALL(MBB, MBBI);
    return true;
  case X86::OXCAML_PUSH_TRAP:
    expandOXCAML_PUSH_TRAP(MBB, MBBI);
    return true;
  case X86::OXCAML_PUSH_TRAP_DEAD:
    expandOXCAML_PUSH_TRAP_DEAD(MBB, MBBI);
    return true;
  case X86::OXCAML_TRAP_RECOVER:
    expandOXCAML_TRAP_RECOVER(MBB, MBBI);
    return true;
  case X86::OXCAML_POP_TRAP:
    expandOXCAML_POP_TRAP(MBB, MBBI);
    return true;
  case X86::OXCAML_RAISE_NOTRACE:
  case X86::OXCAML_RAISE_NOTRACE_EDGE:
    expandOXCAML_RAISE_NOTRACE(MBB, MBBI);
    return true;
  case X86::TCRETURNdi:
  case X86::TCRETURNdicc:
  case X86::TCRETURNri:
  case X86::TCRETURNmi:
  case X86::TCRETURNdi64:
  case X86::TCRETURNdi64cc:
  case X86::TCRETURNri64:
  case X86::TCRETURNmi64: {
    bool isMem = Opcode == X86::TCRETURNmi || Opcode == X86::TCRETURNmi64;
    MachineOperand &JumpTarget = MBBI->getOperand(0);
    MachineOperand &StackAdjust = MBBI->getOperand(isMem ? X86::AddrNumOperands
                                                         : 1);
    assert(StackAdjust.isImm() && "Expecting immediate value.");

    // Adjust stack pointer.
    int StackAdj = StackAdjust.getImm();
    int MaxTCDelta = X86FI->getTCReturnAddrDelta();
    int Offset = 0;
    assert(MaxTCDelta <= 0 && "MaxTCDelta should never be positive");

    // Incoporate the retaddr area.
    Offset = StackAdj - MaxTCDelta;
    assert(Offset >= 0 && "Offset should never be negative");

    if (Opcode == X86::TCRETURNdicc || Opcode == X86::TCRETURNdi64cc) {
      assert(Offset == 0 && "Conditional tail call cannot adjust the stack.");
    }

    if (Offset) {
      // Check for possible merge with preceding ADD instruction.
      Offset += X86FL->mergeSPUpdates(MBB, MBBI, true);
      X86FL->emitSPUpdate(MBB, MBBI, DL, Offset, /*InEpilogue=*/true);
    }

    // Jump to label or value in register.
    bool IsWin64 = STI->isTargetWin64();
    if (Opcode == X86::TCRETURNdi || Opcode == X86::TCRETURNdicc ||
        Opcode == X86::TCRETURNdi64 || Opcode == X86::TCRETURNdi64cc) {
      unsigned Op;
      switch (Opcode) {
      case X86::TCRETURNdi:
        Op = X86::TAILJMPd;
        break;
      case X86::TCRETURNdicc:
        Op = X86::TAILJMPd_CC;
        break;
      case X86::TCRETURNdi64cc:
        assert(!MBB.getParent()->hasWinCFI() &&
               "Conditional tail calls confuse "
               "the Win64 unwinder.");
        Op = X86::TAILJMPd64_CC;
        break;
      default:
        // Note: Win64 uses REX prefixes indirect jumps out of functions, but
        // not direct ones.
        Op = X86::TAILJMPd64;
        break;
      }
      MachineInstrBuilder MIB = BuildMI(MBB, MBBI, DL, TII->get(Op));
      if (JumpTarget.isGlobal()) {
        MIB.addGlobalAddress(JumpTarget.getGlobal(), JumpTarget.getOffset(),
                             JumpTarget.getTargetFlags());
      } else {
        assert(JumpTarget.isSymbol());
        MIB.addExternalSymbol(JumpTarget.getSymbolName(),
                              JumpTarget.getTargetFlags());
      }
      if (Op == X86::TAILJMPd_CC || Op == X86::TAILJMPd64_CC) {
        MIB.addImm(MBBI->getOperand(2).getImm());
      }

    } else if (Opcode == X86::TCRETURNmi || Opcode == X86::TCRETURNmi64) {
      unsigned Op = (Opcode == X86::TCRETURNmi)
                        ? X86::TAILJMPm
                        : (IsWin64 ? X86::TAILJMPm64_REX : X86::TAILJMPm64);
      MachineInstrBuilder MIB = BuildMI(MBB, MBBI, DL, TII->get(Op));
      for (unsigned i = 0; i != X86::AddrNumOperands; ++i)
        MIB.add(MBBI->getOperand(i));
    } else if (Opcode == X86::TCRETURNri64) {
      JumpTarget.setIsKill();
      BuildMI(MBB, MBBI, DL,
              TII->get(IsWin64 ? X86::TAILJMPr64_REX : X86::TAILJMPr64))
          .add(JumpTarget);
    } else {
      JumpTarget.setIsKill();
      BuildMI(MBB, MBBI, DL, TII->get(X86::TAILJMPr))
          .add(JumpTarget);
    }

    MachineInstr &NewMI = *std::prev(MBBI);
    NewMI.copyImplicitOps(*MBBI->getParent()->getParent(), *MBBI);
    NewMI.setCFIType(*MBB.getParent(), MI.getCFIType());

    // Update the call site info.
    if (MBBI->isCandidateForCallSiteEntry())
      MBB.getParent()->moveCallSiteInfo(&*MBBI, &NewMI);

    // Delete the pseudo instruction TCRETURN.
    MBB.erase(MBBI);

    return true;
  }
  case X86::EH_RETURN:
  case X86::EH_RETURN64: {
    MachineOperand &DestAddr = MBBI->getOperand(0);
    assert(DestAddr.isReg() && "Offset should be in register!");
    const bool Uses64BitFramePtr =
        STI->isTarget64BitLP64() || STI->isTargetNaCl64();
    Register StackPtr = TRI->getStackRegister();
    BuildMI(MBB, MBBI, DL,
            TII->get(Uses64BitFramePtr ? X86::MOV64rr : X86::MOV32rr), StackPtr)
        .addReg(DestAddr.getReg());
    // The EH_RETURN pseudo is really removed during the MC Lowering.
    return true;
  }
  case X86::IRET: {
    // Adjust stack to erase error code
    int64_t StackAdj = MBBI->getOperand(0).getImm();
    X86FL->emitSPUpdate(MBB, MBBI, DL, StackAdj, true);
    // Replace pseudo with machine iret
    unsigned RetOp = STI->is64Bit() ? X86::IRET64 : X86::IRET32;
    // Use UIRET if UINTR is present (except for building kernel)
    if (STI->is64Bit() && STI->hasUINTR() &&
        MBB.getParent()->getTarget().getCodeModel() != CodeModel::Kernel)
      RetOp = X86::UIRET;
    BuildMI(MBB, MBBI, DL, TII->get(RetOp));
    MBB.erase(MBBI);
    return true;
  }
  case X86::RET: {
    // Adjust stack to erase error code
    int64_t StackAdj = MBBI->getOperand(0).getImm();
    MachineInstrBuilder MIB;
    if (StackAdj == 0) {
      MIB = BuildMI(MBB, MBBI, DL,
                    TII->get(STI->is64Bit() ? X86::RET64 : X86::RET32));
    } else if (isUInt<16>(StackAdj)) {
      MIB = BuildMI(MBB, MBBI, DL,
                    TII->get(STI->is64Bit() ? X86::RETI64 : X86::RETI32))
                .addImm(StackAdj);
    } else {
      assert(!STI->is64Bit() &&
             "shouldn't need to do this for x86_64 targets!");
      // A ret can only handle immediates as big as 2**16-1.  If we need to pop
      // off bytes before the return address, we must do it manually.
      BuildMI(MBB, MBBI, DL, TII->get(X86::POP32r)).addReg(X86::ECX, RegState::Define);
      X86FL->emitSPUpdate(MBB, MBBI, DL, StackAdj, /*InEpilogue=*/true);
      BuildMI(MBB, MBBI, DL, TII->get(X86::PUSH32r)).addReg(X86::ECX);
      MIB = BuildMI(MBB, MBBI, DL, TII->get(X86::RET32));
    }
    for (unsigned I = 1, E = MBBI->getNumOperands(); I != E; ++I)
      MIB.add(MBBI->getOperand(I));
    MBB.erase(MBBI);
    return true;
  }
  case X86::LCMPXCHG16B_SAVE_RBX: {
    // Perform the following transformation.
    // SaveRbx = pseudocmpxchg Addr, <4 opds for the address>, InArg, SaveRbx
    // =>
    // RBX = InArg
    // actualcmpxchg Addr
    // RBX = SaveRbx
    const MachineOperand &InArg = MBBI->getOperand(6);
    Register SaveRbx = MBBI->getOperand(7).getReg();

    // Copy the input argument of the pseudo into the argument of the
    // actual instruction.
    // NOTE: We don't copy the kill flag since the input might be the same reg
    // as one of the other operands of LCMPXCHG16B.
    TII->copyPhysReg(MBB, MBBI, DL, X86::RBX, InArg.getReg(), false);
    // Create the actual instruction.
    MachineInstr *NewInstr = BuildMI(MBB, MBBI, DL, TII->get(X86::LCMPXCHG16B));
    // Copy the operands related to the address.
    for (unsigned Idx = 1; Idx < 6; ++Idx)
      NewInstr->addOperand(MBBI->getOperand(Idx));
    // Finally, restore the value of RBX.
    TII->copyPhysReg(MBB, MBBI, DL, X86::RBX, SaveRbx,
                     /*SrcIsKill*/ true);

    // Delete the pseudo.
    MBBI->eraseFromParent();
    return true;
  }
  // Loading/storing mask pairs requires two kmov operations. The second one of
  // these needs a 2 byte displacement relative to the specified address (with
  // 32 bit spill size). The pairs of 1bit masks up to 16 bit masks all use the
  // same spill size, they all are stored using MASKPAIR16STORE, loaded using
  // MASKPAIR16LOAD.
  //
  // The displacement value might wrap around in theory, thus the asserts in
  // both cases.
  case X86::MASKPAIR16LOAD: {
    int64_t Disp = MBBI->getOperand(1 + X86::AddrDisp).getImm();
    assert(Disp >= 0 && Disp <= INT32_MAX - 2 && "Unexpected displacement");
    Register Reg = MBBI->getOperand(0).getReg();
    bool DstIsDead = MBBI->getOperand(0).isDead();
    Register Reg0 = TRI->getSubReg(Reg, X86::sub_mask_0);
    Register Reg1 = TRI->getSubReg(Reg, X86::sub_mask_1);

    auto MIBLo = BuildMI(MBB, MBBI, DL, TII->get(X86::KMOVWkm))
      .addReg(Reg0, RegState::Define | getDeadRegState(DstIsDead));
    auto MIBHi = BuildMI(MBB, MBBI, DL, TII->get(X86::KMOVWkm))
      .addReg(Reg1, RegState::Define | getDeadRegState(DstIsDead));

    for (int i = 0; i < X86::AddrNumOperands; ++i) {
      MIBLo.add(MBBI->getOperand(1 + i));
      if (i == X86::AddrDisp)
        MIBHi.addImm(Disp + 2);
      else
        MIBHi.add(MBBI->getOperand(1 + i));
    }

    // Split the memory operand, adjusting the offset and size for the halves.
    MachineMemOperand *OldMMO = MBBI->memoperands().front();
    MachineFunction *MF = MBB.getParent();
    MachineMemOperand *MMOLo = MF->getMachineMemOperand(OldMMO, 0, 2);
    MachineMemOperand *MMOHi = MF->getMachineMemOperand(OldMMO, 2, 2);

    MIBLo.setMemRefs(MMOLo);
    MIBHi.setMemRefs(MMOHi);

    // Delete the pseudo.
    MBB.erase(MBBI);
    return true;
  }
  case X86::MASKPAIR16STORE: {
    int64_t Disp = MBBI->getOperand(X86::AddrDisp).getImm();
    assert(Disp >= 0 && Disp <= INT32_MAX - 2 && "Unexpected displacement");
    Register Reg = MBBI->getOperand(X86::AddrNumOperands).getReg();
    bool SrcIsKill = MBBI->getOperand(X86::AddrNumOperands).isKill();
    Register Reg0 = TRI->getSubReg(Reg, X86::sub_mask_0);
    Register Reg1 = TRI->getSubReg(Reg, X86::sub_mask_1);

    auto MIBLo = BuildMI(MBB, MBBI, DL, TII->get(X86::KMOVWmk));
    auto MIBHi = BuildMI(MBB, MBBI, DL, TII->get(X86::KMOVWmk));

    for (int i = 0; i < X86::AddrNumOperands; ++i) {
      MIBLo.add(MBBI->getOperand(i));
      if (i == X86::AddrDisp)
        MIBHi.addImm(Disp + 2);
      else
        MIBHi.add(MBBI->getOperand(i));
    }
    MIBLo.addReg(Reg0, getKillRegState(SrcIsKill));
    MIBHi.addReg(Reg1, getKillRegState(SrcIsKill));

    // Split the memory operand, adjusting the offset and size for the halves.
    MachineMemOperand *OldMMO = MBBI->memoperands().front();
    MachineFunction *MF = MBB.getParent();
    MachineMemOperand *MMOLo = MF->getMachineMemOperand(OldMMO, 0, 2);
    MachineMemOperand *MMOHi = MF->getMachineMemOperand(OldMMO, 2, 2);

    MIBLo.setMemRefs(MMOLo);
    MIBHi.setMemRefs(MMOHi);

    // Delete the pseudo.
    MBB.erase(MBBI);
    return true;
  }
  case X86::MWAITX_SAVE_RBX: {
    // Perform the following transformation.
    // SaveRbx = pseudomwaitx InArg, SaveRbx
    // =>
    // [E|R]BX = InArg
    // actualmwaitx
    // [E|R]BX = SaveRbx
    const MachineOperand &InArg = MBBI->getOperand(1);
    // Copy the input argument of the pseudo into the argument of the
    // actual instruction.
    TII->copyPhysReg(MBB, MBBI, DL, X86::EBX, InArg.getReg(), InArg.isKill());
    // Create the actual instruction.
    BuildMI(MBB, MBBI, DL, TII->get(X86::MWAITXrrr));
    // Finally, restore the value of RBX.
    Register SaveRbx = MBBI->getOperand(2).getReg();
    TII->copyPhysReg(MBB, MBBI, DL, X86::RBX, SaveRbx, /*SrcIsKill*/ true);
    // Delete the pseudo.
    MBBI->eraseFromParent();
    return true;
  }
  case TargetOpcode::ICALL_BRANCH_FUNNEL:
    ExpandICallBranchFunnel(&MBB, MBBI);
    return true;
  case X86::PLDTILECFGV: {
    MI.setDesc(TII->get(X86::LDTILECFG));
    return true;
  }
  case X86::PTILELOADDV:
  case X86::PTILELOADDT1V: {
    for (unsigned i = 2; i > 0; --i)
      MI.removeOperand(i);
    unsigned Opc =
        Opcode == X86::PTILELOADDV ? X86::TILELOADD : X86::TILELOADDT1;
    MI.setDesc(TII->get(Opc));
    return true;
  }
  case X86::PTDPBSSDV:
  case X86::PTDPBSUDV:
  case X86::PTDPBUSDV:
  case X86::PTDPBUUDV:
  case X86::PTDPBF16PSV:
  case X86::PTDPFP16PSV: {
    MI.untieRegOperand(4);
    for (unsigned i = 3; i > 0; --i)
      MI.removeOperand(i);
    unsigned Opc;
    switch (Opcode) {
    case X86::PTDPBSSDV:   Opc = X86::TDPBSSD; break;
    case X86::PTDPBSUDV:   Opc = X86::TDPBSUD; break;
    case X86::PTDPBUSDV:   Opc = X86::TDPBUSD; break;
    case X86::PTDPBUUDV:   Opc = X86::TDPBUUD; break;
    case X86::PTDPBF16PSV: Opc = X86::TDPBF16PS; break;
    case X86::PTDPFP16PSV: Opc = X86::TDPFP16PS; break;
    default: llvm_unreachable("Impossible Opcode!");
    }
    MI.setDesc(TII->get(Opc));
    MI.tieOperands(0, 1);
    return true;
  }
  case X86::PTILESTOREDV: {
    for (int i = 1; i >= 0; --i)
      MI.removeOperand(i);
    MI.setDesc(TII->get(X86::TILESTORED));
    return true;
  }
  case X86::PTILEZEROV: {
    for (int i = 2; i > 0; --i) // Remove row, col
      MI.removeOperand(i);
    MI.setDesc(TII->get(X86::TILEZERO));
    return true;
  }
  case X86::CALL64pcrel32_RVMARKER:
  case X86::CALL64r_RVMARKER:
  case X86::CALL64m_RVMARKER:
    expandCALL_RVMARKER(MBB, MBBI);
    return true;
  }
  llvm_unreachable("Previous switch has a fallthrough?");
}

// This function creates additional block for storing varargs guarded
// registers. It adds check for %al into entry block, to skip
// GuardedRegsBlk if xmm registers should not be stored.
//
//     EntryBlk[VAStartPseudoInstr]     EntryBlk
//        |                              |     .
//        |                              |        .
//        |                              |   GuardedRegsBlk
//        |                      =>      |        .
//        |                              |     .
//        |                             TailBlk
//        |                              |
//        |                              |
//
void X86ExpandPseudo::ExpandVastartSaveXmmRegs(
    MachineBasicBlock *EntryBlk,
    MachineBasicBlock::iterator VAStartPseudoInstr) const {
  assert(VAStartPseudoInstr->getOpcode() == X86::VASTART_SAVE_XMM_REGS);

  MachineFunction *Func = EntryBlk->getParent();
  const TargetInstrInfo *TII = STI->getInstrInfo();
  const DebugLoc &DL = VAStartPseudoInstr->getDebugLoc();
  Register CountReg = VAStartPseudoInstr->getOperand(0).getReg();

  // Calculate liveins for newly created blocks.
  LivePhysRegs LiveRegs(*STI->getRegisterInfo());
  SmallVector<std::pair<MCPhysReg, const MachineOperand *>, 8> Clobbers;

  LiveRegs.addLiveIns(*EntryBlk);
  for (MachineInstr &MI : EntryBlk->instrs()) {
    if (MI.getOpcode() == VAStartPseudoInstr->getOpcode())
      break;

    LiveRegs.stepForward(MI, Clobbers);
  }

  // Create the new basic blocks. One block contains all the XMM stores,
  // and another block is the final destination regardless of whether any
  // stores were performed.
  const BasicBlock *LLVMBlk = EntryBlk->getBasicBlock();
  MachineFunction::iterator EntryBlkIter = ++EntryBlk->getIterator();
  MachineBasicBlock *GuardedRegsBlk = Func->CreateMachineBasicBlock(LLVMBlk);
  MachineBasicBlock *TailBlk = Func->CreateMachineBasicBlock(LLVMBlk);
  Func->insert(EntryBlkIter, GuardedRegsBlk);
  Func->insert(EntryBlkIter, TailBlk);

  // Transfer the remainder of EntryBlk and its successor edges to TailBlk.
  TailBlk->splice(TailBlk->begin(), EntryBlk,
                  std::next(MachineBasicBlock::iterator(VAStartPseudoInstr)),
                  EntryBlk->end());
  TailBlk->transferSuccessorsAndUpdatePHIs(EntryBlk);

  uint64_t FrameOffset = VAStartPseudoInstr->getOperand(4).getImm();
  uint64_t VarArgsRegsOffset = VAStartPseudoInstr->getOperand(6).getImm();

  // TODO: add support for YMM and ZMM here.
  unsigned MOVOpc = STI->hasAVX() ? X86::VMOVAPSmr : X86::MOVAPSmr;

  // In the XMM save block, save all the XMM argument registers.
  for (int64_t OpndIdx = 7, RegIdx = 0;
       OpndIdx < VAStartPseudoInstr->getNumOperands() - 1;
       OpndIdx++, RegIdx++) {
    auto NewMI = BuildMI(GuardedRegsBlk, DL, TII->get(MOVOpc));
    for (int i = 0; i < X86::AddrNumOperands; ++i) {
      if (i == X86::AddrDisp)
        NewMI.addImm(FrameOffset + VarArgsRegsOffset + RegIdx * 16);
      else
        NewMI.add(VAStartPseudoInstr->getOperand(i + 1));
    }
    NewMI.addReg(VAStartPseudoInstr->getOperand(OpndIdx).getReg());
    assert(VAStartPseudoInstr->getOperand(OpndIdx).getReg().isPhysical());
  }

  // The original block will now fall through to the GuardedRegsBlk.
  EntryBlk->addSuccessor(GuardedRegsBlk);
  // The GuardedRegsBlk will fall through to the TailBlk.
  GuardedRegsBlk->addSuccessor(TailBlk);

  if (!STI->isCallingConvWin64(Func->getFunction().getCallingConv())) {
    // If %al is 0, branch around the XMM save block.
    BuildMI(EntryBlk, DL, TII->get(X86::TEST8rr))
        .addReg(CountReg)
        .addReg(CountReg);
    BuildMI(EntryBlk, DL, TII->get(X86::JCC_1))
        .addMBB(TailBlk)
        .addImm(X86::COND_E);
    EntryBlk->addSuccessor(TailBlk);
  }

  // Add liveins to the created block.
  addLiveIns(*GuardedRegsBlk, LiveRegs);
  addLiveIns(*TailBlk, LiveRegs);

  // Delete the pseudo.
  VAStartPseudoInstr->eraseFromParent();
}

/// Expand all pseudo instructions contained in \p MBB.
/// \returns true if any expansion occurred for \p MBB.
bool X86ExpandPseudo::ExpandMBB(MachineBasicBlock &MBB) {
  bool Modified = false;

  // MBBI may be invalidated by the expansion.
  MachineBasicBlock::iterator MBBI = MBB.begin(), E = MBB.end();
  while (MBBI != E) {
    MachineBasicBlock::iterator NMBBI = std::next(MBBI);
    DebugLoc DL = MBBI->getDebugLoc();
    bool AddPostRaiseNop =
        isOxCamlCallingConv(MBB.getParent()->getFunction().getCallingConv()) &&
        isCamlRaiseExnLikeStatepoint(*MBBI);
    Modified |= ExpandMI(MBB, MBBI);
    if (AddPostRaiseNop) {
      BuildMI(MBB, NMBBI, DL, TII->get(X86::NOOP));
      Modified = true;
    }
    MBBI = NMBBI;
  }

  return Modified;
}

bool X86ExpandPseudo::ExpandPseudosWhichAffectControlFlow(MachineFunction &MF) {
  // Currently pseudo which affects control flow is only
  // X86::VASTART_SAVE_XMM_REGS which is located in Entry block.
  // So we do not need to evaluate other blocks.
  for (MachineInstr &Instr : MF.front().instrs()) {
    if (Instr.getOpcode() == X86::VASTART_SAVE_XMM_REGS) {
      ExpandVastartSaveXmmRegs(&(MF.front()), Instr);
      return true;
    }
  }

  return false;
}

bool X86ExpandPseudo::runOnMachineFunction(MachineFunction &MF) {
  STI = &MF.getSubtarget<X86Subtarget>();
  TII = STI->getInstrInfo();
  TRI = STI->getRegisterInfo();
  X86FI = MF.getInfo<X86MachineFunctionInfo>();
  X86FL = STI->getFrameLowering();

  bool Modified = ExpandPseudosWhichAffectControlFlow(MF);

  for (MachineBasicBlock &MBB : MF)
    Modified |= ExpandMBB(MBB);
  return Modified;
}

/// Returns an instance of the pseudo instruction expansion pass.
FunctionPass *llvm::createX86ExpandPseudoPass() {
  return new X86ExpandPseudo();
}
