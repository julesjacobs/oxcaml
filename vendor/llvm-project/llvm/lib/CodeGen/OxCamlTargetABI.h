//===- OxCamlTargetABI.h - OxCaml target ABI facts --------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_CODEGEN_OXCAMLTARGETABI_H
#define LLVM_LIB_CODEGEN_OXCAMLTARGETABI_H

#include "llvm/ADT/STLFunctionalExtras.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/TargetInstrInfo.h"
#include "llvm/CodeGen/TargetRegisterInfo.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/TargetParser/Triple.h"
#include <optional>

namespace llvm {
namespace oxcamlroots {

/// Target-specific OxCaml ABI facts needed by post-RA root handling.
/// Keep these facts centralized: the root analysis asks semantic questions
/// and unsupported targets fail closed.
class OxCamlTargetABI {
public:
  enum class Arch { Unsupported, AArch64, AMD64 };

private:
  Arch A = Arch::Unsupported;

  static bool aarch64RegNumber(StringRef RName, unsigned &N) {
    return RName.consume_front("X") && !RName.getAsInteger(10, N);
  }

public:
  OxCamlTargetABI() = default;
  explicit OxCamlTargetABI(Arch A) : A(A) {}

  static OxCamlTargetABI get(const MachineFunction &MF) {
    switch (MF.getTarget().getTargetTriple().getArch()) {
    case Triple::aarch64:
    case Triple::aarch64_be:
      return OxCamlTargetABI(Arch::AArch64);
    case Triple::x86_64:
      return OxCamlTargetABI(Arch::AMD64);
    default:
      return OxCamlTargetABI();
    }
  }

  bool isSupported() const { return A != Arch::Unsupported; }

  bool isAllocFamilyPreservedRegister(MCRegister R,
                                      const TargetRegisterInfo *TRI) const {
    StringRef RName = TRI->getName(R);
    switch (A) {
    case Arch::AArch64:
      return RName == "X0";
    case Arch::AMD64:
      return RName == "RAX";
    case Arch::Unsupported:
      return false;
    }
    llvm_unreachable("unhandled OxCaml target");
  }

  bool isAllocationCursorRegister(MCRegister R,
                                  const TargetRegisterInfo *TRI) const {
    StringRef RName = TRI->getName(R);
    switch (A) {
    case Arch::AArch64:
      return RName == "X27";
    case Arch::AMD64:
      return RName == "R15";
    case Arch::Unsupported:
      return false;
    }
    llvm_unreachable("unhandled OxCaml target");
  }

  std::optional<unsigned> resultGPROrdinal(MCRegister R,
                                           const TargetRegisterInfo *TRI) const {
    StringRef RName = TRI->getName(R);
    unsigned N;
    switch (A) {
    case Arch::AArch64:
      if (aarch64RegNumber(RName, N) && N <= 15)
        return N;
      return std::nullopt;
    case Arch::AMD64:
      return StringSwitch<std::optional<unsigned>>(RName)
          .Case("RAX", 0)
          .Case("RBX", 1)
          .Case("RDI", 2)
          .Case("RSI", 3)
          .Case("RDX", 4)
          .Case("RCX", 5)
          .Case("R8", 6)
          .Case("R9", 7)
          .Case("R12", 8)
          .Case("R13", 9)
          .Default(std::nullopt);
    case Arch::Unsupported:
      return std::nullopt;
    }
    llvm_unreachable("unhandled OxCaml target");
  }

  std::optional<unsigned> paramGPROrdinal(MCRegister R,
                                          const TargetRegisterInfo *TRI) const {
    StringRef RName = TRI->getName(R);
    unsigned N;
    switch (A) {
    case Arch::AArch64:
      if (RName == "X28")
        return 0;
      if (RName == "X27")
        return 1;
      if (aarch64RegNumber(RName, N) && N <= 15)
        return N + 2;
      return std::nullopt;
    case Arch::AMD64:
      return StringSwitch<std::optional<unsigned>>(RName)
          .Case("R14", 0)
          .Case("R15", 1)
          .Case("RAX", 2)
          .Case("RBX", 3)
          .Case("RDI", 4)
          .Case("RSI", 5)
          .Case("RDX", 6)
          .Case("RCX", 7)
          .Case("R8", 8)
          .Case("R9", 9)
          .Case("R12", 10)
          .Case("R13", 11)
          .Default(std::nullopt);
    case Arch::Unsupported:
      return std::nullopt;
    }
    llvm_unreachable("unhandled OxCaml target");
  }

  bool isOrdinaryGPR(MCRegister R, const TargetRegisterInfo *TRI) const {
    return resultGPROrdinal(R, TRI).has_value();
  }

  bool isAllocationCursorDecrement(const MachineInstr &MI,
                                   const TargetInstrInfo *TII,
                                   Register &Base) const {
    switch (A) {
    case Arch::AArch64:
      if (StringRef(TII->getName(MI.getOpcode())) != "SUBXri" ||
          MI.getNumExplicitOperands() < 4 || !MI.getOperand(1).isReg() ||
          !MI.getOperand(2).isImm() || !MI.getOperand(3).isImm() ||
          MI.getOperand(3).getImm() != 0)
        return false;
      Base = MI.getOperand(1).getReg();
      return true;
    case Arch::AMD64: {
      if (MI.getNumExplicitOperands() < 3 || !MI.getOperand(1).isReg() ||
          !MI.getOperand(2).isImm())
        return false;
      StringRef OpName = TII->getName(MI.getOpcode());
      int64_t Imm = MI.getOperand(2).getImm();
      if (!((OpName.starts_with("ADD64ri") && Imm <= 0) ||
            (OpName.starts_with("SUB64ri") && Imm >= 0)))
        return false;
      Base = MI.getOperand(1).getReg();
      return true;
    }
    case Arch::Unsupported:
      return false;
    }
    llvm_unreachable("unhandled OxCaml target");
  }

  bool isAllocationResultValue(const MachineInstr &MI, Register R,
                               const TargetInstrInfo *TII,
                               function_ref<bool(Register)> IsAllocCursor) const {
    switch (A) {
    case Arch::AArch64:
      if (MI.mayStore() && MI.hasOneMemOperand() &&
          MI.memoperands().front()->getAddrSpace() == 1 &&
          MI.getNumExplicitDefs() == 1 && MI.getOperand(0).isReg() &&
          MI.getOperand(0).getReg() == R &&
          MI.getNumExplicitOperands() >= 4 && MI.getOperand(2).isReg() &&
          MI.getOperand(2).getReg() == R && MI.getOperand(3).isImm() &&
          MI.getOperand(3).getImm() == 8)
        return true;
      if (StringRef(TII->getName(MI.getOpcode())) == "ADDXri" &&
          MI.getNumExplicitOperands() >= 4 && MI.getOperand(0).isReg() &&
          MI.getOperand(0).getReg() == R && MI.getOperand(1).isReg() &&
          MI.getOperand(2).isImm() && MI.getOperand(2).getImm() == 8 &&
          MI.getOperand(3).isImm() && MI.getOperand(3).getImm() == 0 &&
          IsAllocCursor(MI.getOperand(1).getReg()))
        return true;
      return false;
    case Arch::AMD64:
      return StringRef(TII->getName(MI.getOpcode())) == "LEA64r" &&
             MI.getNumExplicitOperands() >= 5 && MI.getOperand(0).isReg() &&
             MI.getOperand(0).getReg() == R && MI.getOperand(1).isReg() &&
             MI.getOperand(2).isImm() && MI.getOperand(2).getImm() == 1 &&
             MI.getOperand(3).isReg() &&
             !MI.getOperand(3).getReg().isValid() &&
             MI.getOperand(4).isImm() && MI.getOperand(4).getImm() == 8 &&
             IsAllocCursor(MI.getOperand(1).getReg());
    case Arch::Unsupported:
      return false;
    }
    llvm_unreachable("unhandled OxCaml target");
  }
};

inline bool isAllocFamilyMask(const OxCamlTargetABI &ABI,
                              const uint32_t *RegMask,
                              const TargetRegisterInfo *TRI) {
  if (!RegMask || !ABI.isSupported())
    return false;
  for (unsigned R = 1, E = TRI->getNumRegs(); R != E; ++R)
    if (ABI.isAllocFamilyPreservedRegister(MCRegister::from(R), TRI))
      return !MachineOperand::clobbersPhysReg(RegMask, R);
  return false;
}

} // end namespace oxcamlroots
} // end namespace llvm

#endif // LLVM_LIB_CODEGEN_OXCAMLTARGETABI_H
