//===- OxCamlGCPrinter.cpp - OxCaml frametable emitter --------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements printing the assembly code for an OxCaml frametable.
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/Twine.h"
#include "llvm/ADT/Triple.h"
#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/CodeGen/GCMetadata.h"
#include "llvm/CodeGen/GCMetadataPrinter.h"
#include "llvm/CodeGen/StackMaps.h"
#include "llvm/IR/BuiltinGCs.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Mangler.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Statepoint.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCDirectives.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Target/TargetLoweringObjectFile.h"
#include <array>
#include <cctype>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <string>
#include <vector>

using namespace llvm;

namespace {

class OxCamlGCMetadataPrinter : public GCMetadataPrinter {
public:
  void beginAssembly(Module &M, GCModuleInfo &Info, AsmPrinter &AP) override;
  void finishAssembly(Module &M, GCModuleInfo &Info, AsmPrinter &AP) override;
  bool emitStackMaps(Module &M, StackMaps &SM, AsmPrinter &AP) override;
};

} // end anonymous namespace

static GCMetadataPrinterRegistry::Add<OxCamlGCMetadataPrinter>
    Y("oxcaml", "OxCaml frametable printer");

void llvm::linkOxCamlGCPrinter() {}

static std::string camlGlobalSymName(const Module &M, const char *Id) {
  if (Metadata *ModuleMD = M.getModuleFlag(StringRef("oxcaml_module"))) {
    if (MDString *Str = dyn_cast<MDString>(ModuleMD)) {
      StringRef ModuleName = Str->getString();
      
      std::string SymName;
      SymName += "caml";
      SymName += ModuleName;
      SymName += "__";
      SymName += Id;
    
      return SymName;
    }
  }

  report_fatal_error("[OxCamlGCPrinter] module name not provided");
}

static void emitCamlGlobal(const Module &M, MCStreamer &OS, const char *Id) {
  std::string SymName = camlGlobalSymName(M, Id);

  SmallString<128> TmpStr;
  Mangler::getNameWithPrefix(TmpStr, SymName, M.getDataLayout());

  MCSymbol *Sym = OS.getContext().getOrCreateSymbol(TmpStr);

  OS.emitSymbolAttribute(Sym, MCSA_Global);
  OS.emitLabel(Sym);
}

void OxCamlGCMetadataPrinter::beginAssembly(Module &M, GCModuleInfo &Info,
                                           AsmPrinter &AP) {
  AP.OutStreamer->switchSection(AP.getObjFileLowering().getTextSection());
  emitCamlGlobal(M, *(AP.OutStreamer), "code_begin");

  AP.OutStreamer->switchSection(AP.getObjFileLowering().getDataSection());
  emitCamlGlobal(M, *(AP.OutStreamer), "data_begin");
}


void OxCamlGCMetadataPrinter::finishAssembly(Module &M, GCModuleInfo &Info,
                                         AsmPrinter &AP) {
  AP.OutStreamer->switchSection(AP.getObjFileLowering().getTextSection());
  emitCamlGlobal(M, *(AP.OutStreamer), "code_end");

  AP.OutStreamer->switchSection(AP.getObjFileLowering().getDataSection());
  emitCamlGlobal(M, *(AP.OutStreamer), "data_end");
}

/// Map LLVM DWARF register numbers to OxCaml register map.
/// * See llvm/lib/Target/X86/X86RegisterInfo.td and
///   llvm/lib/Target/AArch64/AArch64RegisterInfo.td for DWARF register numbers.
/// * See backend/amd64/proc.ml and backend/arm64/proc.ml for the OxCaml
///   register maps.

// TODO: This is target-specific and should probably live in a
// target-specific location.

// Directly taken from [Reg_class.gpr_dwarf_reg_numbers]:
// https://github.com/oxcaml/oxcaml/blob/main/backend/amd64/reg_class.ml#L26
// Note that R14 and R15 are added for completeness
static constexpr std::array<unsigned, 16> GPR_OxCamlToDwarf =
  { 0, 3, 5, 4, 1, 2, 8, 9, 12, 13, 10, 11, 6, 14, 15 };

static constexpr auto GPR_DwarfToOxCaml = []() {
  std::array<unsigned, 16> result{};
  for (size_t ocaml_idx = 0; ocaml_idx < GPR_OxCamlToDwarf.size(); ++ocaml_idx) {
    unsigned dwarf_reg = GPR_OxCamlToDwarf[ocaml_idx];
    if (dwarf_reg < result.size()) {
      result[dwarf_reg] = ocaml_idx;
    }
  }
  return result;
}();

static const unsigned XMMBeginOxCaml = 100;
static const unsigned XMMBeginDwarf = 17;
static const unsigned XMMEndDwarf = 32;

static unsigned mapX86DwarfRegToOxCamlIndex(unsigned DwarfRegNum) {
  if (DwarfRegNum < GPR_DwarfToOxCaml.size()) {
    return GPR_DwarfToOxCaml[DwarfRegNum];
  } else if (XMMBeginDwarf <= DwarfRegNum && DwarfRegNum <= XMMEndDwarf) {
    return DwarfRegNum - XMMBeginDwarf + XMMBeginOxCaml;
  } else {
    report_fatal_error("[OxCamlGCPrinter] unrecognised DWARF register: "
      + Twine(DwarfRegNum));
  }
}

static unsigned mapAArch64DwarfRegToOxCamlIndex(unsigned DwarfRegNum) {
  // backend/arm64/regs.ml uses the following integer register order:
  // x0-x15, x19-x28, x16-x17.
  if (DwarfRegNum <= 15) {
    return DwarfRegNum;
  } else if (19 <= DwarfRegNum && DwarfRegNum <= 28) {
    return DwarfRegNum - 3;
  } else if (DwarfRegNum == 16 || DwarfRegNum == 17) {
    return DwarfRegNum + 10;
  } else {
    report_fatal_error("[OxCamlGCPrinter] unrecognised AArch64 DWARF register: "
      + Twine(DwarfRegNum));
  }
}

static unsigned mapLLVMDwarfRegToOxCamlIndex(const Module &M,
                                             unsigned DwarfRegNum) {
  Triple TheTriple(M.getTargetTriple());
  switch (TheTriple.getArch()) {
  case Triple::x86_64:
    return mapX86DwarfRegToOxCamlIndex(DwarfRegNum);
  case Triple::aarch64:
  case Triple::aarch64_32:
    return mapAArch64DwarfRegToOxCamlIndex(DwarfRegNum);
  default:
    report_fatal_error("[OxCamlGCPrinter] unsupported target: "
      + Twine(M.getTargetTriple()));
  }
}

// note that although `StackMaps` keeps `ID` as a 64-bit integer, anything
// above 32 bits gets truncated, so we can't use them.

static uint64_t stackOffsetOfID(uint64_t ID) {
  return ID & ((1ull << 16) - 1) & ~(1ull);
}

static uint64_t allocSizeOfID(uint64_t ID) {
  return ID >> 16;
}

static bool IDHasAlloc(uint64_t ID) {
  return ID & 1ull;
}

// Every 8-bit entry emitted in the frametable is offset by 2 (since that is the
// min allocation size). So, every slot can represent allocations of size [2, 257]
static uint8_t encodeAllocSize(uint64_t AllocSize) {
  if (AllocSize < 2 || AllocSize > 257)
    report_fatal_error("[OxCamlGCPrinter] invalid allocation size: "
      + Twine(AllocSize));
  return AllocSize - 2;
}

static const int AllocMask = 2;
static const int DebugMask = 1;
static const int FrameSizeReservedMask = 3; // Debug + Alloc
static const int64_t OxCamlDebugDeoptMarker = 0x6f786364;
static const int64_t OxCamlDebugDeoptVersion = 1;
static const int64_t OxCamlAllocDeoptMarker = 0x6f786361;
static const int64_t OxCamlAllocDeoptVersion = 1;

struct OxCamlDebugItem {
  uint32_t Line = 0;
  uint32_t EndLineDelta = 0;
  uint32_t StartChr = 0;
  uint32_t EndChr = 0;
  uint32_t CharEndOffset = 0;
  uint32_t EndOffset = 0;
  std::string FunctionName;
  std::string FileName;
};

struct OxCamlDebugInfo {
  bool Valid = false;
  bool PrimitiveCall = false;
  bool RaiseCall = false;
  std::vector<OxCamlDebugItem> Items;
};

struct OxCamlAllocInfoItem {
  uint64_t AllocWords = 0;
  OxCamlDebugInfo DebugInfo;
};

struct OxCamlAllocInfo {
  bool Valid = false;
  std::vector<OxCamlAllocInfoItem> Items;
};

struct PendingDebugItem {
  MCSymbol *NameLabel;
  MCSymbol *FileLabel;
  OxCamlDebugItem Info;
};

struct PendingDebugInfo {
  MCSymbol *DebugLabel;
  OxCamlDebugInfo Info;
};

static uint32_t clampDebugField(int64_t Value, uint32_t Max) {
  if (Value < 0)
    return 0;
  if (Value > Max)
    return Max;
  return static_cast<uint32_t>(Value);
}

static uint32_t clampDebugLine(int64_t Value) {
  if (Value < 1)
    return 1;
  if (Value > 0x7ffff)
    return 0x7ffff;
  return static_cast<uint32_t>(Value);
}

static bool readConstant(const StackMaps::CallsiteInfo &CSI, size_t &Index,
                         int64_t &Value) {
  if (Index >= CSI.Locations.size())
    return false;
  const auto &Loc = CSI.Locations[Index++];
  if (Loc.Type != StackMaps::Location::Constant)
    return false;
  Value = Loc.Offset;
  return true;
}

static bool readDebugString(const StackMaps::CallsiteInfo &CSI, size_t &Index,
                            std::string &String) {
  int64_t LenValue = 0;
  if (!readConstant(CSI, Index, LenValue) || LenValue < 0)
    return false;
  size_t Len = static_cast<size_t>(LenValue);
  size_t NumChunks = (Len + 2) / 3;
  String.clear();
  String.reserve(Len);

  for (size_t ChunkIndex = 0; ChunkIndex < NumChunks; ++ChunkIndex) {
    int64_t Chunk = 0;
    if (!readConstant(CSI, Index, Chunk) || Chunk < 0)
      return false;
    for (int Byte = 0; Byte < 3 && String.size() < Len; ++Byte)
      String.push_back(static_cast<char>((Chunk >> (8 * Byte)) & 0xff));
  }

  return true;
}

static bool readDebugItem(const StackMaps::CallsiteInfo &CSI, size_t &Index,
                          OxCamlDebugItem &Item) {
  int64_t Line = 0;
  int64_t EndLineDelta = 0;
  int64_t StartChr = 0;
  int64_t EndChr = 0;
  int64_t CharEndOffset = 0;
  int64_t EndOffset = 0;
  if (!readConstant(CSI, Index, Line) ||
      !readConstant(CSI, Index, EndLineDelta) ||
      !readConstant(CSI, Index, StartChr) ||
      !readConstant(CSI, Index, EndChr) ||
      !readConstant(CSI, Index, CharEndOffset) ||
      !readConstant(CSI, Index, EndOffset))
    return false;

  Item.Line = clampDebugLine(Line);
  Item.EndLineDelta = clampDebugField(EndLineDelta, 0x3ffff);
  Item.StartChr = clampDebugField(StartChr, 0xffff);
  Item.EndChr = clampDebugField(EndChr, 0xffff);
  Item.CharEndOffset = clampDebugField(CharEndOffset, 0x3fffffff);
  Item.EndOffset = clampDebugField(EndOffset, 0x3fffffff);
  return readDebugString(CSI, Index, Item.FileName) &&
         readDebugString(CSI, Index, Item.FunctionName);
}

static OxCamlDebugInfo debugInfoForCallsite(
    const StackMaps::CallsiteInfo &CSI) {
  for (size_t I = 0; I < CSI.Locations.size(); ++I) {
    const auto &Marker = CSI.Locations[I];
    if (Marker.Type != StackMaps::Location::Constant ||
        Marker.Offset != OxCamlDebugDeoptMarker) {
      continue;
    }

    size_t Index = I + 1;
    int64_t Version = 0;
    int64_t Kind = 0;
    int64_t NumItems = 0;
    if (!readConstant(CSI, Index, Version) ||
        Version != OxCamlDebugDeoptVersion ||
        !readConstant(CSI, Index, Kind) ||
        !readConstant(CSI, Index, NumItems) || NumItems <= 0) {
      continue;
    }

    OxCamlDebugInfo Info;
    Info.Valid = true;
    Info.PrimitiveCall = Kind == 1;
    Info.RaiseCall = Kind == 2;

    for (int64_t ItemIndex = 0; ItemIndex < NumItems; ++ItemIndex) {
      OxCamlDebugItem Item;
      if (!readDebugItem(CSI, Index, Item))
        return OxCamlDebugInfo();
      Info.Items.push_back(std::move(Item));
    }

    if (!Info.Items.empty())
      return Info;
  }

  return OxCamlDebugInfo();
}

static OxCamlAllocInfo allocInfoForCallsite(
    const StackMaps::CallsiteInfo &CSI) {
  for (size_t I = 0; I < CSI.Locations.size(); ++I) {
    const auto &Marker = CSI.Locations[I];
    if (Marker.Type != StackMaps::Location::Constant ||
        Marker.Offset != OxCamlAllocDeoptMarker) {
      continue;
    }

    size_t Index = I + 1;
    int64_t Version = 0;
    int64_t NumAllocs = 0;
    if (!readConstant(CSI, Index, Version) ||
        Version != OxCamlAllocDeoptVersion ||
        !readConstant(CSI, Index, NumAllocs) || NumAllocs < 0) {
      continue;
    }

    OxCamlAllocInfo Info;
    Info.Valid = true;

    for (int64_t AllocIndex = 0; AllocIndex < NumAllocs; ++AllocIndex) {
      int64_t AllocWords = 0;
      int64_t NumDebugItems = 0;
      if (!readConstant(CSI, Index, AllocWords) || AllocWords < 0 ||
          !readConstant(CSI, Index, NumDebugItems) || NumDebugItems < 0)
        return OxCamlAllocInfo();

      OxCamlAllocInfoItem Item;
      Item.AllocWords = static_cast<uint64_t>(AllocWords);
      Item.DebugInfo.Valid = NumDebugItems > 0;

      for (int64_t DebugIndex = 0; DebugIndex < NumDebugItems; ++DebugIndex) {
        OxCamlDebugItem DebugItem;
        if (!readDebugItem(CSI, Index, DebugItem))
          return OxCamlAllocInfo();
        Item.DebugInfo.Items.push_back(std::move(DebugItem));
      }

      Info.Items.push_back(std::move(Item));
    }

    return Info;
  }

  return OxCamlAllocInfo();
}

static std::vector<uint8_t> encodedAllocSizes(uint64_t AllocSize) {
  std::vector<uint8_t> Sizes;

  if (AllocSize == 0)
    return Sizes;
  if (AllocSize < 2) {
    report_fatal_error("[OxCamlGCPrinter] alloc size must at least be two!");
  }

  int MaxAllocSize = 257;

  if (AllocSize % MaxAllocSize == 0) {
    size_t NumAlloc = AllocSize / MaxAllocSize;
    Sizes.assign(NumAlloc, encodeAllocSize(MaxAllocSize));
  } else if (AllocSize % MaxAllocSize == 1) {
    size_t NumMaxAlloc = AllocSize / MaxAllocSize - 1;
    Sizes.assign(NumMaxAlloc, encodeAllocSize(MaxAllocSize));
    Sizes.push_back(encodeAllocSize(MaxAllocSize - 1));
    Sizes.push_back(encodeAllocSize(2));
  } else {
    size_t NumMaxAlloc = AllocSize / MaxAllocSize;
    Sizes.assign(NumMaxAlloc, encodeAllocSize(MaxAllocSize));
    Sizes.push_back(encodeAllocSize(AllocSize % MaxAllocSize));
  }

  return Sizes;
}

static bool isAArch64Target(const Module &M) {
  Triple TheTriple(M.getTargetTriple());
  return TheTriple.getArch() == Triple::aarch64 ||
         TheTriple.getArch() == Triple::aarch64_32;
}

static int64_t stackOffset(uint64_t FrameSize, unsigned PtrSize,
                           int64_t Offset) {
  // BP-relative addressing -> SP
  if (Offset < 0) {
    int64_t TempFrameSize =
      FrameSize - PtrSize /* return address */ - PtrSize /* pushed BP */;
    Offset += TempFrameSize;
  }

  return Offset;
}

static void checkShortStackOffset(int64_t Offset) {
  if (Offset < -(1 << 15) || Offset >= (1 << 15)) {
    report_fatal_error("[OxCamlGCPrinter] stack offset too large: "
      + Twine(Offset));
  }
}

static void emitDebugOffset(MCStreamer &OS, const MCSymbol *DebugLabel) {
  MCSymbol *Here = OS.getContext().createTempSymbol();
  OS.emitValueToAlignment(Align(4));
  OS.emitLabel(Here);
  const MCExpr *Offset = MCBinaryExpr::createSub(
      MCSymbolRefExpr::create(DebugLabel, OS.getContext()),
      MCSymbolRefExpr::create(Here, OS.getContext()), OS.getContext());
  OS.emitValue(Offset, 4);
}

static void emitStringz(MCStreamer &OS, StringRef String) {
  OS.emitBytes(String);
  OS.emitInt8(0);
}

static bool isFullyPackable(const OxCamlDebugItem &Info) {
  return Info.Line <= 0xfff && Info.EndLineDelta <= 0x7 &&
         Info.StartChr <= 0x3f && Info.EndChr <= 0x7f &&
         Info.CharEndOffset <= 0x1ff;
}

static uint64_t packDebugInfo(const OxCamlDebugItem &Info, bool IsRaise,
                              bool HasNext) {
  if (isFullyPackable(Info)) {
    return (uint64_t(Info.Line) << 51) |
           (uint64_t(Info.EndLineDelta) << 48) |
           (uint64_t(Info.StartChr) << 42) |
           (uint64_t(Info.EndChr) << 35) |
           (uint64_t(Info.CharEndOffset) << 26) |
           (uint64_t(IsRaise ? 1 : 0) << 1) |
           uint64_t(HasNext ? 1 : 0);
  }

  return (uint64_t(1) << 63) |
         (uint64_t(std::min<uint32_t>(Info.Line, 0x7ffff)) << 44) |
         (uint64_t(std::min<uint32_t>(Info.EndLineDelta, 0x3ffff)) << 26) |
         (uint64_t(IsRaise ? 1 : 0) << 1) | uint64_t(HasNext ? 1 : 0);
}

static void emitDebugInfoRecord(MCStreamer &OS, const MCSymbol *NameLabel,
                                const OxCamlDebugItem &Info, bool IsRaise,
                                bool HasNext) {
  uint64_t PackedInfo = packDebugInfo(Info, IsRaise, HasNext);
  uint32_t InfoLow = static_cast<uint32_t>(PackedInfo);
  uint32_t InfoHigh = static_cast<uint32_t>(PackedInfo >> 32);

  MCSymbol *Here = OS.getContext().createTempSymbol();
  OS.emitLabel(Here);
  const MCExpr *NameOffset = MCBinaryExpr::createSub(
      MCSymbolRefExpr::create(NameLabel, OS.getContext()),
      MCSymbolRefExpr::create(Here, OS.getContext()), OS.getContext());
  const MCExpr *FirstWord = MCBinaryExpr::createAdd(
      NameOffset, MCConstantExpr::create(InfoLow, OS.getContext()),
      OS.getContext());
  OS.emitValue(FirstWord, 4);
  OS.emitInt32(InfoHigh);
}

static void emitNameAndLocInfo(MCStreamer &OS, const PendingDebugItem &Item) {
  OS.emitValueToAlignment(Align(4));
  OS.emitLabel(Item.NameLabel);
  const MCExpr *FileOffset = MCBinaryExpr::createSub(
      MCSymbolRefExpr::create(Item.FileLabel, OS.getContext()),
      MCSymbolRefExpr::create(Item.NameLabel, OS.getContext()),
      OS.getContext());
  OS.emitValue(FileOffset, 4);
  if (!isFullyPackable(Item.Info)) {
    OS.emitInt16(static_cast<uint16_t>(
        std::min<uint32_t>(Item.Info.StartChr, 0xffff)));
    OS.emitInt16(
        static_cast<uint16_t>(std::min<uint32_t>(Item.Info.EndChr, 0xffff)));
    OS.emitInt32(static_cast<int32_t>(
        std::min<uint32_t>(Item.Info.EndOffset, 0x3fffffff)));
  }
  emitStringz(OS, Item.Info.FunctionName);

  OS.emitLabel(Item.FileLabel);
  emitStringz(OS, Item.Info.FileName);
}

static void emitPendingDebugInfo(MCStreamer &OS,
                                 const PendingDebugInfo &DebugInfo) {
  const OxCamlDebugInfo &Info = DebugInfo.Info;
  std::vector<PendingDebugItem> Items;
  for (const OxCamlDebugItem &Item : Info.Items) {
    Items.push_back({OS.getContext().createTempSymbol(),
                     OS.getContext().createTempSymbol(), Item});
  }

  OS.emitValueToAlignment(Align(4));
  OS.emitLabel(DebugInfo.DebugLabel);
  if (Info.RaiseCall) {
    for (size_t I = 0; I < Items.size(); ++I)
      emitDebugInfoRecord(OS, Items[I].NameLabel, Items[I].Info, I == 0,
                          I + 1 < Items.size());
  } else {
    for (size_t I = 0; I < Items.size(); ++I)
      emitDebugInfoRecord(OS, Items[I].NameLabel, Items[I].Info, false,
                          I + 1 < Items.size());
  }

  for (const PendingDebugItem &Item : Items)
    emitNameAndLocInfo(OS, Item);
}

bool OxCamlGCMetadataPrinter::emitStackMaps(Module &M, StackMaps &SM, AsmPrinter &AP) {
  MCStreamer &OS = *AP.OutStreamer;
  unsigned PtrSize = M.getDataLayout().getPointerSize(); // Can only be 8 for now
  std::vector<PendingDebugInfo> PendingDebugInfos;
  
  OS.switchSection(AP.getObjFileLowering().getDataSection());
  
  emitCamlGlobal(M, OS, "frametable");

  // Number of records
  OS.emitInt64(SM.getCSInfos().size());

  for (const auto &CSI : SM.getCSInfos()) {
    // From runtime/frame_descriptors.h:
    // https://github.com/oxcaml/oxcaml/blob/main/runtime/caml/frame_descriptors.h#L63
    //
    // typedef struct {
    //   int32_t retaddr_rel; /* offset of return address from &retaddr_rel */
    //   uint16_t frame_data; /* frame size and various flags */
    //   uint16_t num_live;
    //   uint16_t live_ofs[num_live];
    // } frame_descr;

    // retaddr_rel
    MCSymbol *Here = OS.getContext().createTempSymbol();
    OS.emitLabel(Here);
    const MCExpr *RelativeAddr = MCBinaryExpr::createSub(
        MCSymbolRefExpr::create(CSI.CSLabel, OS.getContext()),
        MCSymbolRefExpr::create(Here, OS.getContext()),
        OS.getContext());
    OS.emitValue(RelativeAddr, 4);

    // frame_data
    uint64_t FrameSize = CSI.CSFunctionInfo.StaticStackSize;
    bool IsAArch64 = isAArch64Target(M);
    if (!IsAArch64)
      FrameSize += PtrSize; // Return address

    // The LLVM IR emitted from OxCaml will always set the statepoint ID for
    // calls to be wrapped in a statepoint. Also, note that DefaultStatepointID
    // (= 0xABCDEF00 as of now) does not clash with the encoding we use since
    // anything that sets the upper 16 bits will also set the bottom bit.
    if (CSI.ID != StatepointDirectives::DefaultStatepointID) {
      // Stack offset from OxCaml for active trap blocks and explicit stack
      // adjustments. LLVM does not model this as part of the static frame size
      // consistently across call sites, so apply the OxCaml offset directly.
      uint64_t StackOffset = stackOffsetOfID(CSI.ID);
      bool HasDynamicFrameSize =
          CSI.CSFunctionInfo.StackSize == std::numeric_limits<uint64_t>::max();
      if (!IsAArch64 || HasDynamicFrameSize)
        FrameSize += StackOffset;

      if (FrameSize & FrameSizeReservedMask) {
        report_fatal_error("[OxCamlGCPrinter] frame size has bottom bits set: "
          + Twine(FrameSize));
      }
      
    }

    OxCamlDebugInfo DebugInfo = debugInfoForCallsite(CSI);
    OxCamlAllocInfo AllocInfo = allocInfoForCallsite(CSI);
    bool HasAlloc = CSI.ID != StatepointDirectives::DefaultStatepointID &&
                    IDHasAlloc(CSI.ID);
    uint64_t AllocSize = HasAlloc ? allocSizeOfID(CSI.ID) : 0;
    std::vector<uint8_t> AllocSizes;
    if (HasAlloc && AllocInfo.Valid) {
      for (const OxCamlAllocInfoItem &Item : AllocInfo.Items)
        AllocSizes.push_back(encodeAllocSize(Item.AllocWords));
    } else if (HasAlloc) {
      AllocSizes = encodedAllocSizes(AllocSize);
    }
    bool HasAllocDebug = false;
    if (HasAlloc && AllocInfo.Valid)
      for (const OxCamlAllocInfoItem &Item : AllocInfo.Items)
        HasAllocDebug |= Item.DebugInfo.Valid;
    bool HasDebug = HasAlloc ? HasAllocDebug : DebugInfo.Valid;

    MCSymbol *DebugLabel = nullptr;
    std::vector<MCSymbol *> AllocDebugLabels;
    if (HasAlloc && HasAllocDebug) {
      for (OxCamlAllocInfoItem &Item : AllocInfo.Items) {
        if (Item.DebugInfo.Valid) {
          MCSymbol *AllocDebugLabel = OS.getContext().createTempSymbol();
          AllocDebugLabels.push_back(AllocDebugLabel);
          PendingDebugInfos.push_back(
              {AllocDebugLabel, std::move(Item.DebugInfo)});
        } else {
          AllocDebugLabels.push_back(nullptr);
        }
      }
    } else if (HasDebug) {
      DebugLabel = OS.getContext().createTempSymbol();
      PendingDebugInfos.push_back({DebugLabel, std::move(DebugInfo)});
    }

    uint64_t FrameData = FrameSize;

    if (HasAlloc) {
      FrameData |= AllocMask;
    }
    if (HasDebug) {
      FrameData |= DebugMask;
    }

    // num_live
    const auto &RootLocations =
        CSI.HasGCLocations ? CSI.GCLocations : CSI.Locations;
    const auto &CSRRootMap = CSI.CSFunctionInfo.CSRRootMap;

    std::vector<int64_t> LiveOffsets;
    for (const auto &Loc : RootLocations) {
      if (Loc.Type == StackMaps::Location::Register) {
        // Register indices are tagged (2n+1) and follow the OxCaml register
        // map (see `mapLLVMDwarfRegToOxCamlIndex`)
        unsigned DwarfRegNum = Loc.Reg;
        unsigned OxCamlIndex = mapLLVMDwarfRegToOxCamlIndex(M, DwarfRegNum);
        LiveOffsets.push_back((OxCamlIndex << 1) + 1);
      } else if (Loc.Type == StackMaps::Location::Direct ||
                 Loc.Type == StackMaps::Location::Indirect) {
        LiveOffsets.push_back(stackOffset(FrameSize, PtrSize, Loc.Offset));
      } else {
        // TODO: Do we need anything else here?
      }
    }

    bool HasCSRMap = !CSRRootMap.empty();

    static const uint64_t LongFrameMarker = 0x7fff;
    bool IsLongFrame =
        FrameData >= LongFrameMarker || LiveOffsets.size() >= LongFrameMarker;
    for (int64_t Offset : LiveOffsets) {
      if (Offset < 0 || Offset >= static_cast<int64_t>(LongFrameMarker)) {
        IsLongFrame = true;
        break;
      }
    }

    if (IsLongFrame && HasCSRMap)
      report_fatal_error("[OxCamlGCPrinter] CSR root maps with long frames "
                         "are not supported");

    if (IsLongFrame) {
      if (FrameData >= (1ULL << 32))
        report_fatal_error("[OxCamlGCPrinter] frame size too large: "
          + Twine(FrameData));
      if (LiveOffsets.size() >= (1ULL << 32))
        report_fatal_error("[OxCamlGCPrinter] live count too large: "
          + Twine(LiveOffsets.size()));

      OS.emitInt16(LongFrameMarker);
      OS.emitValueToAlignment(Align(4));
      OS.emitInt32(static_cast<uint32_t>(FrameData));
      OS.emitInt32(static_cast<uint32_t>(LiveOffsets.size()));
      for (int64_t Offset : LiveOffsets) {
        if (Offset < 0 || Offset >= (1LL << 32))
          report_fatal_error("[OxCamlGCPrinter] live offset too large: "
            + Twine(Offset));
        OS.emitInt32(static_cast<uint32_t>(Offset));
      }
    } else {
      if (LiveOffsets.size() >= (HasCSRMap ? (1 << 15) : (1 << 16))) {
        // Very rude!
        report_fatal_error("[OxCamlGCPrinter] live count requires long frames: "
          + Twine(LiveOffsets.size()));
      }

      OS.emitInt16(static_cast<uint16_t>(FrameData));
      OS.emitInt16(static_cast<uint16_t>(
          LiveOffsets.size() | (HasCSRMap ? (1 << 15) : 0)));
      for (int64_t Offset : LiveOffsets) {
        checkShortStackOffset(Offset);
        OS.emitInt16(static_cast<uint16_t>(Offset));
      }
    }

    if (HasCSRMap) {
      if (CSRRootMap.size() >= 1 << 16)
        report_fatal_error("[OxCamlGCPrinter] CSR root map too large: " +
                           Twine(CSRRootMap.size()));
      OS.emitInt16(CSRRootMap.size());
      for (const auto &Entry : CSRRootMap) {
        unsigned OxCamlIndex = mapLLVMDwarfRegToOxCamlIndex(
            M, Entry.DwarfRegNum);
        if (OxCamlIndex >= 1 << 16)
          report_fatal_error("[OxCamlGCPrinter] CSR root register index too "
                             "large: " +
                             Twine(OxCamlIndex));
        OS.emitInt16(OxCamlIndex);
        int64_t Offset = stackOffset(FrameSize, PtrSize, Entry.Offset);
        checkShortStackOffset(Offset);
        OS.emitInt16(static_cast<uint16_t>(Offset));
      }
    }

    if (HasAlloc) {
      if (AllocSize == 0) {
        // Poll frames are encoded like allocation frames with no allocation
        // entries. This matches Dbg_alloc [] in OxCaml's normal backend.
        OS.emitInt8(0);
      } else {
        OS.emitInt8(AllocSizes.size());
        for (uint8_t Size : AllocSizes)
          OS.emitInt8(Size);
      }
    }

    if (HasDebug) {
      if (HasAlloc && HasAllocDebug) {
        for (MCSymbol *AllocDebugLabel : AllocDebugLabels) {
          if (AllocDebugLabel == nullptr) {
            OS.emitValueToAlignment(Align(4));
            OS.emitInt32(0);
          } else {
            emitDebugOffset(OS, AllocDebugLabel);
          }
        }
      } else {
        size_t NumDebugOffsets = HasAlloc ? AllocSizes.size() : 1;
        for (size_t I = 0; I < NumDebugOffsets; ++I)
          emitDebugOffset(OS, DebugLabel);
      }
    }

    OS.emitValueToAlignment(Align(PtrSize));
  }

  for (const PendingDebugInfo &DebugInfo : PendingDebugInfos)
    emitPendingDebugInfo(OS, DebugInfo);

  OS.addBlankLine();
  return true;
}
