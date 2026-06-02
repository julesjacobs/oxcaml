; RUN: opt -S -O3 < %s | opt -S -passes=rewrite-statepoints-for-gc,verify | FileCheck %s
;
; Reduced from a self-stage2 compile of
; backend/debug/dwarf/dwarf_high/assign_abbrevs.ml. Before the
; eh.recover-vs-earlier-value compatibility rule, late EH-root materialization
; tried to append a pre-recovery PHI to a statepoint bundle that already used
; the recovered value for the same logical root and aborted with:
; "conflicting OxCaml EH root value for existing statepoint bundle".
;
; CHECK-DAG: "oxcaml-eh-live"
; CHECK-DAG: @llvm.oxcaml.gc.eh.recover

define  oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlDwarf_high__Assign_abbrevs__fn$5bbackend$2fdebug$2fdwarf$2fdwarf_high$2fassign_abbrevs.ml$3a41$2c9$2d$2d2423$5d_1_3_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) %3, ptr addrspace(1) %4) "oxcaml-stack-check"="true" "oxcaml-stack-check-before-bytes"="0" "oxcaml-stack-check-bytes"="80" noinline gc "oxcaml" personality ptr @"\01_caml_llvm_eh_personality" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %6 = alloca ptr addrspace(1) 
  store ptr addrspace(1) %2, ptr %6
  %7 = alloca ptr addrspace(1) 
  store ptr addrspace(1) %3, ptr %7
  %8 = alloca ptr addrspace(1) 
  store ptr addrspace(1) %4, ptr %8
  %9 = alloca i64 
  %10 = alloca ptr addrspace(1) 
  %11 = alloca ptr addrspace(1) 
  %12 = alloca ptr addrspace(1) 
  %13 = alloca ptr addrspace(1) 
  %14 = alloca ptr addrspace(1) 
  %15 = alloca ptr addrspace(1) 
  %16 = alloca ptr addrspace(1) 
  %17 = alloca ptr addrspace(1) 
  %18 = alloca ptr addrspace(1) 
  %19 = alloca ptr addrspace(1) 
  %20 = alloca ptr addrspace(1) 
  %21 = alloca ptr addrspace(1) 
  %22 = alloca ptr addrspace(1) 
  %23 = alloca ptr addrspace(1) 
  %24 = alloca ptr addrspace(1) 
  %25 = alloca ptr addrspace(1) 
  %26 = alloca i64 
  %27 = alloca i64 
  %28 = alloca ptr addrspace(1) 
  %29 = alloca ptr addrspace(1) 
  %30 = alloca ptr addrspace(1) 
  %31 = alloca i64 
  %32 = alloca i64 
  %33 = alloca ptr addrspace(1) 
  %34 = alloca ptr addrspace(1) 
  %35 = alloca ptr addrspace(1) 
  %36 = alloca ptr addrspace(1) 
  %37 = alloca ptr addrspace(1) 
  %38 = alloca ptr addrspace(1) 
  %39 = alloca i64 
  %40 = alloca i64 
  %41 = alloca ptr addrspace(1) 
  %42 = alloca ptr addrspace(1) 
  %43 = alloca ptr addrspace(1) 
  %44 = alloca ptr addrspace(1) 
  %45 = alloca ptr addrspace(1) 
  %46 = alloca i64 
  %47 = alloca i64 
  %48 = alloca ptr addrspace(1) 
  %49 = alloca ptr addrspace(1) 
  %50 = alloca ptr addrspace(1) 
  %51 = alloca ptr addrspace(1) 
  %52 = alloca ptr addrspace(1) 
  %53 = alloca ptr addrspace(1) 
  %54 = alloca ptr addrspace(1) 
  %55 = alloca ptr addrspace(1) 
  %56 = alloca ptr addrspace(1) 
  %57 = alloca ptr addrspace(1) 
  %58 = alloca ptr addrspace(1) 
  %59 = alloca ptr addrspace(1) 
  %60 = alloca ptr addrspace(1) 
  %61 = alloca ptr addrspace(1) 
  %62 = alloca ptr addrspace(1) 
  %63 = alloca ptr addrspace(1) 
  %64 = alloca ptr addrspace(1) 
  %65 = alloca ptr addrspace(1) 
  %66 = alloca ptr addrspace(1) 
  %67 = alloca i64 
  %68 = alloca i64 
  %69 = alloca ptr addrspace(1) 
  %70 = alloca ptr addrspace(1) 
  %71 = alloca i64 
  %72 = alloca ptr addrspace(1) 
  %73 = alloca ptr addrspace(1) 
  %74 = alloca ptr addrspace(1) 
  %75 = alloca ptr addrspace(1) 
  %76 = alloca ptr addrspace(1) 
  %77 = alloca i64 
  %78 = alloca ptr addrspace(1) 
  %79 = alloca i64 
  %80 = alloca i64 
  %81 = alloca ptr addrspace(1) 
  %82 = alloca ptr addrspace(1) 
  %83 = alloca ptr addrspace(1) 
  %84 = alloca ptr addrspace(1) 
  %85 = alloca i64 
  %86 = alloca ptr addrspace(1) 
  %87 = alloca ptr addrspace(1) 
  %88 = alloca ptr addrspace(1) 
  %89 = alloca ptr addrspace(1) 
  %90 = alloca i64 
  %91 = alloca i64 
  %92 = alloca ptr addrspace(1) 
  %93 = alloca ptr addrspace(1) 
  %94 = alloca ptr addrspace(1) 
  %95 = alloca ptr addrspace(1) 
  %96 = alloca ptr addrspace(1) 
  %97 = alloca ptr addrspace(1) 
  %98 = alloca ptr addrspace(1) 
  %99 = alloca ptr addrspace(1) 
  %100 = alloca i64 
  %101 = alloca ptr addrspace(1) 
  %102 = alloca i64 
  %103 = alloca ptr addrspace(1) 
  %104 = alloca i64 
  %105 = alloca i64 
  %106 = alloca i64 
  %107 = alloca i64 
  %108 = alloca i64 
  %109 = alloca ptr addrspace(1) 
  %110 = alloca i64 
  %111 = alloca i64 
  %112 = alloca ptr addrspace(1) 
  %113 = alloca ptr addrspace(1) 
  %114 = alloca ptr addrspace(1) 
  %115 = alloca ptr addrspace(1) 
  %116 = alloca ptr addrspace(1) 
  %117 = alloca ptr addrspace(1) 
  %118 = alloca ptr addrspace(1) 
  %119 = alloca ptr addrspace(1) 
  %120 = alloca ptr addrspace(1) 
  %121 = alloca i64 
  %122 = alloca i64 
  %123 = alloca i64 
  %124 = alloca i64 
  %125 = alloca ptr addrspace(1) 
  %126 = alloca i64 
  %127 = alloca ptr addrspace(1) 
  %128 = alloca i64 
  %129 = alloca i64 
  %130 = alloca ptr addrspace(1) 
  %131 = alloca ptr addrspace(1) 
  %132 = alloca ptr addrspace(1) 
  %133 = alloca ptr addrspace(1) 
  %134 = alloca ptr addrspace(1) 
  %135 = alloca ptr addrspace(1) 
  %136 = alloca ptr addrspace(1) 
  %137 = alloca ptr addrspace(1) 
  %138 = alloca i64 
  %139 = alloca ptr addrspace(1) 
  %140 = alloca ptr addrspace(1) 
  %141 = alloca ptr addrspace(1) 
  %142 = alloca ptr addrspace(1) 
  %143 = alloca ptr addrspace(1) 
  %144 = alloca ptr addrspace(1) 
  %145 = alloca i64 
  %146 = alloca i64 
  %147 = alloca i64 
  %148 = alloca i64 
  %149 = alloca i64 
  %150 = alloca i64 
  %151 = alloca i64 
  %152 = alloca ptr addrspace(1) 
  %153 = alloca i64 
  %154 = alloca ptr addrspace(1) 
  %155 = alloca ptr addrspace(1) 
  %156 = alloca i64 
  %157 = alloca ptr addrspace(1) 
  %158 = alloca ptr addrspace(1) 
  %159 = alloca ptr addrspace(1) 
  %160 = alloca ptr addrspace(1) 
  %161 = alloca ptr addrspace(1) 
  %162 = alloca ptr addrspace(1) 
  %163 = alloca ptr addrspace(1) 
  %164 = alloca ptr addrspace(1) 
  %165 = alloca ptr addrspace(1) 
  %166 = alloca i64 
  %167 = alloca ptr addrspace(1) 
  %168 = alloca ptr addrspace(1) 
  %169 = alloca ptr addrspace(1) 
  %170 = alloca ptr addrspace(1) 
  %171 = alloca ptr addrspace(1) 
  %172 = alloca ptr addrspace(1) 
  %173 = alloca ptr addrspace(1) 
  %174 = alloca i64 
  %175 = alloca ptr addrspace(1) 
  %176 = alloca ptr addrspace(1) 
  %177 = alloca ptr addrspace(1) 
  %178 = alloca ptr addrspace(1) 
  %179 = alloca i64 
  %180 = alloca i64
  %181 = alloca i64
  br label %L1
L1:
  br label %L169
L169:
  %182 = load i64, ptr %ds
  %183 = add i64 %182, 40
  %184 = inttoptr i64 %183 to ptr
  %185 = load i64, ptr %184
  %186 = add i64 %185, 440
  %187 = call i64 asm sideeffect "mov $0, sp", "=r"() "gc-leaf-function"="true"
  %188 = icmp uge i64 %187, %186
  %189 = call  i1 @llvm.expect.i1(i1 %188, i1 1) 
  br i1 %189, label %L361, label %L360
L360:
  %190 = load i64, ptr %ds
  %191 = load i64, ptr %alloc
  %192 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_llvm_call_realloc_stack"(i64 %190, i64 %191, i64 42) "statepoint-id"="0" cold
  %193 = extractvalue { { i64, i64 }, {  } } %192, 0, 0
  %194 = extractvalue { { i64, i64 }, {  } } %192, 0, 1
  store i64 %193, ptr %ds
  store i64 %194, ptr %alloc
  br label %L361
L361:
  %195 = load ptr addrspace(1), ptr %6
  store ptr addrspace(1) %195, ptr %11
  %196 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %196, ptr %12
  %197 = load ptr addrspace(1), ptr %8
  store ptr addrspace(1) %197, ptr %13
  %198 = load ptr addrspace(1), ptr %11
  %199 = getelementptr i8, ptr addrspace(1) %198, i64 24
  store ptr addrspace(1) %199, ptr %14
  %200 = load ptr addrspace(1), ptr %14
  %201 = load ptr addrspace(1), ptr addrspace(1) %200
  store ptr addrspace(1) %201, ptr %15
  %202 = load ptr addrspace(1), ptr %15
  store ptr addrspace(1) %202, ptr %16
  %203 = load ptr addrspace(1), ptr %11
  %204 = getelementptr i8, ptr addrspace(1) %203, i64 16
  store ptr addrspace(1) %204, ptr %17
  %205 = load ptr addrspace(1), ptr %17
  %206 = load ptr addrspace(1), ptr addrspace(1) %205
  store ptr addrspace(1) %206, ptr %18
  %207 = load ptr addrspace(1), ptr %18
  store ptr addrspace(1) %207, ptr %19
  %208 = load ptr addrspace(1), ptr %11
  %209 = load ptr addrspace(1), ptr addrspace(1) %208
  store ptr addrspace(1) %209, ptr %20
  %210 = load ptr addrspace(1), ptr %20
  store ptr addrspace(1) %210, ptr %21
  %211 = load ptr addrspace(1), ptr %12
  %212 = ptrtoint ptr addrspace(1) %211 to i64
  %213 = trunc i64 %212 to i1
  br i1 %213, label %L179, label %L213
L179:
  %214 = ptrtoint ptr @"\01_camlDwarf_low__Debugging_information_entry__create_null_14" to i64
  store i64 %214, ptr %26
  %215 = load i64, ptr %26
  %216 = add i64 %215, 16
  store i64 %216, ptr %27
  %217 = load i64, ptr %27
  %218 = inttoptr i64 %217 to ptr
  %219 = load ptr addrspace(1), ptr %218
  store ptr addrspace(1) %219, ptr %28
  %220 = load ptr addrspace(1), ptr %28
  store ptr addrspace(1) %220, ptr %6
  %221 = ptrtoint ptr @"\01_caml_obj_tag" to i64
  %222 = load ptr addrspace(1), ptr %6
  %223 = ptrtoint ptr addrspace(1) %222 to i64
  %224 = icmp eq i64 %223, 0
  br i1 %224, label %L363, label %L364
L363:
  %225 = inttoptr i64 2021 to ptr addrspace(1)
  store ptr addrspace(1) %225, ptr %6
  br label %L362
L364:
  %226 = and i64 %223, 1
  %227 = icmp ne i64 %226, 0
  br i1 %227, label %L365, label %L366
L365:
  %228 = inttoptr i64 2001 to ptr addrspace(1)
  store ptr addrspace(1) %228, ptr %6
  br label %L362
L366:
  %229 = and i64 %223, 7
  %230 = icmp ne i64 %229, 0
  br i1 %230, label %L367, label %L368
L367:
  %231 = inttoptr i64 2005 to ptr addrspace(1)
  store ptr addrspace(1) %231, ptr %6
  br label %L362
L368:
  %232 = inttoptr i64 %223 to ptr
  %233 = getelementptr i8, ptr %232, i64 -8
  %234 = load atomic i64, ptr %233 acquire, align 8
  %235 = and i64 %234, 255
  %236 = shl i64 %235, 1
  %237 = or i64 %236, 1
  %238 = inttoptr i64 %237 to ptr addrspace(1)
  store ptr addrspace(1) %238, ptr %6
  br label %L362
L362:
  br label %L181
L181:
  %239 = load ptr addrspace(1), ptr %6
  store ptr addrspace(1) %239, ptr %29
  %240 = load ptr addrspace(1), ptr %29
  store ptr addrspace(1) %240, ptr %30
  %241 = load ptr addrspace(1), ptr %30
  %242 = inttoptr i64 501 to ptr addrspace(1)
  %243 = icmp slt ptr addrspace(1) %241, %242
  br i1 %243, label %L192, label %L369
L369:
  %244 = load ptr addrspace(1), ptr %30
  %245 = inttoptr i64 501 to ptr addrspace(1)
  %246 = icmp sgt ptr addrspace(1) %244, %245
  br i1 %246, label %L192, label %L184
L184:
  %247 = ptrtoint ptr @"\01_camlDwarf_low__Debugging_information_entry__create_null_14" to i64
  store i64 %247, ptr %31
  %248 = load i64, ptr %31
  %249 = add i64 %248, 16
  store i64 %249, ptr %32
  %250 = load i64, ptr %32
  %251 = inttoptr i64 %250 to ptr
  %252 = load ptr addrspace(1), ptr %251
  store ptr addrspace(1) %252, ptr %33
  %253 = load ptr addrspace(1), ptr %33
  %254 = load ptr addrspace(1), ptr addrspace(1) %253
  store ptr addrspace(1) %254, ptr %34
  %255 = load ptr addrspace(1), ptr %21
  store ptr addrspace(1) %255, ptr %35
  %256 = load ptr addrspace(1), ptr %34
  store ptr addrspace(1) %256, ptr %36
  %257 = load ptr addrspace(1), ptr %19
  store ptr addrspace(1) %257, ptr %37
  %258 = load ptr addrspace(1), ptr %16
  store ptr addrspace(1) %258, ptr %38
  %259 = load ptr addrspace(1), ptr %35
  store ptr addrspace(1) %259, ptr %22
  %260 = load ptr addrspace(1), ptr %36
  store ptr addrspace(1) %260, ptr %23
  %261 = load ptr addrspace(1), ptr %37
  store ptr addrspace(1) %261, ptr %24
  %262 = load ptr addrspace(1), ptr %38
  store ptr addrspace(1) %262, ptr %25
  br label %L353
L192:
  %263 = load ptr addrspace(1), ptr %30
  %264 = inttoptr i64 493 to ptr addrspace(1)
  %265 = icmp slt ptr addrspace(1) %263, %264
  br i1 %265, label %L196, label %L370
L370:
  %266 = load ptr addrspace(1), ptr %30
  %267 = inttoptr i64 493 to ptr addrspace(1)
  %268 = icmp sgt ptr addrspace(1) %266, %267
  br i1 %268, label %L196, label %L206
L196:
  %269 = load ptr addrspace(1), ptr %30
  %270 = inttoptr i64 489 to ptr addrspace(1)
  %271 = icmp slt ptr addrspace(1) %269, %270
  br i1 %271, label %L200, label %L371
L371:
  %272 = load ptr addrspace(1), ptr %30
  %273 = inttoptr i64 489 to ptr addrspace(1)
  %274 = icmp sgt ptr addrspace(1) %272, %273
  br i1 %274, label %L200, label %L206
L200:
  %275 = ptrtoint ptr @"\01_camlDwarf_low__Debugging_information_entry__create_null_14" to i64
  store i64 %275, ptr %39
  %276 = load i64, ptr %39
  %277 = add i64 %276, 16
  store i64 %277, ptr %40
  %278 = load i64, ptr %40
  %279 = inttoptr i64 %278 to ptr
  %280 = load ptr addrspace(1), ptr %279
  store ptr addrspace(1) %280, ptr %41
  %281 = load ptr addrspace(1), ptr %21
  store ptr addrspace(1) %281, ptr %42
  %282 = load ptr addrspace(1), ptr %41
  store ptr addrspace(1) %282, ptr %43
  %283 = load ptr addrspace(1), ptr %19
  store ptr addrspace(1) %283, ptr %44
  %284 = load ptr addrspace(1), ptr %16
  store ptr addrspace(1) %284, ptr %45
  %285 = load ptr addrspace(1), ptr %42
  store ptr addrspace(1) %285, ptr %22
  %286 = load ptr addrspace(1), ptr %43
  store ptr addrspace(1) %286, ptr %23
  %287 = load ptr addrspace(1), ptr %44
  store ptr addrspace(1) %287, ptr %24
  %288 = load ptr addrspace(1), ptr %45
  store ptr addrspace(1) %288, ptr %25
  br label %L353
L206:
  %289 = ptrtoint ptr @"\01_camlDwarf_low__Debugging_information_entry__create_null_14" to i64
  store i64 %289, ptr %46
  %290 = load i64, ptr %46
  %291 = add i64 %290, 16
  store i64 %291, ptr %47
  %292 = load i64, ptr %47
  %293 = inttoptr i64 %292 to ptr
  %294 = load ptr addrspace(1), ptr %293
  store ptr addrspace(1) %294, ptr %48
  %295 = load ptr addrspace(1), ptr %48
  %296 = call ptr addrspace(1) asm  "", "=r,0"(ptr addrspace(1) %295) "gc-leaf-function"="true"
  store ptr addrspace(1) %296, ptr %48
  %297 = load ptr addrspace(1), ptr %48
  store ptr addrspace(1) %297, ptr %49
  %298 = load ptr addrspace(1), ptr %49
  store ptr addrspace(1) %298, ptr %6
  %299 = load ptr addrspace(1), ptr %6
  %300 = load i64, ptr %ds
  %301 = load i64, ptr %alloc
  %302 = call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlCamlinternalLazy__force_lazy_block_4_10_code"(i64 %300, i64 %301, ptr addrspace(1) %299) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 2, i64 41, i64 0, i64 21, i64 36, i64 0, i64 36, i64 60, i64 6512994, i64 7234923, i64 6565732, i64 7692901, i64 6565735, i64 7496055, i64 6565734, i64 7496055, i64 7102310, i64 3110767, i64 6448484, i64 6776693, i64 6778473, i64 7235935, i64 7499622, i64 7627117, i64 7237481, i64 7234911, i64 7959156, i64 7105838, i64 50, i64 6387524, i64 6252146, i64 7827308, i64 4480863, i64 7692901, i64 6907751, i64 6252398, i64 6712937, i64 7172719, i64 6911073, i64 6254191, i64 7630437, i64 3045746, i64 6648419, i64 6648929, i64 7695967, i64 27756, i64 49, i64 0, i64 14, i64 32, i64 0, i64 32, i64 48, i64 6512994, i64 7234923, i64 6565732, i64 7692901, i64 6565735, i64 7496055, i64 6565734, i64 7496055, i64 6840166, i64 6842217, i64 7561519, i64 6777203, i64 6381422, i64 7496290, i64 7566949, i64 7105838, i64 36, i64 6387524, i64 6252146, i64 6777192, i64 6250344, i64 7566145, i64 7235433, i64 6447455, i64 6648418, i64 3044214, i64 7239026, i64 6694958, i64 2715253) ]
  %303 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %302, 0, 0
  %304 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %302, 0, 1
  store i64 %303, ptr %ds
  store i64 %304, ptr %alloc
  %305 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %302, 1, 0
  store ptr addrspace(1) %305, ptr %6
  br label %L210
L210:
  %306 = load ptr addrspace(1), ptr %6
  store ptr addrspace(1) %306, ptr %50
  %307 = load ptr addrspace(1), ptr %50
  store ptr addrspace(1) %307, ptr %51
  %308 = load ptr addrspace(1), ptr %21
  store ptr addrspace(1) %308, ptr %52
  %309 = load ptr addrspace(1), ptr %51
  store ptr addrspace(1) %309, ptr %53
  %310 = load ptr addrspace(1), ptr %19
  store ptr addrspace(1) %310, ptr %54
  %311 = load ptr addrspace(1), ptr %16
  store ptr addrspace(1) %311, ptr %55
  %312 = load ptr addrspace(1), ptr %52
  store ptr addrspace(1) %312, ptr %22
  %313 = load ptr addrspace(1), ptr %53
  store ptr addrspace(1) %313, ptr %23
  %314 = load ptr addrspace(1), ptr %54
  store ptr addrspace(1) %314, ptr %24
  %315 = load ptr addrspace(1), ptr %55
  store ptr addrspace(1) %315, ptr %25
  br label %L353
L213:
  %316 = load ptr addrspace(1), ptr %12
  %317 = getelementptr i8, ptr addrspace(1) %316, i64 40
  store ptr addrspace(1) %317, ptr %56
  %318 = load ptr addrspace(1), ptr %56
  %319 = load ptr addrspace(1), ptr addrspace(1) %318
  store ptr addrspace(1) %319, ptr %57
  %320 = load ptr addrspace(1), ptr %57
  store ptr addrspace(1) %320, ptr %58
  %321 = load ptr addrspace(1), ptr %12
  %322 = getelementptr i8, ptr addrspace(1) %321, i64 16
  store ptr addrspace(1) %322, ptr %59
  %323 = load ptr addrspace(1), ptr %59
  %324 = load ptr addrspace(1), ptr addrspace(1) %323
  store ptr addrspace(1) %324, ptr %60
  %325 = load ptr addrspace(1), ptr %60
  store ptr addrspace(1) %325, ptr %61
  %326 = load ptr addrspace(1), ptr %12
  %327 = getelementptr i8, ptr addrspace(1) %326, i64 8
  store ptr addrspace(1) %327, ptr %62
  %328 = load ptr addrspace(1), ptr %62
  %329 = load ptr addrspace(1), ptr addrspace(1) %328
  store ptr addrspace(1) %329, ptr %63
  %330 = load ptr addrspace(1), ptr %63
  store ptr addrspace(1) %330, ptr %64
  %331 = load ptr addrspace(1), ptr %12
  %332 = load ptr addrspace(1), ptr addrspace(1) %331
  store ptr addrspace(1) %332, ptr %65
  %333 = load ptr addrspace(1), ptr %65
  store ptr addrspace(1) %333, ptr %66
  %334 = ptrtoint ptr @"\01_camlDwarf_low__Dwarf_attributes__Pmakeblock6606" to i64
  store i64 %334, ptr %67
  %335 = load i64, ptr %67
  %336 = add i64 %335, 400
  store i64 %336, ptr %68
  %337 = load i64, ptr %68
  %338 = inttoptr i64 %337 to ptr
  %339 = load ptr addrspace(1), ptr %338
  store ptr addrspace(1) %339, ptr %69
  %340 = load ptr addrspace(1), ptr %69
  store ptr addrspace(1) %340, ptr %70
  %341 = load ptr addrspace(1), ptr %70
  %342 = load i64, ptr addrspace(1) %341
  store i64 %342, ptr %71
  %343 = load ptr addrspace(1), ptr %61
  store ptr addrspace(1) %343, ptr %6
  %344 = load ptr addrspace(1), ptr %70
  store ptr addrspace(1) %344, ptr %7
  %345 = load ptr addrspace(1), ptr %6
  %346 = load ptr addrspace(1), ptr %7
  %347 = load i64, ptr %ds
  %348 = load i64, ptr %alloc
  %349 = load i64, ptr %71
  %350 = inttoptr i64 %349 to ptr
  %351 = call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } %350(i64 %347, i64 %348, ptr addrspace(1) %345, ptr addrspace(1) %346) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 60, i64 0, i64 34, i64 63, i64 0, i64 63, i64 48, i64 6512994, i64 7234923, i64 6565732, i64 7692901, i64 6565735, i64 7496055, i64 6565734, i64 7496055, i64 6840166, i64 6842217, i64 7561519, i64 6777203, i64 6381422, i64 7496290, i64 7566949, i64 7105838, i64 36, i64 6387524, i64 6252146, i64 6777192, i64 6250344, i64 7566145, i64 7235433, i64 6447455, i64 6648418, i64 3044214, i64 7239026, i64 6694958, i64 2715253) ]
  %352 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %351, 0, 0
  %353 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %351, 0, 1
  store i64 %352, ptr %ds
  store i64 %353, ptr %alloc
  %354 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %351, 1, 0
  store ptr addrspace(1) %354, ptr %6
  br label %L224
L224:
  %355 = load ptr addrspace(1), ptr %6
  store ptr addrspace(1) %355, ptr %72
  %356 = load ptr addrspace(1), ptr %72
  store ptr addrspace(1) %356, ptr %73
  %357 = load i64, ptr %ds
  %358 = add i64 %357, 64
  %359 = inttoptr i64 %358 to ptr
  %360 = load i64, ptr %359
  store i64 %360, ptr %79
  %361 = load i64, ptr %79
  store i64 %361, ptr %80
  %362 = load i64, ptr %alloc
  %363 = sub i64 %362, 32
  store i64 %363, ptr %alloc
  %364 = load i64, ptr %ds
  %365 = inttoptr i64 %364 to ptr
  %366 = load i64, ptr %365
  %367 = icmp ule i64 %366, %363
  %368 = call  i1 @llvm.expect.i1(i1 %367, i1 1) 
  br i1 %368, label %L373, label %L372
L372:
  %369 = load i64, ptr %ds
  %370 = load i64, ptr %alloc
  %371 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_call_gc"(i64 %369, i64 %370) "statepoint-id"="262145" cold [ "deopt"(i64 1870160737, i64 1, i64 1, i64 4, i64 2, i64 74, i64 0, i64 20, i64 58, i64 0, i64 58, i64 52, i64 6512994, i64 7234923, i64 6565732, i64 7692901, i64 6565735, i64 7496055, i64 6565734, i64 7496055, i64 7102310, i64 3110767, i64 6447713, i64 7759218, i64 7627113, i64 7237481, i64 7626611, i64 7103073, i64 7155301, i64 108, i64 35, i64 6387524, i64 6252146, i64 7827308, i64 4284255, i64 7496290, i64 6911589, i64 6911073, i64 7564911, i64 6386783, i64 6646882, i64 6907438, i64 25710, i64 63, i64 1, i64 16, i64 34, i64 73, i64 107, i64 48, i64 6512994, i64 7234923, i64 6565732, i64 7692901, i64 6565735, i64 7496055, i64 6565734, i64 7496055, i64 6840166, i64 6842217, i64 7561519, i64 6777203, i64 6381422, i64 7496290, i64 7566949, i64 7105838, i64 36, i64 6387524, i64 6252146, i64 6777192, i64 6250344, i64 7566145, i64 7235433, i64 6447455, i64 6648418, i64 3044214, i64 7239026, i64 6694958, i64 2715253) ]
  %372 = extractvalue { { i64, i64 }, {  } } %371, 0, 0
  %373 = extractvalue { { i64, i64 }, {  } } %371, 0, 1
  store i64 %372, ptr %ds
  store i64 %373, ptr %alloc
  br label %L373
L373:
  %374 = load i64, ptr %alloc
  %375 = add i64 %374, 8
  %376 = inttoptr i64 %375 to ptr addrspace(1)
  store ptr addrspace(1) %376, ptr %84
  %377 = load ptr addrspace(1), ptr %84
  %378 = getelementptr i8, ptr addrspace(1) %377, i64 -8
  store volatile i64 3072, ptr addrspace(1) %378
  %379 = load ptr addrspace(1), ptr %84
  %380 = load ptr addrspace(1), ptr %66
  store ptr addrspace(1) %380, ptr addrspace(1) %379
  %381 = load ptr addrspace(1), ptr %84
  %382 = getelementptr i8, ptr addrspace(1) %381, i64 8
  %383 = load ptr addrspace(1), ptr %64
  store ptr addrspace(1) %383, ptr addrspace(1) %382
  %384 = load ptr addrspace(1), ptr %84
  %385 = getelementptr i8, ptr addrspace(1) %384, i64 16
  %386 = load ptr addrspace(1), ptr %73
  store ptr addrspace(1) %386, ptr addrspace(1) %385
  %387 = load ptr addrspace(1), ptr %84
  store ptr addrspace(1) %387, ptr %86
  %388 = load ptr addrspace(1), ptr %86
  store ptr addrspace(1) %388, ptr %87
  %389 = load ptr addrspace(1), ptr %87
  store ptr addrspace(1) %389, ptr %83
  call  void @llvm.aarch64.oxcaml.push.trap(ptr blockaddress(@"\01_camlDwarf_high__Assign_abbrevs__fn$5bbackend$2fdebug$2fdwarf$2fdwarf_high$2fassign_abbrevs.ml$3a41$2c9$2d$2d2423$5d_1_3_code", %L375)) 
  br label %L374
L375:
  %390 = landingpad token cleanup
  %391 = call  { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover() 
  %392 = extractvalue { ptr addrspace(1), i64, i64, i64 } %391, 0
  %393 = extractvalue { ptr addrspace(1), i64, i64, i64 } %391, 2
  %394 = extractvalue { ptr addrspace(1), i64, i64, i64 } %391, 3
  store ptr addrspace(1) %392, ptr %180
  store i64 %394, ptr %ds
  store i64 %393, ptr %alloc
  br label %L233
L374:
  call  void @llvm.aarch64.oxcaml.push.trap(ptr blockaddress(@"\01_camlDwarf_high__Assign_abbrevs__fn$5bbackend$2fdebug$2fdwarf$2fdwarf_high$2fassign_abbrevs.ml$3a41$2c9$2d$2d2423$5d_1_3_code", %L377)) 
  br label %L376
L377:
  %395 = landingpad token cleanup
  %396 = call  { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover() 
  %397 = extractvalue { ptr addrspace(1), i64, i64, i64 } %396, 0
  %398 = extractvalue { ptr addrspace(1), i64, i64, i64 } %396, 2
  %399 = extractvalue { ptr addrspace(1), i64, i64, i64 } %396, 3
  store ptr addrspace(1) %397, ptr %181
  store i64 %399, ptr %ds
  store i64 %398, ptr %alloc
  br label %L242
L376:
  %400 = ptrtoint ptr @"\01_camlDwarf_low__Abbreviations_table__Pmakeblock3174" to i64
  store i64 %400, ptr %90
  %401 = load i64, ptr %90
  %402 = add i64 %401, 128
  store i64 %402, ptr %91
  %403 = load i64, ptr %91
  %404 = inttoptr i64 %403 to ptr
  %405 = load ptr addrspace(1), ptr %404
  store ptr addrspace(1) %405, ptr %92
  %406 = load ptr addrspace(1), ptr %83
  store ptr addrspace(1) %406, ptr %6
  %407 = load ptr addrspace(1), ptr %21
  store ptr addrspace(1) %407, ptr %7
  %408 = load ptr addrspace(1), ptr %92
  store ptr addrspace(1) %408, ptr %8
  %409 = load ptr addrspace(1), ptr %6
  %410 = load ptr addrspace(1), ptr %7
  %411 = load ptr addrspace(1), ptr %8
  %412 = load i64, ptr %ds
  %413 = load i64, ptr %alloc
  %414 = invoke oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_apply2"(i64 %412, i64 %413, ptr addrspace(1) %409, ptr addrspace(1) %410, ptr addrspace(1) %411) "statepoint-id"="36" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 2, i64 75, i64 0, i64 8, i64 26, i64 0, i64 26, i64 52, i64 6512994, i64 7234923, i64 6565732, i64 7692901, i64 6565735, i64 7496055, i64 6565734, i64 7496055, i64 7102310, i64 3110767, i64 6447713, i64 7759218, i64 7627113, i64 7237481, i64 7626611, i64 7103073, i64 7155301, i64 108, i64 35, i64 6387524, i64 6252146, i64 7827308, i64 4284255, i64 7496290, i64 6911589, i64 6911073, i64 7564911, i64 6386783, i64 6646882, i64 6907438, i64 25710, i64 63, i64 1, i64 16, i64 34, i64 73, i64 107, i64 48, i64 6512994, i64 7234923, i64 6565732, i64 7692901, i64 6565735, i64 7496055, i64 6565734, i64 7496055, i64 6840166, i64 6842217, i64 7561519, i64 6777203, i64 6381422, i64 7496290, i64 7566949, i64 7105838, i64 36, i64 6387524, i64 6252146, i64 6777192, i64 6250344, i64 7566145, i64 7235433, i64 6447455, i64 6648418, i64 3044214, i64 7239026, i64 6694958, i64 2715253) ] to label %L378 unwind label %L377
L378:
  %415 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %414, 0, 0
  %416 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %414, 0, 1
  store i64 %415, ptr %ds
  store i64 %416, ptr %alloc
  %417 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %414, 1, 0
  store ptr addrspace(1) %417, ptr %6
  br label %L253
L253:
  %418 = load ptr addrspace(1), ptr %6
  store ptr addrspace(1) %418, ptr %93
  %419 = load ptr addrspace(1), ptr %93
  store ptr addrspace(1) %419, ptr %94
  %420 = load ptr addrspace(1), ptr %94
  store ptr addrspace(1) %420, ptr %95
  %421 = load ptr addrspace(1), ptr %95
  store ptr addrspace(1) %421, ptr %89
  call  void @llvm.aarch64.oxcaml.pop.trap() 
  %422 = load ptr addrspace(1), ptr %89
  store ptr addrspace(1) %422, ptr %96
  %423 = load ptr addrspace(1), ptr %96
  store ptr addrspace(1) %423, ptr %97
  %424 = load ptr addrspace(1), ptr %97
  store ptr addrspace(1) %424, ptr %98
  call  void @llvm.aarch64.oxcaml.pop.trap() 
  %425 = load ptr addrspace(1), ptr %98
  %426 = load ptr addrspace(1), ptr addrspace(1) %425
  store ptr addrspace(1) %426, ptr %99
  %427 = load ptr addrspace(1), ptr %83
  store ptr addrspace(1) %427, ptr %101
  store i64 0, ptr %102
  %428 = load ptr addrspace(1), ptr %99
  store ptr addrspace(1) %428, ptr %103
  %429 = load ptr addrspace(1), ptr %101
  store ptr addrspace(1) %429, ptr %76
  %430 = load i64, ptr %102
  store i64 %430, ptr %77
  %431 = load ptr addrspace(1), ptr %103
  store ptr addrspace(1) %431, ptr %78
  br label %L299
L242:
  %432 = load i64, ptr %181
  %433 = inttoptr i64 %432 to ptr addrspace(1)
  store ptr addrspace(1) %433, ptr %6
  %434 = load ptr addrspace(1), ptr %6
  store ptr addrspace(1) %434, ptr %88
  %435 = load ptr addrspace(1), ptr %88
  store ptr addrspace(1) %435, ptr %6
  %436 = load ptr addrspace(1), ptr %83
  store ptr addrspace(1) %436, ptr %82
  %437 = load ptr addrspace(1), ptr %6
  %438 = load i64, ptr %ds
  %439 = load i64, ptr %alloc
  %440 = invoke oxcaml_nofpcc { { i64, i64 }, {  } } @"\01_caml_reraise_exn"(i64 %438, i64 %439, ptr addrspace(1) %437) "statepoint-id"="18" [ "deopt"(i64 1870160740, i64 1, i64 2, i64 2, i64 75, i64 0, i64 8, i64 26, i64 0, i64 26, i64 52, i64 6512994, i64 7234923, i64 6565732, i64 7692901, i64 6565735, i64 7496055, i64 6565734, i64 7496055, i64 7102310, i64 3110767, i64 6447713, i64 7759218, i64 7627113, i64 7237481, i64 7626611, i64 7103073, i64 7155301, i64 108, i64 35, i64 6387524, i64 6252146, i64 7827308, i64 4284255, i64 7496290, i64 6911589, i64 6911073, i64 7564911, i64 6386783, i64 6646882, i64 6907438, i64 25710, i64 63, i64 1, i64 16, i64 34, i64 73, i64 107, i64 48, i64 6512994, i64 7234923, i64 6565732, i64 7692901, i64 6565735, i64 7496055, i64 6565734, i64 7496055, i64 6840166, i64 6842217, i64 7561519, i64 6777203, i64 6381422, i64 7496290, i64 7566949, i64 7105838, i64 36, i64 6387524, i64 6252146, i64 6777192, i64 6250344, i64 7566145, i64 7235433, i64 6447455, i64 6648418, i64 3044214, i64 7239026, i64 6694958, i64 2715253) ] to label %L379 unwind label %L375
L379:
  %441 = extractvalue { { i64, i64 }, {  } } %440, 0, 0
  %442 = extractvalue { { i64, i64 }, {  } } %440, 0, 1
  store i64 %441, ptr %ds
  store i64 %442, ptr %alloc
  unreachable
L233:
  %443 = load i64, ptr %180
  %444 = inttoptr i64 %443 to ptr addrspace(1)
  store ptr addrspace(1) %444, ptr %6
  %445 = load ptr addrspace(1), ptr %6
  store ptr addrspace(1) %445, ptr %81
  %446 = load i64, ptr %ds
  %447 = add i64 %446, 64
  %448 = inttoptr i64 %447 to ptr
  %449 = load i64, ptr %80
  store i64 %449, ptr %448
  store i64 1, ptr %105
  %450 = ptrtoint ptr @"\01_caml_exn_Not_found" to i64
  store i64 %450, ptr %106
  %451 = load ptr addrspace(1), ptr %81
  %452 = load i64, ptr %106
  %453 = inttoptr i64 %452 to ptr addrspace(1)
  %454 = icmp slt ptr addrspace(1) %451, %453
  br i1 %454, label %L276, label %L380
L380:
  %455 = load ptr addrspace(1), ptr %81
  %456 = load i64, ptr %106
  %457 = inttoptr i64 %456 to ptr addrspace(1)
  %458 = icmp sgt ptr addrspace(1) %455, %457
  br i1 %458, label %L276, label %L274
L274:
  %459 = load ptr addrspace(1), ptr %82
  store ptr addrspace(1) %459, ptr %109
  store i64 1, ptr %110
  store i64 1, ptr %111
  %460 = load ptr addrspace(1), ptr %109
  store ptr addrspace(1) %460, ptr %76
  %461 = load i64, ptr %110
  store i64 %461, ptr %77
  %462 = load i64, ptr %111
  %463 = inttoptr i64 %462 to ptr addrspace(1)
  store ptr addrspace(1) %463, ptr %78
  br label %L282
L276:
  %464 = load ptr addrspace(1), ptr %81
  store ptr addrspace(1) %464, ptr %6
  %465 = load ptr addrspace(1), ptr %6
  %466 = load i64, ptr %ds
  %467 = load i64, ptr %alloc
  %468 = call oxcaml_nofpcc { { i64, i64 }, {  } } @"\01_caml_reraise_exn"(i64 %466, i64 %467, ptr addrspace(1) %465) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 2, i64 1, i64 63, i64 1, i64 16, i64 34, i64 73, i64 107, i64 48, i64 6512994, i64 7234923, i64 6565732, i64 7692901, i64 6565735, i64 7496055, i64 6565734, i64 7496055, i64 6840166, i64 6842217, i64 7561519, i64 6777203, i64 6381422, i64 7496290, i64 7566949, i64 7105838, i64 36, i64 6387524, i64 6252146, i64 6777192, i64 6250344, i64 7566145, i64 7235433, i64 6447455, i64 6648418, i64 3044214, i64 7239026, i64 6694958, i64 2715253) ]
  %469 = extractvalue { { i64, i64 }, {  } } %468, 0, 0
  %470 = extractvalue { { i64, i64 }, {  } } %468, 0, 1
  store i64 %469, ptr %ds
  store i64 %470, ptr %alloc
  unreachable
L282:
  %471 = load ptr addrspace(1), ptr %13
  %472 = getelementptr i8, ptr addrspace(1) %471, i64 24
  store ptr addrspace(1) %472, ptr %112
  %473 = load ptr addrspace(1), ptr %112
  %474 = load ptr addrspace(1), ptr addrspace(1) %473
  store ptr addrspace(1) %474, ptr %113
  %475 = load ptr addrspace(1), ptr %113
  %476 = load ptr addrspace(1), ptr addrspace(1) %475
  store ptr addrspace(1) %476, ptr %114
  %477 = load ptr addrspace(1), ptr %114
  store ptr addrspace(1) %477, ptr %6
  %478 = load ptr addrspace(1), ptr %66
  store ptr addrspace(1) %478, ptr %7
  %479 = load ptr addrspace(1), ptr %6
  %480 = load ptr addrspace(1), ptr %7
  %481 = load i64, ptr %ds
  %482 = load i64, ptr %alloc
  %483 = call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlDwarf_low__Abbreviation_code__of_int_0_7_code"(i64 %481, i64 %482, ptr addrspace(1) %479, ptr addrspace(1) %480) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 69, i64 0, i64 18, i64 70, i64 0, i64 70, i64 48, i64 6512994, i64 7234923, i64 6565732, i64 7692901, i64 6565735, i64 7496055, i64 6565734, i64 7496055, i64 6840166, i64 6842217, i64 7561519, i64 6777203, i64 6381422, i64 7496290, i64 7566949, i64 7105838, i64 36, i64 6387524, i64 6252146, i64 6777192, i64 6250344, i64 7566145, i64 7235433, i64 6447455, i64 6648418, i64 3044214, i64 7239026, i64 6694958, i64 2715253) ]
  %484 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %483, 0, 0
  %485 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %483, 0, 1
  store i64 %484, ptr %ds
  store i64 %485, ptr %alloc
  %486 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %483, 1, 0
  store ptr addrspace(1) %486, ptr %6
  br label %L284
L284:
  %487 = load ptr addrspace(1), ptr %6
  store ptr addrspace(1) %487, ptr %115
  %488 = load ptr addrspace(1), ptr %115
  store ptr addrspace(1) %488, ptr %116
  %489 = load ptr addrspace(1), ptr %13
  %490 = getelementptr i8, ptr addrspace(1) %489, i64 24
  store ptr addrspace(1) %490, ptr %117
  %491 = load ptr addrspace(1), ptr %117
  %492 = load ptr addrspace(1), ptr addrspace(1) %491
  store ptr addrspace(1) %492, ptr %118
  %493 = load ptr addrspace(1), ptr %13
  %494 = getelementptr i8, ptr addrspace(1) %493, i64 24
  store ptr addrspace(1) %494, ptr %119
  %495 = load ptr addrspace(1), ptr %119
  %496 = load ptr addrspace(1), ptr addrspace(1) %495
  store ptr addrspace(1) %496, ptr %120
  %497 = load ptr addrspace(1), ptr %120
  %498 = load i64, ptr addrspace(1) %497
  store i64 %498, ptr %121
  %499 = load i64, ptr %121
  %500 = add i64 %499, 2
  store i64 %500, ptr %122
  %501 = load ptr addrspace(1), ptr %118
  fence acquire
  %502 = load i64, ptr %122
  store i64 %502, ptr addrspace(1) %501
  store i64 1, ptr %124
  %503 = load i64, ptr %alloc
  %504 = sub i64 %503, 40
  store i64 %504, ptr %alloc
  %505 = load i64, ptr %ds
  %506 = inttoptr i64 %505 to ptr
  %507 = load i64, ptr %506
  %508 = icmp ule i64 %507, %504
  %509 = call  i1 @llvm.expect.i1(i1 %508, i1 1) 
  br i1 %509, label %L382, label %L381
L381:
  %510 = load i64, ptr %ds
  %511 = load i64, ptr %alloc
  %512 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_call_gc"(i64 %510, i64 %511) "statepoint-id"="327681" cold [ "deopt"(i64 1870160737, i64 1, i64 1, i64 5, i64 2, i64 29, i64 0, i64 2, i64 59, i64 0, i64 59, i64 58, i64 6512994, i64 7234923, i64 6565732, i64 7692901, i64 6565735, i64 7496055, i64 6565734, i64 7496055, i64 7102310, i64 3110767, i64 6447713, i64 7759218, i64 7627113, i64 7237481, i64 7626611, i64 7103073, i64 6643557, i64 7500910, i64 7155321, i64 108, i64 43, i64 6387524, i64 6252146, i64 7827308, i64 4284255, i64 7496290, i64 6911589, i64 6911073, i64 7564911, i64 6386783, i64 6646882, i64 7234911, i64 7959156, i64 7496494, i64 7627109, i64 101, i64 73, i64 1, i64 18, i64 50, i64 75, i64 125, i64 48, i64 6512994, i64 7234923, i64 6565732, i64 7692901, i64 6565735, i64 7496055, i64 6565734, i64 7496055, i64 6840166, i64 6842217, i64 7561519, i64 6777203, i64 6381422, i64 7496290, i64 7566949, i64 7105838, i64 36, i64 6387524, i64 6252146, i64 6777192, i64 6250344, i64 7566145, i64 7235433, i64 6447455, i64 6648418, i64 3044214, i64 7239026, i64 6694958, i64 2715253) ]
  %513 = extractvalue { { i64, i64 }, {  } } %512, 0, 0
  %514 = extractvalue { { i64, i64 }, {  } } %512, 0, 1
  store i64 %513, ptr %ds
  store i64 %514, ptr %alloc
  br label %L382
L382:
  %515 = load i64, ptr %alloc
  %516 = add i64 %515, 8
  %517 = inttoptr i64 %516 to ptr addrspace(1)
  store ptr addrspace(1) %517, ptr %125
  %518 = load ptr addrspace(1), ptr %125
  %519 = getelementptr i8, ptr addrspace(1) %518, i64 -8
  store volatile i64 4096, ptr addrspace(1) %519
  %520 = load ptr addrspace(1), ptr %125
  %521 = load ptr addrspace(1), ptr %116
  store ptr addrspace(1) %521, ptr addrspace(1) %520
  %522 = load ptr addrspace(1), ptr %125
  %523 = getelementptr i8, ptr addrspace(1) %522, i64 8
  %524 = load ptr addrspace(1), ptr %66
  store ptr addrspace(1) %524, ptr addrspace(1) %523
  %525 = load ptr addrspace(1), ptr %125
  %526 = getelementptr i8, ptr addrspace(1) %525, i64 16
  %527 = load ptr addrspace(1), ptr %64
  store ptr addrspace(1) %527, ptr addrspace(1) %526
  %528 = load ptr addrspace(1), ptr %125
  %529 = getelementptr i8, ptr addrspace(1) %528, i64 24
  %530 = load ptr addrspace(1), ptr %73
  store ptr addrspace(1) %530, ptr addrspace(1) %529
  %531 = load ptr addrspace(1), ptr %125
  store ptr addrspace(1) %531, ptr %127
  %532 = ptrtoint ptr @"\01_camlDwarf_low__Abbreviations_table__Pmakeblock3174" to i64
  store i64 %532, ptr %128
  %533 = load i64, ptr %128
  %534 = add i64 %533, 8
  store i64 %534, ptr %129
  %535 = load i64, ptr %129
  %536 = inttoptr i64 %535 to ptr
  %537 = load ptr addrspace(1), ptr %536
  store ptr addrspace(1) %537, ptr %130
  %538 = load ptr addrspace(1), ptr %76
  store ptr addrspace(1) %538, ptr %6
  %539 = load ptr addrspace(1), ptr %127
  store ptr addrspace(1) %539, ptr %7
  %540 = load ptr addrspace(1), ptr %21
  store ptr addrspace(1) %540, ptr %8
  %541 = load ptr addrspace(1), ptr %130
  store ptr addrspace(1) %541, ptr %10
  %542 = load ptr addrspace(1), ptr %6
  %543 = load ptr addrspace(1), ptr %7
  %544 = load ptr addrspace(1), ptr %8
  %545 = load ptr addrspace(1), ptr %10
  %546 = load i64, ptr %ds
  %547 = load i64, ptr %alloc
  %548 = call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_apply3"(i64 %546, i64 %547, ptr addrspace(1) %542, ptr addrspace(1) %543, ptr addrspace(1) %544, ptr addrspace(1) %545) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 2, i64 71, i64 0, i64 2, i64 25, i64 0, i64 25, i64 52, i64 6512994, i64 7234923, i64 6565732, i64 7692901, i64 6565735, i64 7496055, i64 6565734, i64 7496055, i64 7102310, i64 3110767, i64 6447713, i64 7759218, i64 7627113, i64 7237481, i64 7626611, i64 7103073, i64 7155301, i64 108, i64 34, i64 6387524, i64 6252146, i64 7827308, i64 4284255, i64 7496290, i64 6911589, i64 6911073, i64 7564911, i64 6386783, i64 6646882, i64 6578478, i64 100, i64 76, i64 0, i64 18, i64 73, i64 0, i64 73, i64 48, i64 6512994, i64 7234923, i64 6565732, i64 7692901, i64 6565735, i64 7496055, i64 6565734, i64 7496055, i64 6840166, i64 6842217, i64 7561519, i64 6777203, i64 6381422, i64 7496290, i64 7566949, i64 7105838, i64 36, i64 6387524, i64 6252146, i64 6777192, i64 6250344, i64 7566145, i64 7235433, i64 6447455, i64 6648418, i64 3044214, i64 7239026, i64 6694958, i64 2715253) ]
  %549 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %548, 0, 0
  %550 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %548, 0, 1
  store i64 %549, ptr %ds
  store i64 %550, ptr %alloc
  %551 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %548, 1, 0
  store ptr addrspace(1) %551, ptr %6
  br label %L296
L296:
  %552 = load ptr addrspace(1), ptr %6
  store ptr addrspace(1) %552, ptr %131
  %553 = load ptr addrspace(1), ptr %131
  store ptr addrspace(1) %553, ptr %132
  %554 = load ptr addrspace(1), ptr %132
  store ptr addrspace(1) %554, ptr %133
  %555 = load ptr addrspace(1), ptr %116
  store ptr addrspace(1) %555, ptr %134
  %556 = load ptr addrspace(1), ptr %133
  store ptr addrspace(1) %556, ptr %74
  %557 = load ptr addrspace(1), ptr %134
  store ptr addrspace(1) %557, ptr %75
  br label %L303
L299:
  %558 = load ptr addrspace(1), ptr %21
  store ptr addrspace(1) %558, ptr %135
  %559 = load ptr addrspace(1), ptr %78
  store ptr addrspace(1) %559, ptr %136
  %560 = load ptr addrspace(1), ptr %135
  store ptr addrspace(1) %560, ptr %74
  %561 = load ptr addrspace(1), ptr %136
  store ptr addrspace(1) %561, ptr %75
  br label %L303
L303:
  %562 = load i64, ptr %alloc
  %563 = sub i64 %562, 40
  store i64 %563, ptr %alloc
  %564 = load i64, ptr %ds
  %565 = inttoptr i64 %564 to ptr
  %566 = load i64, ptr %565
  %567 = icmp ule i64 %566, %563
  %568 = call  i1 @llvm.expect.i1(i1 %567, i1 1) 
  br i1 %568, label %L384, label %L383
L383:
  %569 = load i64, ptr %ds
  %570 = load i64, ptr %alloc
  %571 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_call_gc"(i64 %569, i64 %570) "statepoint-id"="327681" cold [ "deopt"(i64 1870160737, i64 1, i64 1, i64 5, i64 2, i64 31, i64 0, i64 2, i64 54, i64 0, i64 54, i64 60, i64 6512994, i64 7234923, i64 6565732, i64 7692901, i64 6565735, i64 7496055, i64 6565734, i64 7496055, i64 7102310, i64 3110767, i64 6448484, i64 6776693, i64 6778473, i64 7235935, i64 7499622, i64 7627117, i64 7237481, i64 7234911, i64 7959156, i64 7105838, i64 45, i64 6387524, i64 6252146, i64 7827308, i64 4480863, i64 7692901, i64 6907751, i64 6252398, i64 6712937, i64 7172719, i64 6911073, i64 6254191, i64 7630437, i64 3045746, i64 6648419, i64 6648929, i64 80, i64 0, i64 14, i64 74, i64 0, i64 74, i64 48, i64 6512994, i64 7234923, i64 6565732, i64 7692901, i64 6565735, i64 7496055, i64 6565734, i64 7496055, i64 6840166, i64 6842217, i64 7561519, i64 6777203, i64 6381422, i64 7496290, i64 7566949, i64 7105838, i64 36, i64 6387524, i64 6252146, i64 6777192, i64 6250344, i64 7566145, i64 7235433, i64 6447455, i64 6648418, i64 3044214, i64 7239026, i64 6694958, i64 2715253) ]
  %572 = extractvalue { { i64, i64 }, {  } } %571, 0, 0
  %573 = extractvalue { { i64, i64 }, {  } } %571, 0, 1
  store i64 %572, ptr %ds
  store i64 %573, ptr %alloc
  br label %L384
L384:
  %574 = load i64, ptr %alloc
  %575 = add i64 %574, 8
  %576 = inttoptr i64 %575 to ptr addrspace(1)
  store ptr addrspace(1) %576, ptr %137
  %577 = load ptr addrspace(1), ptr %137
  %578 = getelementptr i8, ptr addrspace(1) %577, i64 -8
  store volatile i64 4096, ptr addrspace(1) %578
  %579 = load ptr addrspace(1), ptr %12
  %580 = getelementptr i8, ptr addrspace(1) %579, i64 24
  store ptr addrspace(1) %580, ptr %139
  %581 = load ptr addrspace(1), ptr %139
  %582 = load ptr addrspace(1), ptr addrspace(1) %581
  store ptr addrspace(1) %582, ptr %140
  %583 = load ptr addrspace(1), ptr %137
  %584 = load ptr addrspace(1), ptr %140
  store ptr addrspace(1) %584, ptr addrspace(1) %583
  %585 = load ptr addrspace(1), ptr %12
  %586 = getelementptr i8, ptr addrspace(1) %585, i64 32
  store ptr addrspace(1) %586, ptr %141
  %587 = load ptr addrspace(1), ptr %141
  %588 = load ptr addrspace(1), ptr addrspace(1) %587
  store ptr addrspace(1) %588, ptr %142
  %589 = load ptr addrspace(1), ptr %137
  %590 = getelementptr i8, ptr addrspace(1) %589, i64 8
  %591 = load ptr addrspace(1), ptr %142
  store ptr addrspace(1) %591, ptr addrspace(1) %590
  %592 = load ptr addrspace(1), ptr %137
  %593 = getelementptr i8, ptr addrspace(1) %592, i64 16
  %594 = load ptr addrspace(1), ptr %75
  store ptr addrspace(1) %594, ptr addrspace(1) %593
  %595 = load ptr addrspace(1), ptr %137
  %596 = getelementptr i8, ptr addrspace(1) %595, i64 24
  %597 = load ptr addrspace(1), ptr %61
  store ptr addrspace(1) %597, ptr addrspace(1) %596
  %598 = load ptr addrspace(1), ptr %137
  store ptr addrspace(1) %598, ptr %143
  %599 = load ptr addrspace(1), ptr %66
  %600 = ptrtoint ptr addrspace(1) %599 to i64
  %601 = trunc i64 %600 to i1
  br i1 %601, label %L316, label %L323
L316:
  %602 = load ptr addrspace(1), ptr %66
  %603 = inttoptr i64 23 to ptr addrspace(1)
  %604 = icmp slt ptr addrspace(1) %602, %603
  br i1 %604, label %L318, label %L385
L385:
  %605 = load ptr addrspace(1), ptr %66
  %606 = inttoptr i64 23 to ptr addrspace(1)
  %607 = icmp sgt ptr addrspace(1) %605, %606
  br i1 %607, label %L318, label %L320
L318:
  store i64 0, ptr %147
  %608 = load i64, ptr %147
  store i64 %608, ptr %145
  br label %L337
L320:
  store i64 1, ptr %149
  %609 = load i64, ptr %149
  store i64 %609, ptr %145
  br label %L328
L323:
  store i64 0, ptr %151
  %610 = load i64, ptr %151
  store i64 %610, ptr %145
  br label %L337
L328:
  %611 = load ptr addrspace(1), ptr %19
  %612 = ptrtoint ptr addrspace(1) %611 to i64
  %613 = trunc i64 %612 to i1
  br i1 %613, label %L330, label %L333
L330:
  %614 = load i64, ptr %alloc
  %615 = sub i64 %614, 16
  store i64 %615, ptr %alloc
  %616 = load i64, ptr %ds
  %617 = inttoptr i64 %616 to ptr
  %618 = load i64, ptr %617
  %619 = icmp ule i64 %618, %615
  %620 = call  i1 @llvm.expect.i1(i1 %619, i1 1) 
  br i1 %620, label %L387, label %L386
L386:
  %621 = load i64, ptr %ds
  %622 = load i64, ptr %alloc
  %623 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_call_gc"(i64 %621, i64 %622) "statepoint-id"="131073" cold [ "deopt"(i64 1870160737, i64 1, i64 1, i64 2, i64 1, i64 90, i64 0, i64 26, i64 34, i64 0, i64 34, i64 48, i64 6512994, i64 7234923, i64 6565732, i64 7692901, i64 6565735, i64 7496055, i64 6565734, i64 7496055, i64 6840166, i64 6842217, i64 7561519, i64 6777203, i64 6381422, i64 7496290, i64 7566949, i64 7105838, i64 36, i64 6387524, i64 6252146, i64 6777192, i64 6250344, i64 7566145, i64 7235433, i64 6447455, i64 6648418, i64 3044214, i64 7239026, i64 6694958, i64 2715253) ]
  %624 = extractvalue { { i64, i64 }, {  } } %623, 0, 0
  %625 = extractvalue { { i64, i64 }, {  } } %623, 0, 1
  store i64 %624, ptr %ds
  store i64 %625, ptr %alloc
  br label %L387
L387:
  %626 = load i64, ptr %alloc
  %627 = add i64 %626, 8
  %628 = inttoptr i64 %627 to ptr addrspace(1)
  store ptr addrspace(1) %628, ptr %152
  %629 = load ptr addrspace(1), ptr %152
  %630 = getelementptr i8, ptr addrspace(1) %629, i64 -8
  store volatile i64 1024, ptr addrspace(1) %630
  %631 = load ptr addrspace(1), ptr %152
  %632 = load ptr addrspace(1), ptr %143
  store ptr addrspace(1) %632, ptr addrspace(1) %631
  %633 = load ptr addrspace(1), ptr %152
  store ptr addrspace(1) %633, ptr %154
  %634 = load ptr addrspace(1), ptr %154
  store ptr addrspace(1) %634, ptr %155
  %635 = load ptr addrspace(1), ptr %155
  store ptr addrspace(1) %635, ptr %144
  %636 = load ptr addrspace(1), ptr %58
  %637 = ptrtoint ptr addrspace(1) %636 to i64
  %638 = trunc i64 %637 to i1
  br i1 %638, label %L343, label %L345
L333:
  %639 = ptrtoint ptr @"\01_camlDwarf_high__Assign_abbrevs__immstring79" to i64
  store i64 %639, ptr %156
  %640 = load i64, ptr %156
  store i64 %640, ptr %9
  %641 = load i64, ptr %9
  %642 = load i64, ptr %ds
  %643 = load i64, ptr %alloc
  %644 = call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlMisc__fatal_error_2_355_code"(i64 %642, i64 %643, i64 %641) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 92, i64 0, i64 18, i64 80, i64 0, i64 80, i64 48, i64 6512994, i64 7234923, i64 6565732, i64 7692901, i64 6565735, i64 7496055, i64 6565734, i64 7496055, i64 6840166, i64 6842217, i64 7561519, i64 6777203, i64 6381422, i64 7496290, i64 7566949, i64 7105838, i64 36, i64 6387524, i64 6252146, i64 6777192, i64 6250344, i64 7566145, i64 7235433, i64 6447455, i64 6648418, i64 3044214, i64 7239026, i64 6694958, i64 2715253) ]
  %645 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %644, 0, 0
  %646 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %644, 0, 1
  store i64 %645, ptr %ds
  store i64 %646, ptr %alloc
  %647 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %644, 1, 0
  store ptr addrspace(1) %647, ptr %6
  br label %L335
L335:
  %648 = load ptr addrspace(1), ptr %6
  store ptr addrspace(1) %648, ptr %157
  %649 = load ptr addrspace(1), ptr %157
  store ptr addrspace(1) %649, ptr %158
  %650 = load ptr addrspace(1), ptr %158
  store ptr addrspace(1) %650, ptr %159
  %651 = load ptr addrspace(1), ptr %159
  store ptr addrspace(1) %651, ptr %144
  %652 = load ptr addrspace(1), ptr %58
  %653 = ptrtoint ptr addrspace(1) %652 to i64
  %654 = trunc i64 %653 to i1
  br i1 %654, label %L343, label %L345
L337:
  %655 = load ptr addrspace(1), ptr %19
  store ptr addrspace(1) %655, ptr %160
  %656 = load ptr addrspace(1), ptr %160
  store ptr addrspace(1) %656, ptr %144
  %657 = load ptr addrspace(1), ptr %58
  %658 = ptrtoint ptr addrspace(1) %657 to i64
  %659 = trunc i64 %658 to i1
  br i1 %659, label %L343, label %L345
L343:
  %660 = load ptr addrspace(1), ptr %74
  store ptr addrspace(1) %660, ptr %161
  %661 = load ptr addrspace(1), ptr %143
  store ptr addrspace(1) %661, ptr %162
  %662 = load ptr addrspace(1), ptr %144
  store ptr addrspace(1) %662, ptr %163
  %663 = load ptr addrspace(1), ptr %16
  store ptr addrspace(1) %663, ptr %164
  %664 = load ptr addrspace(1), ptr %161
  store ptr addrspace(1) %664, ptr %22
  %665 = load ptr addrspace(1), ptr %162
  store ptr addrspace(1) %665, ptr %23
  %666 = load ptr addrspace(1), ptr %163
  store ptr addrspace(1) %666, ptr %24
  %667 = load ptr addrspace(1), ptr %164
  store ptr addrspace(1) %667, ptr %25
  br label %L353
L345:
  %668 = load i64, ptr %alloc
  %669 = sub i64 %668, 24
  store i64 %669, ptr %alloc
  %670 = load i64, ptr %ds
  %671 = inttoptr i64 %670 to ptr
  %672 = load i64, ptr %671
  %673 = icmp ule i64 %672, %669
  %674 = call  i1 @llvm.expect.i1(i1 %673, i1 1) 
  br i1 %674, label %L389, label %L388
L388:
  %675 = load i64, ptr %ds
  %676 = load i64, ptr %alloc
  %677 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_call_gc"(i64 %675, i64 %676) "statepoint-id"="196609" cold [ "deopt"(i64 1870160737, i64 1, i64 1, i64 3, i64 1, i64 97, i64 0, i64 38, i64 73, i64 0, i64 73, i64 48, i64 6512994, i64 7234923, i64 6565732, i64 7692901, i64 6565735, i64 7496055, i64 6565734, i64 7496055, i64 6840166, i64 6842217, i64 7561519, i64 6777203, i64 6381422, i64 7496290, i64 7566949, i64 7105838, i64 36, i64 6387524, i64 6252146, i64 6777192, i64 6250344, i64 7566145, i64 7235433, i64 6447455, i64 6648418, i64 3044214, i64 7239026, i64 6694958, i64 2715253) ]
  %678 = extractvalue { { i64, i64 }, {  } } %677, 0, 0
  %679 = extractvalue { { i64, i64 }, {  } } %677, 0, 1
  store i64 %678, ptr %ds
  store i64 %679, ptr %alloc
  br label %L389
L389:
  %680 = load i64, ptr %alloc
  %681 = add i64 %680, 8
  %682 = inttoptr i64 %681 to ptr addrspace(1)
  store ptr addrspace(1) %682, ptr %165
  %683 = load ptr addrspace(1), ptr %165
  %684 = getelementptr i8, ptr addrspace(1) %683, i64 -8
  store volatile i64 2048, ptr addrspace(1) %684
  %685 = load ptr addrspace(1), ptr %58
  %686 = load ptr addrspace(1), ptr addrspace(1) %685
  store ptr addrspace(1) %686, ptr %167
  %687 = load ptr addrspace(1), ptr %165
  %688 = load ptr addrspace(1), ptr %167
  store ptr addrspace(1) %688, ptr addrspace(1) %687
  %689 = load ptr addrspace(1), ptr %165
  %690 = getelementptr i8, ptr addrspace(1) %689, i64 8
  %691 = load ptr addrspace(1), ptr %16
  store ptr addrspace(1) %691, ptr addrspace(1) %690
  %692 = load ptr addrspace(1), ptr %165
  store ptr addrspace(1) %692, ptr %168
  %693 = load ptr addrspace(1), ptr %74
  store ptr addrspace(1) %693, ptr %169
  %694 = load ptr addrspace(1), ptr %143
  store ptr addrspace(1) %694, ptr %170
  %695 = load ptr addrspace(1), ptr %144
  store ptr addrspace(1) %695, ptr %171
  %696 = load ptr addrspace(1), ptr %168
  store ptr addrspace(1) %696, ptr %172
  %697 = load ptr addrspace(1), ptr %169
  store ptr addrspace(1) %697, ptr %22
  %698 = load ptr addrspace(1), ptr %170
  store ptr addrspace(1) %698, ptr %23
  %699 = load ptr addrspace(1), ptr %171
  store ptr addrspace(1) %699, ptr %24
  %700 = load ptr addrspace(1), ptr %172
  store ptr addrspace(1) %700, ptr %25
  br label %L353
L353:
  %701 = load i64, ptr %alloc
  %702 = sub i64 %701, 24
  store i64 %702, ptr %alloc
  %703 = load i64, ptr %ds
  %704 = inttoptr i64 %703 to ptr
  %705 = load i64, ptr %704
  %706 = icmp ule i64 %705, %702
  %707 = call  i1 @llvm.expect.i1(i1 %706, i1 1) 
  br i1 %707, label %L391, label %L390
L390:
  %708 = load i64, ptr %ds
  %709 = load i64, ptr %alloc
  %710 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_call_gc"(i64 %708, i64 %709) "statepoint-id"="196609" cold [ "deopt"(i64 1870160737, i64 1, i64 1, i64 3, i64 1, i64 101, i64 0, i64 22, i64 33, i64 0, i64 33, i64 48, i64 6512994, i64 7234923, i64 6565732, i64 7692901, i64 6565735, i64 7496055, i64 6565734, i64 7496055, i64 6840166, i64 6842217, i64 7561519, i64 6777203, i64 6381422, i64 7496290, i64 7566949, i64 7105838, i64 36, i64 6387524, i64 6252146, i64 6777192, i64 6250344, i64 7566145, i64 7235433, i64 6447455, i64 6648418, i64 3044214, i64 7239026, i64 6694958, i64 2715253) ]
  %711 = extractvalue { { i64, i64 }, {  } } %710, 0, 0
  %712 = extractvalue { { i64, i64 }, {  } } %710, 0, 1
  store i64 %711, ptr %ds
  store i64 %712, ptr %alloc
  br label %L391
L391:
  %713 = load i64, ptr %alloc
  %714 = add i64 %713, 8
  %715 = inttoptr i64 %714 to ptr addrspace(1)
  store ptr addrspace(1) %715, ptr %173
  %716 = load ptr addrspace(1), ptr %173
  %717 = getelementptr i8, ptr addrspace(1) %716, i64 -8
  store volatile i64 2048, ptr addrspace(1) %717
  %718 = load ptr addrspace(1), ptr %173
  %719 = load ptr addrspace(1), ptr %23
  store ptr addrspace(1) %719, ptr addrspace(1) %718
  %720 = load ptr addrspace(1), ptr %11
  %721 = getelementptr i8, ptr addrspace(1) %720, i64 8
  store ptr addrspace(1) %721, ptr %175
  %722 = load ptr addrspace(1), ptr %175
  %723 = load ptr addrspace(1), ptr addrspace(1) %722
  store ptr addrspace(1) %723, ptr %176
  %724 = load ptr addrspace(1), ptr %173
  %725 = getelementptr i8, ptr addrspace(1) %724, i64 8
  %726 = load ptr addrspace(1), ptr %176
  store ptr addrspace(1) %726, ptr addrspace(1) %725
  %727 = load ptr addrspace(1), ptr %173
  store ptr addrspace(1) %727, ptr %177
  %728 = load i64, ptr %alloc
  %729 = sub i64 %728, 40
  store i64 %729, ptr %alloc
  %730 = load i64, ptr %ds
  %731 = inttoptr i64 %730 to ptr
  %732 = load i64, ptr %731
  %733 = icmp ule i64 %732, %729
  %734 = call  i1 @llvm.expect.i1(i1 %733, i1 1) 
  br i1 %734, label %L393, label %L392
L392:
  %735 = load i64, ptr %ds
  %736 = load i64, ptr %alloc
  %737 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_call_gc"(i64 %735, i64 %736) "statepoint-id"="327681" cold [ "deopt"(i64 1870160737, i64 1, i64 1, i64 5, i64 1, i64 101, i64 0, i64 8, i64 75, i64 0, i64 75, i64 48, i64 6512994, i64 7234923, i64 6565732, i64 7692901, i64 6565735, i64 7496055, i64 6565734, i64 7496055, i64 6840166, i64 6842217, i64 7561519, i64 6777203, i64 6381422, i64 7496290, i64 7566949, i64 7105838, i64 36, i64 6387524, i64 6252146, i64 6777192, i64 6250344, i64 7566145, i64 7235433, i64 6447455, i64 6648418, i64 3044214, i64 7239026, i64 6694958, i64 2715253) ]
  %738 = extractvalue { { i64, i64 }, {  } } %737, 0, 0
  %739 = extractvalue { { i64, i64 }, {  } } %737, 0, 1
  store i64 %738, ptr %ds
  store i64 %739, ptr %alloc
  br label %L393
L393:
  %740 = load i64, ptr %alloc
  %741 = add i64 %740, 8
  %742 = inttoptr i64 %741 to ptr addrspace(1)
  store ptr addrspace(1) %742, ptr %178
  %743 = load ptr addrspace(1), ptr %178
  %744 = getelementptr i8, ptr addrspace(1) %743, i64 -8
  store volatile i64 4096, ptr addrspace(1) %744
  %745 = load ptr addrspace(1), ptr %178
  %746 = load ptr addrspace(1), ptr %22
  store ptr addrspace(1) %746, ptr addrspace(1) %745
  %747 = load ptr addrspace(1), ptr %178
  %748 = getelementptr i8, ptr addrspace(1) %747, i64 8
  %749 = load ptr addrspace(1), ptr %177
  store ptr addrspace(1) %749, ptr addrspace(1) %748
  %750 = load ptr addrspace(1), ptr %178
  %751 = getelementptr i8, ptr addrspace(1) %750, i64 16
  %752 = load ptr addrspace(1), ptr %24
  store ptr addrspace(1) %752, ptr addrspace(1) %751
  %753 = load ptr addrspace(1), ptr %178
  %754 = getelementptr i8, ptr addrspace(1) %753, i64 24
  %755 = load ptr addrspace(1), ptr %25
  store ptr addrspace(1) %755, ptr addrspace(1) %754
  %756 = load ptr addrspace(1), ptr %178
  store ptr addrspace(1) %756, ptr %6
  %757 = load ptr addrspace(1), ptr %6
  %758 = load i64, ptr %ds
  %759 = load i64, ptr %alloc
  %760 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %758, 0, 0
  %761 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %760, i64 %759, 0, 1
  %762 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %761, ptr addrspace(1) %757, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %762
}

@"\01_camlDwarf_high__Assign_abbrevs__Pmakeblock356" = global { i64, i64, i64, i64 } { i64 1, i64 1, i64 1, i64 1 }, section "__DATA,__data", align 8
@"\01_header.camlDwarf_high__Assign_abbrevs__immstring79" = global i64 7164, section "__DATA,__data", align 8
@"\01_camlDwarf_high__Assign_abbrevs__immstring79" = global { [ 43 x i8 ], [ 4 x i8 ], i8 } { [ 43 x i8 ] c"\4d\6f\72\65\20\74\68\61\6e\20\6f\6e\65\20\60\43\6f\6d\70\69\6c\65\5f\75\6e\69\74\27\20\44\49\45\20\69\73\20\70\72\65\73\65\6e\74", [ 4 x i8 ] zeroinitializer, i8 4 }, section "__DATA,__data", align 8
@"\01_camlCamlinternalLazy__force_lazy_block_4_10_code" = external global ptr
@"\01_camlDwarf_high__Proto_die__depth_first_fold_10_24_code" = external global ptr
@"\01_camlDwarf_low__Abbreviation_code__of_int_0_7_code" = external global ptr
@"\01_camlDwarf_low__Abbreviations_table__Pmakeblock3174" = external global ptr
@"\01_camlDwarf_low__Debugging_information_entry__create_null_14" = external global ptr
@"\01_camlDwarf_low__Dwarf_attributes__Pmakeblock6606" = external global ptr
@"\01_camlMisc__fatal_error_2_355_code" = external global ptr
@"\01_caml_apply2" = external global ptr
@"\01_caml_apply3" = external global ptr
@"\01_caml_call_gc" = external global ptr
@"\01_caml_curry2" = external global ptr
@"\01_caml_exn_Not_found" = external global ptr
@"\01_caml_initialize" = external global ptr
@"\01_caml_llvm_call_realloc_stack" = external global ptr
@"\01_caml_obj_tag" = external global ptr
@"\01_caml_reraise_exn" = external global ptr
declare i32 @"\01_caml_llvm_eh_personality"(...)
declare void @llvm.aarch64.oxcaml.pop.trap()
declare void @llvm.aarch64.oxcaml.push.trap(ptr)
declare { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
declare i1 @llvm.expect.i1(i1, i1)

!0 = !{i32 1, !"oxcaml_module", !"Dwarf_high__Assign_abbrevs_reduced"}
!llvm.module.flags = !{!0}
