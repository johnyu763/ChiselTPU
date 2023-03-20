circuit SystArr :
  module SystArr :
    input clock : Clock
    input reset : UInt<1>
    input io_a_in_0 : SInt<32>
    input io_a_in_1 : SInt<32>
    input io_a_in_2 : SInt<32>
    input io_b_in_0_0 : SInt<32>
    input io_b_in_0_1 : SInt<32>
    input io_b_in_0_2 : SInt<32>
    input io_b_in_1_0 : SInt<32>
    input io_b_in_1_1 : SInt<32>
    input io_b_in_1_2 : SInt<32>
    input io_b_in_2_0 : SInt<32>
    input io_b_in_2_1 : SInt<32>
    input io_b_in_2_2 : SInt<32>
    input io_en : UInt<1>
    input io_b_readingin : UInt<1>
    output io_out_0 : SInt<32>
    output io_out_1 : SInt<32>
    output io_out_2 : SInt<32>
    output io_cmp_debug_0_0 : SInt<32>
    output io_cmp_debug_0_1 : SInt<32>
    output io_cmp_debug_0_2 : SInt<32>
    output io_cmp_debug_1_0 : SInt<32>
    output io_cmp_debug_1_1 : SInt<32>
    output io_cmp_debug_1_2 : SInt<32>
    output io_cmp_debug_2_0 : SInt<32>
    output io_cmp_debug_2_1 : SInt<32>
    output io_cmp_debug_2_2 : SInt<32>
    output io_debug_b_regs_0_0 : SInt<32>
    output io_debug_b_regs_0_1 : SInt<32>
    output io_debug_b_regs_0_2 : SInt<32>
    output io_debug_b_regs_1_0 : SInt<32>
    output io_debug_b_regs_1_1 : SInt<32>
    output io_debug_b_regs_1_2 : SInt<32>
    output io_debug_b_regs_2_0 : SInt<32>
    output io_debug_b_regs_2_1 : SInt<32>
    output io_debug_b_regs_2_2 : SInt<32>
    output io_debug_a_regs_0_0 : SInt<32>
    output io_debug_a_regs_0_1 : SInt<32>
    output io_debug_a_regs_0_2 : SInt<32>
    output io_debug_a_regs_1_0 : SInt<32>
    output io_debug_a_regs_1_1 : SInt<32>
    output io_debug_a_regs_1_2 : SInt<32>
    output io_debug_a_regs_2_0 : SInt<32>
    output io_debug_a_regs_2_1 : SInt<32>
    output io_debug_a_regs_2_2 : SInt<32>
    output io_debug_00 : SInt<32>

    reg a_reg_0_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_0_0) @[TPU.scala 360:20]
    reg a_reg_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_0_1) @[TPU.scala 360:20]
    reg a_reg_0_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_0_2) @[TPU.scala 360:20]
    reg a_reg_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_1_0) @[TPU.scala 360:20]
    reg a_reg_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_1_1) @[TPU.scala 360:20]
    reg a_reg_1_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_1_2) @[TPU.scala 360:20]
    reg a_reg_2_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_2_0) @[TPU.scala 360:20]
    reg a_reg_2_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_2_1) @[TPU.scala 360:20]
    reg a_reg_2_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_2_2) @[TPU.scala 360:20]
    reg b_reg_0_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_0_0) @[TPU.scala 361:20]
    reg b_reg_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_0_1) @[TPU.scala 361:20]
    reg b_reg_0_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_0_2) @[TPU.scala 361:20]
    reg b_reg_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_1_0) @[TPU.scala 361:20]
    reg b_reg_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_1_1) @[TPU.scala 361:20]
    reg b_reg_1_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_1_2) @[TPU.scala 361:20]
    reg b_reg_2_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_2_0) @[TPU.scala 361:20]
    reg b_reg_2_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_2_1) @[TPU.scala 361:20]
    reg b_reg_2_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_2_2) @[TPU.scala 361:20]
    reg cms_reg_0_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_0_0) @[TPU.scala 362:22]
    reg cms_reg_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_0_1) @[TPU.scala 362:22]
    reg cms_reg_0_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_0_2) @[TPU.scala 362:22]
    reg cms_reg_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_1_0) @[TPU.scala 362:22]
    reg cms_reg_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_1_1) @[TPU.scala 362:22]
    reg cms_reg_1_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_1_2) @[TPU.scala 362:22]
    reg cms_reg_2_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_2_0) @[TPU.scala 362:22]
    reg cms_reg_2_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_2_1) @[TPU.scala 362:22]
    reg cms_reg_2_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_2_2) @[TPU.scala 362:22]
    node _io_debug_00_T = mul(io_a_in_0, b_reg_0_0) @[TPU.scala 369:33]
    node _GEN_0 = mux(io_b_readingin, io_b_in_0_0, b_reg_0_0) @[TPU.scala 370:27 371:15 373:15]
    node _GEN_1 = mux(io_b_readingin, io_b_in_0_1, b_reg_0_1) @[TPU.scala 370:27 371:15 373:15]
    node _GEN_2 = mux(io_b_readingin, io_b_in_0_2, b_reg_0_2) @[TPU.scala 370:27 371:15 373:15]
    node _GEN_3 = mux(io_b_readingin, io_b_in_1_0, b_reg_1_0) @[TPU.scala 370:27 371:15 373:15]
    node _GEN_4 = mux(io_b_readingin, io_b_in_1_1, b_reg_1_1) @[TPU.scala 370:27 371:15 373:15]
    node _GEN_5 = mux(io_b_readingin, io_b_in_1_2, b_reg_1_2) @[TPU.scala 370:27 371:15 373:15]
    node _GEN_6 = mux(io_b_readingin, io_b_in_2_0, b_reg_2_0) @[TPU.scala 370:27 371:15 373:15]
    node _GEN_7 = mux(io_b_readingin, io_b_in_2_1, b_reg_2_1) @[TPU.scala 370:27 371:15 373:15]
    node _GEN_8 = mux(io_b_readingin, io_b_in_2_2, b_reg_2_2) @[TPU.scala 370:27 371:15 373:15]
    node _cmp_input_0_0_T = mul(io_a_in_0, b_reg_0_0) @[TPU.scala 383:62]
    node _cmp_input_0_1_T = mul(a_reg_0_0, b_reg_0_1) @[TPU.scala 386:68]
    node _cmp_input_0_2_T = mul(a_reg_0_1, b_reg_0_2) @[TPU.scala 386:68]
    node _cmp_input_1_0_T = mul(io_a_in_1, b_reg_1_0) @[TPU.scala 391:63]
    node _cmp_input_1_0_T_1 = add(_cmp_input_1_0_T, cms_reg_0_0) @[TPU.scala 391:84]
    node _cmp_input_1_0_T_2 = tail(_cmp_input_1_0_T_1, 1) @[TPU.scala 391:84]
    node _cmp_input_1_0_T_3 = asSInt(_cmp_input_1_0_T_2) @[TPU.scala 391:84]
    node _cmp_input_1_1_T = mul(a_reg_1_0, b_reg_1_1) @[TPU.scala 394:69]
    node _cmp_input_1_1_T_1 = add(_cmp_input_1_1_T, cms_reg_0_1) @[TPU.scala 394:90]
    node _cmp_input_1_1_T_2 = tail(_cmp_input_1_1_T_1, 1) @[TPU.scala 394:90]
    node _cmp_input_1_1_T_3 = asSInt(_cmp_input_1_1_T_2) @[TPU.scala 394:90]
    node _cmp_input_1_2_T = mul(a_reg_1_1, b_reg_1_2) @[TPU.scala 394:69]
    node _cmp_input_1_2_T_1 = add(_cmp_input_1_2_T, cms_reg_0_2) @[TPU.scala 394:90]
    node _cmp_input_1_2_T_2 = tail(_cmp_input_1_2_T_1, 1) @[TPU.scala 394:90]
    node _cmp_input_1_2_T_3 = asSInt(_cmp_input_1_2_T_2) @[TPU.scala 394:90]
    node _cmp_input_2_0_T = mul(io_a_in_2, b_reg_2_0) @[TPU.scala 391:63]
    node _cmp_input_2_0_T_1 = add(_cmp_input_2_0_T, cms_reg_1_0) @[TPU.scala 391:84]
    node _cmp_input_2_0_T_2 = tail(_cmp_input_2_0_T_1, 1) @[TPU.scala 391:84]
    node _cmp_input_2_0_T_3 = asSInt(_cmp_input_2_0_T_2) @[TPU.scala 391:84]
    node _cmp_input_2_1_T = mul(a_reg_2_0, b_reg_2_1) @[TPU.scala 394:69]
    node _cmp_input_2_1_T_1 = add(_cmp_input_2_1_T, cms_reg_1_1) @[TPU.scala 394:90]
    node _cmp_input_2_1_T_2 = tail(_cmp_input_2_1_T_1, 1) @[TPU.scala 394:90]
    node _cmp_input_2_1_T_3 = asSInt(_cmp_input_2_1_T_2) @[TPU.scala 394:90]
    node _cmp_input_2_2_T = mul(a_reg_2_1, b_reg_2_2) @[TPU.scala 394:69]
    node _cmp_input_2_2_T_1 = add(_cmp_input_2_2_T, cms_reg_1_2) @[TPU.scala 394:90]
    node _cmp_input_2_2_T_2 = tail(_cmp_input_2_2_T_1, 1) @[TPU.scala 394:90]
    node _cmp_input_2_2_T_3 = asSInt(_cmp_input_2_2_T_2) @[TPU.scala 394:90]
    node _WIRE_1_0_0 = asSInt(UInt<32>("h0")) @[TPU.scala 406:{47,47}]
    node _GEN_9 = mux(io_en, cms_reg_0_0, _WIRE_1_0_0) @[TPU.scala 364:16 366:20 406:20]
    node _WIRE_1_0_1 = asSInt(UInt<32>("h0")) @[TPU.scala 406:{47,47}]
    node _GEN_10 = mux(io_en, cms_reg_0_1, _WIRE_1_0_1) @[TPU.scala 364:16 366:20 406:20]
    node _WIRE_1_0_2 = asSInt(UInt<32>("h0")) @[TPU.scala 406:{47,47}]
    node _GEN_11 = mux(io_en, cms_reg_0_2, _WIRE_1_0_2) @[TPU.scala 364:16 366:20 406:20]
    node _WIRE_1_1_0 = asSInt(UInt<32>("h0")) @[TPU.scala 406:{47,47}]
    node _GEN_12 = mux(io_en, cms_reg_1_0, _WIRE_1_1_0) @[TPU.scala 364:16 366:20 406:20]
    node _WIRE_1_1_1 = asSInt(UInt<32>("h0")) @[TPU.scala 406:{47,47}]
    node _GEN_13 = mux(io_en, cms_reg_1_1, _WIRE_1_1_1) @[TPU.scala 364:16 366:20 406:20]
    node _WIRE_1_1_2 = asSInt(UInt<32>("h0")) @[TPU.scala 406:{47,47}]
    node _GEN_14 = mux(io_en, cms_reg_1_2, _WIRE_1_1_2) @[TPU.scala 364:16 366:20 406:20]
    node _WIRE_1_2_0 = asSInt(UInt<32>("h0")) @[TPU.scala 406:{47,47}]
    node _GEN_15 = mux(io_en, cms_reg_2_0, _WIRE_1_2_0) @[TPU.scala 364:16 366:20 406:20]
    node _WIRE_1_2_1 = asSInt(UInt<32>("h0")) @[TPU.scala 406:{47,47}]
    node _GEN_16 = mux(io_en, cms_reg_2_1, _WIRE_1_2_1) @[TPU.scala 364:16 366:20 406:20]
    node _WIRE_1_2_2 = asSInt(UInt<32>("h0")) @[TPU.scala 406:{47,47}]
    node _GEN_17 = mux(io_en, cms_reg_2_2, _WIRE_1_2_2) @[TPU.scala 364:16 366:20 406:20]
    node _WIRE_2_0_0 = asSInt(UInt<32>("h0")) @[TPU.scala 407:{50,50}]
    node _GEN_18 = mux(io_en, b_reg_0_0, _WIRE_2_0_0) @[TPU.scala 364:16 367:23 407:23]
    node _WIRE_2_0_1 = asSInt(UInt<32>("h0")) @[TPU.scala 407:{50,50}]
    node _GEN_19 = mux(io_en, b_reg_0_1, _WIRE_2_0_1) @[TPU.scala 364:16 367:23 407:23]
    node _WIRE_2_0_2 = asSInt(UInt<32>("h0")) @[TPU.scala 407:{50,50}]
    node _GEN_20 = mux(io_en, b_reg_0_2, _WIRE_2_0_2) @[TPU.scala 364:16 367:23 407:23]
    node _WIRE_2_1_0 = asSInt(UInt<32>("h0")) @[TPU.scala 407:{50,50}]
    node _GEN_21 = mux(io_en, b_reg_1_0, _WIRE_2_1_0) @[TPU.scala 364:16 367:23 407:23]
    node _WIRE_2_1_1 = asSInt(UInt<32>("h0")) @[TPU.scala 407:{50,50}]
    node _GEN_22 = mux(io_en, b_reg_1_1, _WIRE_2_1_1) @[TPU.scala 364:16 367:23 407:23]
    node _WIRE_2_1_2 = asSInt(UInt<32>("h0")) @[TPU.scala 407:{50,50}]
    node _GEN_23 = mux(io_en, b_reg_1_2, _WIRE_2_1_2) @[TPU.scala 364:16 367:23 407:23]
    node _WIRE_2_2_0 = asSInt(UInt<32>("h0")) @[TPU.scala 407:{50,50}]
    node _GEN_24 = mux(io_en, b_reg_2_0, _WIRE_2_2_0) @[TPU.scala 364:16 367:23 407:23]
    node _WIRE_2_2_1 = asSInt(UInt<32>("h0")) @[TPU.scala 407:{50,50}]
    node _GEN_25 = mux(io_en, b_reg_2_1, _WIRE_2_2_1) @[TPU.scala 364:16 367:23 407:23]
    node _WIRE_2_2_2 = asSInt(UInt<32>("h0")) @[TPU.scala 407:{50,50}]
    node _GEN_26 = mux(io_en, b_reg_2_2, _WIRE_2_2_2) @[TPU.scala 364:16 367:23 407:23]
    node _WIRE_3_0_0 = asSInt(UInt<32>("h0")) @[TPU.scala 408:{50,50}]
    node _GEN_27 = mux(io_en, a_reg_0_0, _WIRE_3_0_0) @[TPU.scala 364:16 368:23 408:23]
    node _WIRE_3_0_1 = asSInt(UInt<32>("h0")) @[TPU.scala 408:{50,50}]
    node _GEN_28 = mux(io_en, a_reg_0_1, _WIRE_3_0_1) @[TPU.scala 364:16 368:23 408:23]
    node _WIRE_3_0_2 = asSInt(UInt<32>("h0")) @[TPU.scala 408:{50,50}]
    node _GEN_29 = mux(io_en, a_reg_0_2, _WIRE_3_0_2) @[TPU.scala 364:16 368:23 408:23]
    node _WIRE_3_1_0 = asSInt(UInt<32>("h0")) @[TPU.scala 408:{50,50}]
    node _GEN_30 = mux(io_en, a_reg_1_0, _WIRE_3_1_0) @[TPU.scala 364:16 368:23 408:23]
    node _WIRE_3_1_1 = asSInt(UInt<32>("h0")) @[TPU.scala 408:{50,50}]
    node _GEN_31 = mux(io_en, a_reg_1_1, _WIRE_3_1_1) @[TPU.scala 364:16 368:23 408:23]
    node _WIRE_3_1_2 = asSInt(UInt<32>("h0")) @[TPU.scala 408:{50,50}]
    node _GEN_32 = mux(io_en, a_reg_1_2, _WIRE_3_1_2) @[TPU.scala 364:16 368:23 408:23]
    node _WIRE_3_2_0 = asSInt(UInt<32>("h0")) @[TPU.scala 408:{50,50}]
    node _GEN_33 = mux(io_en, a_reg_2_0, _WIRE_3_2_0) @[TPU.scala 364:16 368:23 408:23]
    node _WIRE_3_2_1 = asSInt(UInt<32>("h0")) @[TPU.scala 408:{50,50}]
    node _GEN_34 = mux(io_en, a_reg_2_1, _WIRE_3_2_1) @[TPU.scala 364:16 368:23 408:23]
    node _WIRE_3_2_2 = asSInt(UInt<32>("h0")) @[TPU.scala 408:{50,50}]
    node _GEN_35 = mux(io_en, a_reg_2_2, _WIRE_3_2_2) @[TPU.scala 364:16 368:23 408:23]
    node _GEN_36 = mux(io_en, _io_debug_00_T, asSInt(UInt<1>("h0"))) @[TPU.scala 364:16 369:19 409:19]
    node _GEN_37 = mux(io_en, _GEN_0, b_reg_0_0) @[TPU.scala 364:16 361:20]
    node _GEN_38 = mux(io_en, _GEN_1, b_reg_0_1) @[TPU.scala 364:16 361:20]
    node _GEN_39 = mux(io_en, _GEN_2, b_reg_0_2) @[TPU.scala 364:16 361:20]
    node _GEN_40 = mux(io_en, _GEN_3, b_reg_1_0) @[TPU.scala 364:16 361:20]
    node _GEN_41 = mux(io_en, _GEN_4, b_reg_1_1) @[TPU.scala 364:16 361:20]
    node _GEN_42 = mux(io_en, _GEN_5, b_reg_1_2) @[TPU.scala 364:16 361:20]
    node _GEN_43 = mux(io_en, _GEN_6, b_reg_2_0) @[TPU.scala 364:16 361:20]
    node _GEN_44 = mux(io_en, _GEN_7, b_reg_2_1) @[TPU.scala 364:16 361:20]
    node _GEN_45 = mux(io_en, _GEN_8, b_reg_2_2) @[TPU.scala 364:16 361:20]
    node _WIRE__0 = asSInt(UInt<32>("h0")) @[TPU.scala 405:{35,35}]
    node _GEN_46 = mux(io_en, cms_reg_2_0, _WIRE__0) @[TPU.scala 364:16 376:21 405:14]
    node _WIRE__1 = asSInt(UInt<32>("h0")) @[TPU.scala 405:{35,35}]
    node _GEN_47 = mux(io_en, cms_reg_2_1, _WIRE__1) @[TPU.scala 364:16 376:21 405:14]
    node _WIRE__2 = asSInt(UInt<32>("h0")) @[TPU.scala 405:{35,35}]
    node _GEN_48 = mux(io_en, cms_reg_2_2, _WIRE__2) @[TPU.scala 364:16 376:21 405:14]
    node _GEN_49 = mux(io_en, io_a_in_0, a_reg_0_0) @[TPU.scala 364:16 360:20 384:41]
    node cmp_input_0_0 = asSInt(bits(_cmp_input_0_0_T, 31, 0)) @[TPU.scala 378:27 383:45]
    node _WIRE_4_0_0 = asSInt(UInt<32>("h0")) @[TPU.scala 410:{42,42}]
    node _GEN_50 = mux(io_en, cmp_input_0_0, _WIRE_4_0_0) @[TPU.scala 364:16 398:23 410:15]
    node cmp_input_0_1 = asSInt(bits(_cmp_input_0_1_T, 31, 0)) @[TPU.scala 378:27 386:45]
    node _WIRE_4_0_1 = asSInt(UInt<32>("h0")) @[TPU.scala 410:{42,42}]
    node _GEN_51 = mux(io_en, cmp_input_0_1, _WIRE_4_0_1) @[TPU.scala 364:16 398:23 410:15]
    node cmp_input_0_2 = asSInt(bits(_cmp_input_0_2_T, 31, 0)) @[TPU.scala 378:27 386:45]
    node _WIRE_4_0_2 = asSInt(UInt<32>("h0")) @[TPU.scala 410:{42,42}]
    node _GEN_52 = mux(io_en, cmp_input_0_2, _WIRE_4_0_2) @[TPU.scala 364:16 398:23 410:15]
    node cmp_input_1_0 = asSInt(bits(_cmp_input_1_0_T_3, 31, 0)) @[TPU.scala 378:27 391:45]
    node _WIRE_4_1_0 = asSInt(UInt<32>("h0")) @[TPU.scala 410:{42,42}]
    node _GEN_53 = mux(io_en, cmp_input_1_0, _WIRE_4_1_0) @[TPU.scala 364:16 398:23 410:15]
    node cmp_input_1_1 = asSInt(bits(_cmp_input_1_1_T_3, 31, 0)) @[TPU.scala 378:27 394:45]
    node _WIRE_4_1_1 = asSInt(UInt<32>("h0")) @[TPU.scala 410:{42,42}]
    node _GEN_54 = mux(io_en, cmp_input_1_1, _WIRE_4_1_1) @[TPU.scala 364:16 398:23 410:15]
    node cmp_input_1_2 = asSInt(bits(_cmp_input_1_2_T_3, 31, 0)) @[TPU.scala 378:27 394:45]
    node _WIRE_4_1_2 = asSInt(UInt<32>("h0")) @[TPU.scala 410:{42,42}]
    node _GEN_55 = mux(io_en, cmp_input_1_2, _WIRE_4_1_2) @[TPU.scala 364:16 398:23 410:15]
    node cmp_input_2_0 = asSInt(bits(_cmp_input_2_0_T_3, 31, 0)) @[TPU.scala 378:27 391:45]
    node _WIRE_4_2_0 = asSInt(UInt<32>("h0")) @[TPU.scala 410:{42,42}]
    node _GEN_56 = mux(io_en, cmp_input_2_0, _WIRE_4_2_0) @[TPU.scala 364:16 398:23 410:15]
    node cmp_input_2_1 = asSInt(bits(_cmp_input_2_1_T_3, 31, 0)) @[TPU.scala 378:27 394:45]
    node _WIRE_4_2_1 = asSInt(UInt<32>("h0")) @[TPU.scala 410:{42,42}]
    node _GEN_57 = mux(io_en, cmp_input_2_1, _WIRE_4_2_1) @[TPU.scala 364:16 398:23 410:15]
    node cmp_input_2_2 = asSInt(bits(_cmp_input_2_2_T_3, 31, 0)) @[TPU.scala 378:27 394:45]
    node _WIRE_4_2_2 = asSInt(UInt<32>("h0")) @[TPU.scala 410:{42,42}]
    node _GEN_58 = mux(io_en, cmp_input_2_2, _WIRE_4_2_2) @[TPU.scala 364:16 398:23 410:15]
    node _GEN_59 = mux(io_en, a_reg_0_0, a_reg_0_1) @[TPU.scala 364:16 360:20 387:41]
    node _GEN_60 = mux(io_en, a_reg_0_1, a_reg_0_2) @[TPU.scala 364:16 360:20 387:41]
    node _GEN_61 = mux(io_en, io_a_in_1, a_reg_1_0) @[TPU.scala 364:16 360:20 392:41]
    node _GEN_62 = mux(io_en, a_reg_1_0, a_reg_1_1) @[TPU.scala 364:16 360:20 395:41]
    node _GEN_63 = mux(io_en, a_reg_1_1, a_reg_1_2) @[TPU.scala 364:16 360:20 395:41]
    node _GEN_64 = mux(io_en, io_a_in_2, a_reg_2_0) @[TPU.scala 364:16 360:20 392:41]
    node _GEN_65 = mux(io_en, a_reg_2_0, a_reg_2_1) @[TPU.scala 364:16 360:20 395:41]
    node _GEN_66 = mux(io_en, a_reg_2_1, a_reg_2_2) @[TPU.scala 364:16 360:20 395:41]
    io_out_0 <= _GEN_46
    io_out_1 <= _GEN_47
    io_out_2 <= _GEN_48
    io_cmp_debug_0_0 <= _GEN_9
    io_cmp_debug_0_1 <= _GEN_10
    io_cmp_debug_0_2 <= _GEN_11
    io_cmp_debug_1_0 <= _GEN_12
    io_cmp_debug_1_1 <= _GEN_13
    io_cmp_debug_1_2 <= _GEN_14
    io_cmp_debug_2_0 <= _GEN_15
    io_cmp_debug_2_1 <= _GEN_16
    io_cmp_debug_2_2 <= _GEN_17
    io_debug_b_regs_0_0 <= _GEN_18
    io_debug_b_regs_0_1 <= _GEN_19
    io_debug_b_regs_0_2 <= _GEN_20
    io_debug_b_regs_1_0 <= _GEN_21
    io_debug_b_regs_1_1 <= _GEN_22
    io_debug_b_regs_1_2 <= _GEN_23
    io_debug_b_regs_2_0 <= _GEN_24
    io_debug_b_regs_2_1 <= _GEN_25
    io_debug_b_regs_2_2 <= _GEN_26
    io_debug_a_regs_0_0 <= _GEN_27
    io_debug_a_regs_0_1 <= _GEN_28
    io_debug_a_regs_0_2 <= _GEN_29
    io_debug_a_regs_1_0 <= _GEN_30
    io_debug_a_regs_1_1 <= _GEN_31
    io_debug_a_regs_1_2 <= _GEN_32
    io_debug_a_regs_2_0 <= _GEN_33
    io_debug_a_regs_2_1 <= _GEN_34
    io_debug_a_regs_2_2 <= _GEN_35
    io_debug_00 <= asSInt(bits(_GEN_36, 31, 0))
    a_reg_0_0 <= _GEN_49
    a_reg_0_1 <= _GEN_59
    a_reg_0_2 <= _GEN_60
    a_reg_1_0 <= _GEN_61
    a_reg_1_1 <= _GEN_62
    a_reg_1_2 <= _GEN_63
    a_reg_2_0 <= _GEN_64
    a_reg_2_1 <= _GEN_65
    a_reg_2_2 <= _GEN_66
    b_reg_0_0 <= _GEN_37
    b_reg_0_1 <= _GEN_38
    b_reg_0_2 <= _GEN_39
    b_reg_1_0 <= _GEN_40
    b_reg_1_1 <= _GEN_41
    b_reg_1_2 <= _GEN_42
    b_reg_2_0 <= _GEN_43
    b_reg_2_1 <= _GEN_44
    b_reg_2_2 <= _GEN_45
    cms_reg_0_0 <= _GEN_50
    cms_reg_0_1 <= _GEN_51
    cms_reg_0_2 <= _GEN_52
    cms_reg_1_0 <= _GEN_53
    cms_reg_1_1 <= _GEN_54
    cms_reg_1_2 <= _GEN_55
    cms_reg_2_0 <= _GEN_56
    cms_reg_2_1 <= _GEN_57
    cms_reg_2_2 <= _GEN_58
