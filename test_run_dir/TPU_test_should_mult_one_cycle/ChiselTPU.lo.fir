circuit ChiselTPU :
  module ActReg :
    input clock : Clock
    input reset : UInt<1>
    input io_index : UInt<3>
    input io_a_0_0 : SInt<32>
    input io_a_0_1 : SInt<32>
    input io_a_1_0 : SInt<32>
    input io_a_1_1 : SInt<32>
    output io_a_out_0 : SInt<32>
    output io_a_out_1 : SInt<32>

    node _T = lt(io_index, UInt<2>("h3")) @[TPU.scala 360:19]
    node _T_1 = asSInt(io_index) @[TPU.scala 362:35]
    node _T_2 = sub(asSInt(UInt<2>("h1")), _T_1) @[TPU.scala 362:25]
    node _T_3 = tail(_T_2, 1) @[TPU.scala 362:25]
    node _T_4 = asSInt(_T_3) @[TPU.scala 362:25]
    node _T_5 = leq(_T_4, asSInt(UInt<1>("h0"))) @[TPU.scala 362:42]
    node _T_6 = asSInt(io_index) @[TPU.scala 362:77]
    node _T_7 = sub(asSInt(UInt<3>("h3")), _T_6) @[TPU.scala 362:67]
    node _T_8 = tail(_T_7, 1) @[TPU.scala 362:67]
    node _T_9 = asSInt(_T_8) @[TPU.scala 362:67]
    node _T_10 = gt(_T_9, asSInt(UInt<1>("h0"))) @[TPU.scala 362:84]
    node _T_11 = and(_T_5, _T_10) @[TPU.scala 362:49]
    node _io_a_out_0_T = sub(UInt<2>("h2"), io_index) @[TPU.scala 363:55]
    node _io_a_out_0_T_1 = tail(_io_a_out_0_T, 1) @[TPU.scala 363:55]
    node _io_a_out_0_T_2 = bits(_io_a_out_0_T_1, 0, 0)
    node _GEN_0 = validif(eq(UInt<1>("h0"), _io_a_out_0_T_2), io_a_0_0) @[TPU.scala 363:{25,25}]
    node _GEN_1 = mux(eq(UInt<1>("h1"), _io_a_out_0_T_2), io_a_1_0, _GEN_0) @[TPU.scala 363:{25,25}]
    node _io_a_io_a_out_0_T_2_0 = _GEN_1 @[TPU.scala 363:25]
    node _GEN_2 = mux(_T_11, _io_a_io_a_out_0_T_2_0, asSInt(UInt<1>("h0"))) @[TPU.scala 362:90 363:25 366:25]
    node _T_12 = asSInt(io_index) @[TPU.scala 362:35]
    node _T_13 = sub(asSInt(UInt<1>("h0")), _T_12) @[TPU.scala 362:25]
    node _T_14 = tail(_T_13, 1) @[TPU.scala 362:25]
    node _T_15 = asSInt(_T_14) @[TPU.scala 362:25]
    node _T_16 = leq(_T_15, asSInt(UInt<1>("h0"))) @[TPU.scala 362:42]
    node _T_17 = asSInt(io_index) @[TPU.scala 362:77]
    node _T_18 = sub(asSInt(UInt<3>("h2")), _T_17) @[TPU.scala 362:67]
    node _T_19 = tail(_T_18, 1) @[TPU.scala 362:67]
    node _T_20 = asSInt(_T_19) @[TPU.scala 362:67]
    node _T_21 = gt(_T_20, asSInt(UInt<1>("h0"))) @[TPU.scala 362:84]
    node _T_22 = and(_T_16, _T_21) @[TPU.scala 362:49]
    node _io_a_out_1_T = sub(UInt<1>("h1"), io_index) @[TPU.scala 363:55]
    node _io_a_out_1_T_1 = tail(_io_a_out_1_T, 1) @[TPU.scala 363:55]
    node _io_a_out_1_T_2 = bits(_io_a_out_1_T_1, 0, 0)
    node _GEN_3 = validif(eq(UInt<1>("h0"), _io_a_out_1_T_2), io_a_0_1) @[TPU.scala 363:{25,25}]
    node _GEN_4 = mux(eq(UInt<1>("h1"), _io_a_out_1_T_2), io_a_1_1, _GEN_3) @[TPU.scala 363:{25,25}]
    node _io_a_io_a_out_1_T_2_1 = _GEN_4 @[TPU.scala 363:25]
    node _GEN_5 = mux(_T_22, _io_a_io_a_out_1_T_2_1, asSInt(UInt<1>("h0"))) @[TPU.scala 362:90 363:25 366:25]
    node _GEN_6 = mux(_T, _GEN_2, asSInt(UInt<1>("h0"))) @[TPU.scala 360:35 372:23]
    node _GEN_7 = mux(_T, _GEN_5, asSInt(UInt<1>("h0"))) @[TPU.scala 360:35 372:23]
    io_a_out_0 <= _GEN_6
    io_a_out_1 <= _GEN_7

  module SystArr :
    input clock : Clock
    input reset : UInt<1>
    input io_a_in_0 : SInt<32>
    input io_a_in_1 : SInt<32>
    input io_b_in_0_0 : SInt<32>
    input io_b_in_0_1 : SInt<32>
    input io_b_in_1_0 : SInt<32>
    input io_b_in_1_1 : SInt<32>
    input io_b_readingin : UInt<1>
    output io_out_0 : SInt<32>
    output io_out_1 : SInt<32>
    output io_cmp_debug_0_0 : SInt<32>
    output io_cmp_debug_0_1 : SInt<32>
    output io_cmp_debug_1_0 : SInt<32>
    output io_cmp_debug_1_1 : SInt<32>
    output io_debug_b_regs_0_0 : SInt<32>
    output io_debug_b_regs_0_1 : SInt<32>
    output io_debug_b_regs_1_0 : SInt<32>
    output io_debug_b_regs_1_1 : SInt<32>
    output io_debug_a_regs_0_0 : SInt<32>
    output io_debug_a_regs_0_1 : SInt<32>
    output io_debug_a_regs_1_0 : SInt<32>
    output io_debug_a_regs_1_1 : SInt<32>
    output io_debug_00 : SInt<32>

    reg a_reg_0_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_0_0) @[TPU.scala 391:20]
    reg a_reg_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_0_1) @[TPU.scala 391:20]
    reg a_reg_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_1_0) @[TPU.scala 391:20]
    reg a_reg_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_1_1) @[TPU.scala 391:20]
    reg b_reg_0_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_0_0) @[TPU.scala 392:20]
    reg b_reg_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_0_1) @[TPU.scala 392:20]
    reg b_reg_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_1_0) @[TPU.scala 392:20]
    reg b_reg_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_1_1) @[TPU.scala 392:20]
    reg cms_reg_0_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_0_0) @[TPU.scala 393:22]
    reg cms_reg_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_0_1) @[TPU.scala 393:22]
    reg cms_reg_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_1_0) @[TPU.scala 393:22]
    reg cms_reg_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_1_1) @[TPU.scala 393:22]
    node _io_debug_00_T = mul(io_a_in_0, b_reg_0_0) @[TPU.scala 397:31]
    node _GEN_0 = mux(io_b_readingin, io_b_in_0_0, b_reg_0_0) @[TPU.scala 398:25 399:13 401:13]
    node _GEN_1 = mux(io_b_readingin, io_b_in_0_1, b_reg_0_1) @[TPU.scala 398:25 399:13 401:13]
    node _GEN_2 = mux(io_b_readingin, io_b_in_1_0, b_reg_1_0) @[TPU.scala 398:25 399:13 401:13]
    node _GEN_3 = mux(io_b_readingin, io_b_in_1_1, b_reg_1_1) @[TPU.scala 398:25 399:13 401:13]
    node _cmp_input_0_0_T = mul(io_a_in_0, b_reg_0_0) @[TPU.scala 411:60]
    node _cmp_input_0_1_T = mul(a_reg_0_0, b_reg_0_1) @[TPU.scala 414:66]
    node _cmp_input_1_0_T = mul(io_a_in_1, b_reg_1_0) @[TPU.scala 419:61]
    node _cmp_input_1_0_T_1 = add(_cmp_input_1_0_T, cms_reg_0_0) @[TPU.scala 419:82]
    node _cmp_input_1_0_T_2 = tail(_cmp_input_1_0_T_1, 1) @[TPU.scala 419:82]
    node _cmp_input_1_0_T_3 = asSInt(_cmp_input_1_0_T_2) @[TPU.scala 419:82]
    node _cmp_input_1_1_T = mul(a_reg_1_0, b_reg_1_1) @[TPU.scala 422:67]
    node _cmp_input_1_1_T_1 = add(_cmp_input_1_1_T, cms_reg_0_1) @[TPU.scala 422:88]
    node _cmp_input_1_1_T_2 = tail(_cmp_input_1_1_T_1, 1) @[TPU.scala 422:88]
    node _cmp_input_1_1_T_3 = asSInt(_cmp_input_1_1_T_2) @[TPU.scala 422:88]
    node _T = asUInt(reset) @[TPU.scala 432:11]
    node _T_1 = eq(_T, UInt<1>("h0")) @[TPU.scala 432:11]
    node _T_2 = asUInt(reset) @[TPU.scala 434:13]
    node _T_3 = eq(_T_2, UInt<1>("h0")) @[TPU.scala 434:13]
    node _T_4 = asUInt(reset) @[TPU.scala 434:13]
    node _T_5 = eq(_T_4, UInt<1>("h0")) @[TPU.scala 434:13]
    node _T_6 = asUInt(reset) @[TPU.scala 436:11]
    node _T_7 = eq(_T_6, UInt<1>("h0")) @[TPU.scala 436:11]
    node _T_8 = asUInt(reset) @[TPU.scala 437:11]
    node _T_9 = eq(_T_8, UInt<1>("h0")) @[TPU.scala 437:11]
    node _T_10 = asUInt(reset) @[TPU.scala 439:13]
    node _T_11 = eq(_T_10, UInt<1>("h0")) @[TPU.scala 439:13]
    node _T_12 = asUInt(reset) @[TPU.scala 439:13]
    node _T_13 = eq(_T_12, UInt<1>("h0")) @[TPU.scala 439:13]
    node _T_14 = asUInt(reset) @[TPU.scala 441:11]
    node _T_15 = eq(_T_14, UInt<1>("h0")) @[TPU.scala 441:11]
    node _T_16 = asUInt(reset) @[TPU.scala 443:13]
    node _T_17 = eq(_T_16, UInt<1>("h0")) @[TPU.scala 443:13]
    node _T_18 = asUInt(reset) @[TPU.scala 443:13]
    node _T_19 = eq(_T_18, UInt<1>("h0")) @[TPU.scala 443:13]
    node _T_20 = asUInt(reset) @[TPU.scala 445:11]
    node _T_21 = eq(_T_20, UInt<1>("h0")) @[TPU.scala 445:11]
    node _T_22 = asUInt(reset) @[TPU.scala 447:13]
    node _T_23 = eq(_T_22, UInt<1>("h0")) @[TPU.scala 447:13]
    node _T_24 = asUInt(reset) @[TPU.scala 447:13]
    node _T_25 = eq(_T_24, UInt<1>("h0")) @[TPU.scala 447:13]
    node _T_26 = asUInt(reset) @[TPU.scala 449:11]
    node _T_27 = eq(_T_26, UInt<1>("h0")) @[TPU.scala 449:11]
    node _T_28 = asUInt(reset) @[TPU.scala 451:13]
    node _T_29 = eq(_T_28, UInt<1>("h0")) @[TPU.scala 451:13]
    node _T_30 = asUInt(reset) @[TPU.scala 451:13]
    node _T_31 = eq(_T_30, UInt<1>("h0")) @[TPU.scala 451:13]
    node cmp_input_0_0 = asSInt(bits(_cmp_input_0_0_T, 31, 0)) @[TPU.scala 406:25 411:43]
    node cmp_input_0_1 = asSInt(bits(_cmp_input_0_1_T, 31, 0)) @[TPU.scala 406:25 414:43]
    node cmp_input_1_0 = asSInt(bits(_cmp_input_1_0_T_3, 31, 0)) @[TPU.scala 406:25 419:43]
    node cmp_input_1_1 = asSInt(bits(_cmp_input_1_1_T_3, 31, 0)) @[TPU.scala 406:25 422:43]
    io_out_0 <= cms_reg_1_0 @[TPU.scala 404:19]
    io_out_1 <= cms_reg_1_1 @[TPU.scala 404:19]
    io_cmp_debug_0_0 <= cms_reg_0_0 @[TPU.scala 394:18]
    io_cmp_debug_0_1 <= cms_reg_0_1 @[TPU.scala 394:18]
    io_cmp_debug_1_0 <= cms_reg_1_0 @[TPU.scala 394:18]
    io_cmp_debug_1_1 <= cms_reg_1_1 @[TPU.scala 394:18]
    io_debug_b_regs_0_0 <= b_reg_0_0 @[TPU.scala 395:21]
    io_debug_b_regs_0_1 <= b_reg_0_1 @[TPU.scala 395:21]
    io_debug_b_regs_1_0 <= b_reg_1_0 @[TPU.scala 395:21]
    io_debug_b_regs_1_1 <= b_reg_1_1 @[TPU.scala 395:21]
    io_debug_a_regs_0_0 <= a_reg_0_0 @[TPU.scala 396:21]
    io_debug_a_regs_0_1 <= a_reg_0_1 @[TPU.scala 396:21]
    io_debug_a_regs_1_0 <= a_reg_1_0 @[TPU.scala 396:21]
    io_debug_a_regs_1_1 <= a_reg_1_1 @[TPU.scala 396:21]
    io_debug_00 <= asSInt(bits(_io_debug_00_T, 31, 0)) @[TPU.scala 397:17]
    a_reg_0_0 <= io_a_in_0 @[TPU.scala 412:39]
    a_reg_0_1 <= a_reg_0_0 @[TPU.scala 415:39]
    a_reg_1_0 <= io_a_in_1 @[TPU.scala 420:39]
    a_reg_1_1 <= a_reg_1_0 @[TPU.scala 423:39]
    b_reg_0_0 <= _GEN_0
    b_reg_0_1 <= _GEN_1
    b_reg_1_0 <= _GEN_2
    b_reg_1_1 <= _GEN_3
    cms_reg_0_0 <= cmp_input_0_0 @[TPU.scala 426:21]
    cms_reg_0_1 <= cmp_input_0_1 @[TPU.scala 426:21]
    cms_reg_1_0 <= cmp_input_1_0 @[TPU.scala 426:21]
    cms_reg_1_1 <= cmp_input_1_1 @[TPU.scala 426:21]
    printf(clock, and(and(UInt<1>("h1"), _T_1), UInt<1>("h1")), "\nMY A_IN:\n") : printf @[TPU.scala 432:11]
    printf(clock, and(and(UInt<1>("h1"), _T_3), UInt<1>("h1")), "%d ", io_a_in_0) : printf_1 @[TPU.scala 434:13]
    printf(clock, and(and(UInt<1>("h1"), _T_5), UInt<1>("h1")), "%d ", io_a_in_1) : printf_2 @[TPU.scala 434:13]
    printf(clock, and(and(UInt<1>("h1"), _T_7), UInt<1>("h1")), "\n\n") : printf_3 @[TPU.scala 436:11]
    printf(clock, and(and(UInt<1>("h1"), _T_9), UInt<1>("h1")), "MY B_IN:\n") : printf_4 @[TPU.scala 437:11]
    printf(clock, and(and(UInt<1>("h1"), _T_11), UInt<1>("h1")), "Vec(%d, %d)\n", io_b_in_0_0, io_b_in_0_1) : printf_5 @[TPU.scala 439:13]
    printf(clock, and(and(UInt<1>("h1"), _T_13), UInt<1>("h1")), "Vec(%d, %d)\n", io_b_in_1_0, io_b_in_1_1) : printf_6 @[TPU.scala 439:13]
    printf(clock, and(and(UInt<1>("h1"), _T_15), UInt<1>("h1")), "MY A_REG:\n") : printf_7 @[TPU.scala 441:11]
    printf(clock, and(and(UInt<1>("h1"), _T_17), UInt<1>("h1")), "Vec(%d, %d)\n", a_reg_0_0, a_reg_0_1) : printf_8 @[TPU.scala 443:13]
    printf(clock, and(and(UInt<1>("h1"), _T_19), UInt<1>("h1")), "Vec(%d, %d)\n", a_reg_1_0, a_reg_1_1) : printf_9 @[TPU.scala 443:13]
    printf(clock, and(and(UInt<1>("h1"), _T_21), UInt<1>("h1")), "\nMY CSM:\n") : printf_10 @[TPU.scala 445:11]
    printf(clock, and(and(UInt<1>("h1"), _T_23), UInt<1>("h1")), "Vec(%d, %d)\n", cms_reg_0_0, cms_reg_0_1) : printf_11 @[TPU.scala 447:13]
    printf(clock, and(and(UInt<1>("h1"), _T_25), UInt<1>("h1")), "Vec(%d, %d)\n", cms_reg_1_0, cms_reg_1_1) : printf_12 @[TPU.scala 447:13]
    printf(clock, and(and(UInt<1>("h1"), _T_27), UInt<1>("h1")), "MY SYST OUT:\n") : printf_13 @[TPU.scala 449:11]
    printf(clock, and(and(UInt<1>("h1"), _T_29), UInt<1>("h1")), "%d\n", io_out_0) : printf_14 @[TPU.scala 451:13]
    printf(clock, and(and(UInt<1>("h1"), _T_31), UInt<1>("h1")), "%d\n", io_out_1) : printf_15 @[TPU.scala 451:13]

  module ChiselTPU :
    input clock : Clock
    input reset : UInt<1>
    output io_a_ready : UInt<1>
    input io_a_valid : UInt<1>
    input io_a_bits_0_0 : SInt<32>
    input io_a_bits_0_1 : SInt<32>
    input io_a_bits_1_0 : SInt<32>
    input io_a_bits_1_1 : SInt<32>
    output io_b_ready : UInt<1>
    input io_b_valid : UInt<1>
    input io_b_bits_0_0 : SInt<32>
    input io_b_bits_0_1 : SInt<32>
    input io_b_bits_1_0 : SInt<32>
    input io_b_bits_1_1 : SInt<32>
    output io_out_0_0 : SInt<32>
    output io_out_0_1 : SInt<32>
    output io_out_1_0 : SInt<32>
    output io_out_1_1 : SInt<32>

    inst actReg of ActReg @[TPU.scala 48:22]
    inst systArr of SystArr @[TPU.scala 49:23]
    reg state : UInt<3>, clock with :
      reset => (UInt<1>("h0"), state) @[TPU.scala 44:22]
    reg cycle : UInt<3>, clock with :
      reset => (UInt<1>("h0"), cycle) @[Counter.scala 61:40]
    node wrap_wrap = eq(cycle, UInt<3>("h6")) @[Counter.scala 73:24]
    node _wrap_value_T = add(cycle, UInt<1>("h1")) @[Counter.scala 77:24]
    node _wrap_value_T_1 = tail(_wrap_value_T, 1) @[Counter.scala 77:24]
    node _GEN_0 = mux(wrap_wrap, UInt<1>("h0"), _wrap_value_T_1) @[Counter.scala 77:15 87:{20,28}]
    node _T_1 = eq(state, UInt<3>("h0")) @[TPU.scala 162:14]
    node _T_3 = eq(state, UInt<3>("h1")) @[TPU.scala 172:19]
    node _T_4 = and(io_b_valid, io_b_ready) @[TPU.scala 173:23]
    node _GEN_60 = mux(_T_4, UInt<1>("h1"), UInt<1>("h0")) @[TPU.scala 173:37 178:32 85:26]
    node _T_6 = eq(state, UInt<3>("h2")) @[TPU.scala 191:19]
    node _T_13 = eq(state, UInt<3>("h3")) @[TPU.scala 212:19]
    node _GEN_186 = mux(_T_13, UInt<1>("h1"), UInt<1>("h0")) @[TPU.scala 212:32 213:17 80:15]
    node _GEN_205 = mux(_T_6, UInt<1>("h0"), _GEN_186) @[TPU.scala 191:29 80:15]
    node _GEN_222 = mux(_T_3, _GEN_60, _GEN_205) @[TPU.scala 172:28]
    node _GEN_260 = mux(_T_1, UInt<1>("h0"), _GEN_222) @[TPU.scala 162:23 80:15]
    node counterFlag = _GEN_260 @[TPU.scala 45:25]
    node _GEN_1 = mux(counterFlag, _GEN_0, cycle) @[Counter.scala 118:16 61:40]
    node _GEN_2 = mux(counterFlag, wrap_wrap, UInt<1>("h0")) @[Counter.scala 118:{16,23}]
    reg myOut_0_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), myOut_0_0) @[TPU.scala 50:22]
    reg myOut_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), myOut_0_1) @[TPU.scala 50:22]
    reg myOut_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), myOut_1_0) @[TPU.scala 50:22]
    reg myOut_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), myOut_1_1) @[TPU.scala 50:22]
    reg a_ready : UInt<1>, clock with :
      reset => (UInt<1>("h0"), a_ready) @[TPU.scala 51:24]
    reg b_ready : UInt<1>, clock with :
      reset => (UInt<1>("h0"), b_ready) @[TPU.scala 52:24]
    reg paddedA_0_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), paddedA_0_0) @[TPU.scala 66:24]
    reg paddedA_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), paddedA_0_1) @[TPU.scala 66:24]
    reg paddedA_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), paddedA_1_0) @[TPU.scala 66:24]
    reg paddedA_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), paddedA_1_1) @[TPU.scala 66:24]
    reg paddedB_0_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), paddedB_0_0) @[TPU.scala 67:24]
    reg paddedB_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), paddedB_0_1) @[TPU.scala 67:24]
    reg paddedB_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), paddedB_1_0) @[TPU.scala 67:24]
    reg paddedB_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), paddedB_1_1) @[TPU.scala 67:24]
    reg paddedOut_0_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), paddedOut_0_0) @[TPU.scala 68:26]
    reg paddedOut_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), paddedOut_0_1) @[TPU.scala 68:26]
    reg paddedOut_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), paddedOut_1_0) @[TPU.scala 68:26]
    reg paddedOut_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), paddedOut_1_1) @[TPU.scala 68:26]
    reg slicedA_0_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), slicedA_0_0) @[TPU.scala 69:24]
    reg slicedA_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), slicedA_0_1) @[TPU.scala 69:24]
    reg slicedA_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), slicedA_1_0) @[TPU.scala 69:24]
    reg slicedA_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), slicedA_1_1) @[TPU.scala 69:24]
    reg slicedB_0_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), slicedB_0_0) @[TPU.scala 70:24]
    reg slicedB_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), slicedB_0_1) @[TPU.scala 70:24]
    reg slicedB_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), slicedB_1_0) @[TPU.scala 70:24]
    reg slicedB_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), slicedB_1_1) @[TPU.scala 70:24]
    reg slicedOut_0_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), slicedOut_0_0) @[TPU.scala 71:26]
    reg slicedOut_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), slicedOut_0_1) @[TPU.scala 71:26]
    reg slicedOut_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), slicedOut_1_0) @[TPU.scala 71:26]
    reg slicedOut_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), slicedOut_1_1) @[TPU.scala 71:26]
    node _T = eq(state, UInt<3>("h2")) @[TPU.scala 73:47]
    node _GEN_3 = mux(_T, UInt<1>("h1"), UInt<1>("h0")) @[Counter.scala 118:{16,23}]
    node _actReg_io_index_T = sub(UInt<2>("h3"), cycle) @[TPU.scala 82:53]
    node _actReg_io_index_T_1 = tail(_actReg_io_index_T, 1) @[TPU.scala 82:53]
    reg act_in_0_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), act_in_0_0) @[TPU.scala 84:23]
    reg act_in_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), act_in_0_1) @[TPU.scala 84:23]
    reg act_in_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), act_in_1_0) @[TPU.scala 84:23]
    reg act_in_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), act_in_1_1) @[TPU.scala 84:23]
    node _T_5 = eq(io_b_ready, UInt<1>("h0")) @[TPU.scala 183:17]
    node _GEN_51 = mux(_T_5, UInt<1>("h0"), UInt<1>("h0")) @[TPU.scala 183:29 185:20]
    node _GEN_64 = mux(_T_4, UInt<1>("h0"), _GEN_51) @[TPU.scala 173:37]
    node _GEN_225 = mux(_T_3, _GEN_64, UInt<1>("h0")) @[TPU.scala 172:28]
    node _GEN_261 = mux(_T_1, UInt<1>("h0"), _GEN_225) @[TPU.scala 162:23]
    node sliceCycle = _GEN_261
    node _boundK_T = rem(sliceCycle, UInt<1>("h1")) @[TPU.scala 125:47]
    node _boundK_T_1 = rem(_boundK_T, UInt<1>("h1")) @[TPU.scala 125:74]
    node boundK = mul(UInt<2>("h2"), _boundK_T_1) @[TPU.scala 125:32]
    node _boundM_T = rem(sliceCycle, UInt<1>("h1")) @[TPU.scala 126:47]
    node _boundM_T_1 = div(_boundM_T, UInt<1>("h1")) @[TPU.scala 126:74]
    node boundM = mul(UInt<2>("h2"), _boundM_T_1) @[TPU.scala 126:32]
    node _boundN_T = div(sliceCycle, UInt<1>("h1")) @[TPU.scala 127:46]
    node boundN = mul(UInt<2>("h2"), _boundN_T) @[TPU.scala 127:32]
    node _slicedA_0_0_T = add(boundM, UInt<1>("h0")) @[TPU.scala 140:40]
    node _slicedA_0_0_T_1 = tail(_slicedA_0_0_T, 1) @[TPU.scala 140:40]
    node _slicedA_0_0_T_2 = or(_slicedA_0_0_T_1, UInt<1>("h0"))
    node _slicedA_0_0_T_3 = bits(_slicedA_0_0_T_2, 0, 0)
    node _slicedA_0_0_T_4 = add(boundK, UInt<1>("h0")) @[TPU.scala 140:52]
    node _slicedA_0_0_T_5 = tail(_slicedA_0_0_T_4, 1) @[TPU.scala 140:52]
    node _slicedA_0_0_T_6 = or(_slicedA_0_0_T_5, UInt<1>("h0"))
    node _slicedA_0_0_T_7 = bits(_slicedA_0_0_T_6, 0, 0)
    node _GEN_4 = validif(and(eq(UInt<1>("h0"), _slicedA_0_0_T_3), eq(UInt<1>("h0"), _slicedA_0_0_T_7)), paddedA_0_0) @[TPU.scala 140:{23,23}]
    node _GEN_5 = mux(and(eq(UInt<1>("h0"), _slicedA_0_0_T_3), eq(UInt<1>("h1"), _slicedA_0_0_T_7)), paddedA_0_1, _GEN_4) @[TPU.scala 140:{23,23}]
    node _GEN_6 = mux(and(eq(UInt<1>("h1"), _slicedA_0_0_T_3), eq(UInt<1>("h0"), _slicedA_0_0_T_7)), paddedA_1_0, _GEN_5) @[TPU.scala 140:{23,23}]
    node _GEN_7 = mux(and(eq(UInt<1>("h1"), _slicedA_0_0_T_3), eq(UInt<1>("h1"), _slicedA_0_0_T_7)), paddedA_1_1, _GEN_6) @[TPU.scala 140:{23,23}]
    node _slicedA_1_0_T = add(boundM, UInt<1>("h1")) @[TPU.scala 140:40]
    node _slicedA_1_0_T_1 = tail(_slicedA_1_0_T, 1) @[TPU.scala 140:40]
    node _slicedA_1_0_T_2 = or(_slicedA_1_0_T_1, UInt<1>("h0"))
    node _slicedA_1_0_T_3 = bits(_slicedA_1_0_T_2, 0, 0)
    node _slicedA_1_0_T_4 = add(boundK, UInt<1>("h0")) @[TPU.scala 140:52]
    node _slicedA_1_0_T_5 = tail(_slicedA_1_0_T_4, 1) @[TPU.scala 140:52]
    node _slicedA_1_0_T_6 = or(_slicedA_1_0_T_5, UInt<1>("h0"))
    node _slicedA_1_0_T_7 = bits(_slicedA_1_0_T_6, 0, 0)
    node _GEN_8 = validif(and(eq(UInt<1>("h0"), _slicedA_1_0_T_3), eq(UInt<1>("h0"), _slicedA_1_0_T_7)), paddedA_0_0) @[TPU.scala 140:{23,23}]
    node _GEN_9 = mux(and(eq(UInt<1>("h0"), _slicedA_1_0_T_3), eq(UInt<1>("h1"), _slicedA_1_0_T_7)), paddedA_0_1, _GEN_8) @[TPU.scala 140:{23,23}]
    node _GEN_10 = mux(and(eq(UInt<1>("h1"), _slicedA_1_0_T_3), eq(UInt<1>("h0"), _slicedA_1_0_T_7)), paddedA_1_0, _GEN_9) @[TPU.scala 140:{23,23}]
    node _GEN_11 = mux(and(eq(UInt<1>("h1"), _slicedA_1_0_T_3), eq(UInt<1>("h1"), _slicedA_1_0_T_7)), paddedA_1_1, _GEN_10) @[TPU.scala 140:{23,23}]
    node _slicedB_0_0_T = add(boundK, UInt<1>("h0")) @[TPU.scala 143:40]
    node _slicedB_0_0_T_1 = tail(_slicedB_0_0_T, 1) @[TPU.scala 143:40]
    node _slicedB_0_0_T_2 = or(_slicedB_0_0_T_1, UInt<1>("h0"))
    node _slicedB_0_0_T_3 = bits(_slicedB_0_0_T_2, 0, 0)
    node _slicedB_0_0_T_4 = add(boundN, UInt<1>("h0")) @[TPU.scala 143:52]
    node _slicedB_0_0_T_5 = tail(_slicedB_0_0_T_4, 1) @[TPU.scala 143:52]
    node _slicedB_0_0_T_6 = or(_slicedB_0_0_T_5, UInt<1>("h0"))
    node _slicedB_0_0_T_7 = bits(_slicedB_0_0_T_6, 0, 0)
    node _GEN_12 = validif(and(eq(UInt<1>("h0"), _slicedB_0_0_T_3), eq(UInt<1>("h0"), _slicedB_0_0_T_7)), paddedB_0_0) @[TPU.scala 143:{23,23}]
    node _GEN_13 = mux(and(eq(UInt<1>("h0"), _slicedB_0_0_T_3), eq(UInt<1>("h1"), _slicedB_0_0_T_7)), paddedB_0_1, _GEN_12) @[TPU.scala 143:{23,23}]
    node _GEN_14 = mux(and(eq(UInt<1>("h1"), _slicedB_0_0_T_3), eq(UInt<1>("h0"), _slicedB_0_0_T_7)), paddedB_1_0, _GEN_13) @[TPU.scala 143:{23,23}]
    node _GEN_15 = mux(and(eq(UInt<1>("h1"), _slicedB_0_0_T_3), eq(UInt<1>("h1"), _slicedB_0_0_T_7)), paddedB_1_1, _GEN_14) @[TPU.scala 143:{23,23}]
    node _slicedB_0_1_T = add(boundK, UInt<1>("h0")) @[TPU.scala 143:40]
    node _slicedB_0_1_T_1 = tail(_slicedB_0_1_T, 1) @[TPU.scala 143:40]
    node _slicedB_0_1_T_2 = or(_slicedB_0_1_T_1, UInt<1>("h0"))
    node _slicedB_0_1_T_3 = bits(_slicedB_0_1_T_2, 0, 0)
    node _slicedB_0_1_T_4 = add(boundN, UInt<1>("h1")) @[TPU.scala 143:52]
    node _slicedB_0_1_T_5 = tail(_slicedB_0_1_T_4, 1) @[TPU.scala 143:52]
    node _slicedB_0_1_T_6 = or(_slicedB_0_1_T_5, UInt<1>("h0"))
    node _slicedB_0_1_T_7 = bits(_slicedB_0_1_T_6, 0, 0)
    node _GEN_16 = validif(and(eq(UInt<1>("h0"), _slicedB_0_1_T_3), eq(UInt<1>("h0"), _slicedB_0_1_T_7)), paddedB_0_0) @[TPU.scala 143:{23,23}]
    node _GEN_17 = mux(and(eq(UInt<1>("h0"), _slicedB_0_1_T_3), eq(UInt<1>("h1"), _slicedB_0_1_T_7)), paddedB_0_1, _GEN_16) @[TPU.scala 143:{23,23}]
    node _GEN_18 = mux(and(eq(UInt<1>("h1"), _slicedB_0_1_T_3), eq(UInt<1>("h0"), _slicedB_0_1_T_7)), paddedB_1_0, _GEN_17) @[TPU.scala 143:{23,23}]
    node _GEN_19 = mux(and(eq(UInt<1>("h1"), _slicedB_0_1_T_3), eq(UInt<1>("h1"), _slicedB_0_1_T_7)), paddedB_1_1, _GEN_18) @[TPU.scala 143:{23,23}]
    node _slicedA_0_1_T = add(boundM, UInt<1>("h0")) @[TPU.scala 140:40]
    node _slicedA_0_1_T_1 = tail(_slicedA_0_1_T, 1) @[TPU.scala 140:40]
    node _slicedA_0_1_T_2 = or(_slicedA_0_1_T_1, UInt<1>("h0"))
    node _slicedA_0_1_T_3 = bits(_slicedA_0_1_T_2, 0, 0)
    node _slicedA_0_1_T_4 = add(boundK, UInt<1>("h1")) @[TPU.scala 140:52]
    node _slicedA_0_1_T_5 = tail(_slicedA_0_1_T_4, 1) @[TPU.scala 140:52]
    node _slicedA_0_1_T_6 = or(_slicedA_0_1_T_5, UInt<1>("h0"))
    node _slicedA_0_1_T_7 = bits(_slicedA_0_1_T_6, 0, 0)
    node _GEN_20 = validif(and(eq(UInt<1>("h0"), _slicedA_0_1_T_3), eq(UInt<1>("h0"), _slicedA_0_1_T_7)), paddedA_0_0) @[TPU.scala 140:{23,23}]
    node _GEN_21 = mux(and(eq(UInt<1>("h0"), _slicedA_0_1_T_3), eq(UInt<1>("h1"), _slicedA_0_1_T_7)), paddedA_0_1, _GEN_20) @[TPU.scala 140:{23,23}]
    node _GEN_22 = mux(and(eq(UInt<1>("h1"), _slicedA_0_1_T_3), eq(UInt<1>("h0"), _slicedA_0_1_T_7)), paddedA_1_0, _GEN_21) @[TPU.scala 140:{23,23}]
    node _GEN_23 = mux(and(eq(UInt<1>("h1"), _slicedA_0_1_T_3), eq(UInt<1>("h1"), _slicedA_0_1_T_7)), paddedA_1_1, _GEN_22) @[TPU.scala 140:{23,23}]
    node _slicedA_1_1_T = add(boundM, UInt<1>("h1")) @[TPU.scala 140:40]
    node _slicedA_1_1_T_1 = tail(_slicedA_1_1_T, 1) @[TPU.scala 140:40]
    node _slicedA_1_1_T_2 = or(_slicedA_1_1_T_1, UInt<1>("h0"))
    node _slicedA_1_1_T_3 = bits(_slicedA_1_1_T_2, 0, 0)
    node _slicedA_1_1_T_4 = add(boundK, UInt<1>("h1")) @[TPU.scala 140:52]
    node _slicedA_1_1_T_5 = tail(_slicedA_1_1_T_4, 1) @[TPU.scala 140:52]
    node _slicedA_1_1_T_6 = or(_slicedA_1_1_T_5, UInt<1>("h0"))
    node _slicedA_1_1_T_7 = bits(_slicedA_1_1_T_6, 0, 0)
    node _GEN_24 = validif(and(eq(UInt<1>("h0"), _slicedA_1_1_T_3), eq(UInt<1>("h0"), _slicedA_1_1_T_7)), paddedA_0_0) @[TPU.scala 140:{23,23}]
    node _GEN_25 = mux(and(eq(UInt<1>("h0"), _slicedA_1_1_T_3), eq(UInt<1>("h1"), _slicedA_1_1_T_7)), paddedA_0_1, _GEN_24) @[TPU.scala 140:{23,23}]
    node _GEN_26 = mux(and(eq(UInt<1>("h1"), _slicedA_1_1_T_3), eq(UInt<1>("h0"), _slicedA_1_1_T_7)), paddedA_1_0, _GEN_25) @[TPU.scala 140:{23,23}]
    node _GEN_27 = mux(and(eq(UInt<1>("h1"), _slicedA_1_1_T_3), eq(UInt<1>("h1"), _slicedA_1_1_T_7)), paddedA_1_1, _GEN_26) @[TPU.scala 140:{23,23}]
    node _slicedB_1_0_T = add(boundK, UInt<1>("h1")) @[TPU.scala 143:40]
    node _slicedB_1_0_T_1 = tail(_slicedB_1_0_T, 1) @[TPU.scala 143:40]
    node _slicedB_1_0_T_2 = or(_slicedB_1_0_T_1, UInt<1>("h0"))
    node _slicedB_1_0_T_3 = bits(_slicedB_1_0_T_2, 0, 0)
    node _slicedB_1_0_T_4 = add(boundN, UInt<1>("h0")) @[TPU.scala 143:52]
    node _slicedB_1_0_T_5 = tail(_slicedB_1_0_T_4, 1) @[TPU.scala 143:52]
    node _slicedB_1_0_T_6 = or(_slicedB_1_0_T_5, UInt<1>("h0"))
    node _slicedB_1_0_T_7 = bits(_slicedB_1_0_T_6, 0, 0)
    node _GEN_28 = validif(and(eq(UInt<1>("h0"), _slicedB_1_0_T_3), eq(UInt<1>("h0"), _slicedB_1_0_T_7)), paddedB_0_0) @[TPU.scala 143:{23,23}]
    node _GEN_29 = mux(and(eq(UInt<1>("h0"), _slicedB_1_0_T_3), eq(UInt<1>("h1"), _slicedB_1_0_T_7)), paddedB_0_1, _GEN_28) @[TPU.scala 143:{23,23}]
    node _GEN_30 = mux(and(eq(UInt<1>("h1"), _slicedB_1_0_T_3), eq(UInt<1>("h0"), _slicedB_1_0_T_7)), paddedB_1_0, _GEN_29) @[TPU.scala 143:{23,23}]
    node _GEN_31 = mux(and(eq(UInt<1>("h1"), _slicedB_1_0_T_3), eq(UInt<1>("h1"), _slicedB_1_0_T_7)), paddedB_1_1, _GEN_30) @[TPU.scala 143:{23,23}]
    node _slicedB_1_1_T = add(boundK, UInt<1>("h1")) @[TPU.scala 143:40]
    node _slicedB_1_1_T_1 = tail(_slicedB_1_1_T, 1) @[TPU.scala 143:40]
    node _slicedB_1_1_T_2 = or(_slicedB_1_1_T_1, UInt<1>("h0"))
    node _slicedB_1_1_T_3 = bits(_slicedB_1_1_T_2, 0, 0)
    node _slicedB_1_1_T_4 = add(boundN, UInt<1>("h1")) @[TPU.scala 143:52]
    node _slicedB_1_1_T_5 = tail(_slicedB_1_1_T_4, 1) @[TPU.scala 143:52]
    node _slicedB_1_1_T_6 = or(_slicedB_1_1_T_5, UInt<1>("h0"))
    node _slicedB_1_1_T_7 = bits(_slicedB_1_1_T_6, 0, 0)
    node _GEN_32 = validif(and(eq(UInt<1>("h0"), _slicedB_1_1_T_3), eq(UInt<1>("h0"), _slicedB_1_1_T_7)), paddedB_0_0) @[TPU.scala 143:{23,23}]
    node _GEN_33 = mux(and(eq(UInt<1>("h0"), _slicedB_1_1_T_3), eq(UInt<1>("h1"), _slicedB_1_1_T_7)), paddedB_0_1, _GEN_32) @[TPU.scala 143:{23,23}]
    node _GEN_34 = mux(and(eq(UInt<1>("h1"), _slicedB_1_1_T_3), eq(UInt<1>("h0"), _slicedB_1_1_T_7)), paddedB_1_0, _GEN_33) @[TPU.scala 143:{23,23}]
    node _GEN_35 = mux(and(eq(UInt<1>("h1"), _slicedB_1_1_T_3), eq(UInt<1>("h1"), _slicedB_1_1_T_7)), paddedB_1_1, _GEN_34) @[TPU.scala 143:{23,23}]
    node _T_2 = and(io_a_valid, io_a_ready) @[TPU.scala 163:21]
    node _GEN_36 = mux(_T_2, UInt<3>("h1"), state) @[TPU.scala 163:35 164:15 44:22]
    node _GEN_37 = mux(_T_2, io_a_bits_0_0, act_in_0_0) @[TPU.scala 163:35 165:16 84:23]
    node _GEN_38 = mux(_T_2, io_a_bits_0_1, act_in_0_1) @[TPU.scala 163:35 165:16 84:23]
    node _GEN_39 = mux(_T_2, io_a_bits_1_0, act_in_1_0) @[TPU.scala 163:35 165:16 84:23]
    node _GEN_40 = mux(_T_2, io_a_bits_1_1, act_in_1_1) @[TPU.scala 163:35 165:16 84:23]
    node _GEN_41 = mux(_T_2, io_a_bits_0_0, paddedA_0_0) @[TPU.scala 117:21 163:35 66:24]
    node _GEN_42 = mux(_T_2, io_a_bits_0_1, paddedA_0_1) @[TPU.scala 117:21 163:35 66:24]
    node _GEN_43 = mux(_T_2, io_a_bits_1_0, paddedA_1_0) @[TPU.scala 117:21 163:35 66:24]
    node _GEN_44 = mux(_T_2, io_a_bits_1_1, paddedA_1_1) @[TPU.scala 117:21 163:35 66:24]
    node _GEN_45 = mux(_T_2, io_a_bits_0_0, slicedA_0_0) @[TPU.scala 117:21 163:35 86:15]
    node _GEN_46 = mux(_T_2, io_a_bits_0_1, slicedA_0_1) @[TPU.scala 117:21 163:35 86:15]
    node _GEN_47 = mux(_T_2, io_a_bits_1_0, slicedA_1_0) @[TPU.scala 117:21 163:35 86:15]
    node _GEN_48 = mux(_T_2, io_a_bits_1_1, slicedA_1_1) @[TPU.scala 117:21 163:35 86:15]
    node _GEN_49 = mux(_T_2, UInt<1>("h0"), a_ready) @[TPU.scala 163:35 168:17 51:24]
    node _GEN_50 = mux(_T_5, UInt<3>("h2"), state) @[TPU.scala 183:29 184:15 44:22]
    node _GEN_52 = mux(_T_4, io_b_bits_0_0, paddedB_0_0) @[TPU.scala 117:21 173:37 67:24]
    node _GEN_53 = mux(_T_4, io_b_bits_0_1, paddedB_0_1) @[TPU.scala 117:21 173:37 67:24]
    node _GEN_54 = mux(_T_4, io_b_bits_1_0, paddedB_1_0) @[TPU.scala 117:21 173:37 67:24]
    node _GEN_55 = mux(_T_4, io_b_bits_1_1, paddedB_1_1) @[TPU.scala 117:21 173:37 67:24]
    node _GEN_56 = mux(_T_4, io_b_bits_0_0, slicedB_0_0) @[TPU.scala 117:21 173:37 87:19]
    node _GEN_57 = mux(_T_4, io_b_bits_0_1, slicedB_0_1) @[TPU.scala 117:21 173:37 87:19]
    node _GEN_58 = mux(_T_4, io_b_bits_1_0, slicedB_1_0) @[TPU.scala 117:21 173:37 87:19]
    node _GEN_59 = mux(_T_4, io_b_bits_1_1, slicedB_1_1) @[TPU.scala 117:21 173:37 87:19]
    node _GEN_61 = mux(_T_4, UInt<1>("h0"), b_ready) @[TPU.scala 173:37 179:17 52:24]
    node _GEN_62 = mux(_T_4, UInt<1>("h0"), _GEN_1) @[TPU.scala 173:37 181:15]
    node _GEN_63 = mux(_T_4, state, _GEN_50) @[TPU.scala 173:37 44:22]
    node _T_7 = bits(reset, 0, 0) @[TPU.scala 196:11]
    node _T_8 = eq(_T_7, UInt<1>("h0")) @[TPU.scala 196:11]
    node _T_9 = bits(reset, 0, 0) @[TPU.scala 198:13]
    node _T_10 = eq(_T_9, UInt<1>("h0")) @[TPU.scala 198:13]
    node _T_11 = bits(reset, 0, 0) @[TPU.scala 198:13]
    node _T_12 = eq(_T_11, UInt<1>("h0")) @[TPU.scala 198:13]
    node _T_14 = geq(cycle, UInt<2>("h3")) @[TPU.scala 226:16]
    node _cycleIdx_T = sub(cycle, UInt<2>("h3")) @[TPU.scala 227:24]
    node _cycleIdx_T_1 = tail(_cycleIdx_T, 1) @[TPU.scala 227:24]
    node _GEN_65 = mux(_T_14, _cycleIdx_T_1, cycle) @[TPU.scala 226:40 227:16 230:16]
    node _T_15 = geq(cycle, UInt<2>("h3")) @[TPU.scala 232:16]
    node _GEN_187 = mux(_T_13, _GEN_65, UInt<1>("h0")) @[TPU.scala 212:32 94:12]
    node _GEN_206 = mux(_T_6, UInt<1>("h0"), _GEN_187) @[TPU.scala 191:29 94:12]
    node _GEN_230 = mux(_T_3, UInt<1>("h0"), _GEN_206) @[TPU.scala 172:28 94:12]
    node _GEN_266 = mux(_T_1, UInt<1>("h0"), _GEN_230) @[TPU.scala 162:23 94:12]
    node cycleIdx = pad(_GEN_266, 32) @[TPU.scala 90:22]
    node _T_16 = geq(cycleIdx, UInt<1>("h0")) @[TPU.scala 234:22]
    node _T_17 = lt(cycleIdx, UInt<2>("h2")) @[TPU.scala 234:39]
    node _T_18 = and(_T_16, _T_17) @[TPU.scala 234:28]
    node _GEN_66 = mux(_T_18, slicedOut_1_0, slicedOut_0_0) @[TPU.scala 234:59 240:35 71:26]
    node _GEN_67 = mux(_T_18, systArr.io_out_0, slicedOut_1_0) @[TPU.scala 234:59 237:35 71:26]
    node _T_19 = geq(cycleIdx, UInt<1>("h1")) @[TPU.scala 234:22]
    node _T_20 = lt(cycleIdx, UInt<2>("h3")) @[TPU.scala 234:39]
    node _T_21 = and(_T_19, _T_20) @[TPU.scala 234:28]
    node _GEN_68 = mux(_T_21, slicedOut_1_1, slicedOut_0_1) @[TPU.scala 234:59 240:35 71:26]
    node _GEN_69 = mux(_T_21, systArr.io_out_1, slicedOut_1_1) @[TPU.scala 234:59 237:35 71:26]
    node _GEN_70 = mux(_T_15, _GEN_66, slicedOut_0_0) @[TPU.scala 232:40 71:26]
    node _GEN_71 = mux(_T_15, _GEN_67, slicedOut_1_0) @[TPU.scala 232:40 71:26]
    node _GEN_72 = mux(_T_15, _GEN_68, slicedOut_0_1) @[TPU.scala 232:40 71:26]
    node _GEN_73 = mux(_T_15, _GEN_69, slicedOut_1_1) @[TPU.scala 232:40 71:26]
    node _T_22 = eq(sliceCycle, UInt<1>("h0")) @[TPU.scala 250:21]
    node _T_23 = eq(cycle, UInt<3>("h6")) @[TPU.scala 250:76]
    node _T_24 = and(_T_22, _T_23) @[TPU.scala 250:67]
    node _T_25 = bits(reset, 0, 0) @[TPU.scala 252:13]
    node _T_26 = eq(_T_25, UInt<1>("h0")) @[TPU.scala 252:13]
    node _T_27 = add(UInt<1>("h0"), boundM) @[TPU.scala 255:20]
    node _T_28 = tail(_T_27, 1) @[TPU.scala 255:20]
    node _T_29 = lt(_T_28, UInt<2>("h2")) @[TPU.scala 255:29]
    node _T_30 = add(UInt<1>("h0"), boundN) @[TPU.scala 255:44]
    node _T_31 = tail(_T_30, 1) @[TPU.scala 255:44]
    node _T_32 = lt(_T_31, UInt<2>("h2")) @[TPU.scala 255:53]
    node _T_33 = and(_T_29, _T_32) @[TPU.scala 255:37]
    node _T_34 = add(UInt<1>("h0"), boundM) @[TPU.scala 256:23]
    node _T_35 = tail(_T_34, 1) @[TPU.scala 256:23]
    node _T_36 = or(_T_35, UInt<1>("h0"))
    node _T_37 = bits(_T_36, 0, 0)
    node _T_38 = add(UInt<1>("h0"), boundN) @[TPU.scala 256:37]
    node _T_39 = tail(_T_38, 1) @[TPU.scala 256:37]
    node _T_40 = or(_T_39, UInt<1>("h0"))
    node _T_41 = bits(_T_40, 0, 0)
    node _myOut_T = add(UInt<1>("h0"), boundM) @[TPU.scala 256:60]
    node _myOut_T_1 = tail(_myOut_T, 1) @[TPU.scala 256:60]
    node _myOut_T_2 = or(_myOut_T_1, UInt<1>("h0"))
    node _myOut_T_3 = bits(_myOut_T_2, 0, 0)
    node _myOut_T_4 = add(UInt<1>("h0"), boundN) @[TPU.scala 256:74]
    node _myOut_T_5 = tail(_myOut_T_4, 1) @[TPU.scala 256:74]
    node _myOut_T_6 = or(_myOut_T_5, UInt<1>("h0"))
    node _myOut_T_7 = bits(_myOut_T_6, 0, 0)
    node _GEN_74 = validif(and(eq(UInt<1>("h0"), _myOut_T_3), eq(UInt<1>("h0"), _myOut_T_7)), myOut_0_0) @[TPU.scala 256:{84,84}]
    node _GEN_75 = mux(and(eq(UInt<1>("h0"), _myOut_T_3), eq(UInt<1>("h1"), _myOut_T_7)), myOut_0_1, _GEN_74) @[TPU.scala 256:{84,84}]
    node _GEN_76 = mux(and(eq(UInt<1>("h1"), _myOut_T_3), eq(UInt<1>("h0"), _myOut_T_7)), myOut_1_0, _GEN_75) @[TPU.scala 256:{84,84}]
    node _GEN_77 = mux(and(eq(UInt<1>("h1"), _myOut_T_3), eq(UInt<1>("h1"), _myOut_T_7)), myOut_1_1, _GEN_76) @[TPU.scala 256:{84,84}]
    node _myOut_myOut_T_3_myOut_T_7 = _GEN_77 @[TPU.scala 256:84]
    node _myOut_T_8 = add(_myOut_myOut_T_3_myOut_T_7, slicedOut_0_0) @[TPU.scala 256:84]
    node _myOut_T_9 = tail(_myOut_T_8, 1) @[TPU.scala 256:84]
    node _myOut_T_10 = asSInt(_myOut_T_9) @[TPU.scala 256:84]
    node _myOut_T_37_T_41 = _myOut_T_10 @[TPU.scala 256:{47,47}]
    node _GEN_78 = mux(and(eq(UInt<1>("h0"), _T_37), eq(UInt<1>("h0"), _T_41)), _myOut_T_37_T_41, myOut_0_0) @[TPU.scala 256:{47,47} 50:22]
    node _GEN_79 = mux(and(eq(UInt<1>("h0"), _T_37), eq(UInt<1>("h1"), _T_41)), _myOut_T_37_T_41, myOut_0_1) @[TPU.scala 256:{47,47} 50:22]
    node _GEN_80 = mux(and(eq(UInt<1>("h1"), _T_37), eq(UInt<1>("h0"), _T_41)), _myOut_T_37_T_41, myOut_1_0) @[TPU.scala 256:{47,47} 50:22]
    node _GEN_81 = mux(and(eq(UInt<1>("h1"), _T_37), eq(UInt<1>("h1"), _T_41)), _myOut_T_37_T_41, myOut_1_1) @[TPU.scala 256:{47,47} 50:22]
    node _GEN_82 = mux(_T_33, _GEN_78, myOut_0_0) @[TPU.scala 255:61 50:22]
    node _GEN_83 = mux(_T_33, _GEN_79, myOut_0_1) @[TPU.scala 255:61 50:22]
    node _GEN_84 = mux(_T_33, _GEN_80, myOut_1_0) @[TPU.scala 255:61 50:22]
    node _GEN_85 = mux(_T_33, _GEN_81, myOut_1_1) @[TPU.scala 255:61 50:22]
    node _T_42 = add(UInt<1>("h0"), boundM) @[TPU.scala 255:20]
    node _T_43 = tail(_T_42, 1) @[TPU.scala 255:20]
    node _T_44 = lt(_T_43, UInt<2>("h2")) @[TPU.scala 255:29]
    node _T_45 = add(UInt<1>("h1"), boundN) @[TPU.scala 255:44]
    node _T_46 = tail(_T_45, 1) @[TPU.scala 255:44]
    node _T_47 = lt(_T_46, UInt<2>("h2")) @[TPU.scala 255:53]
    node _T_48 = and(_T_44, _T_47) @[TPU.scala 255:37]
    node _T_49 = add(UInt<1>("h0"), boundM) @[TPU.scala 256:23]
    node _T_50 = tail(_T_49, 1) @[TPU.scala 256:23]
    node _T_51 = or(_T_50, UInt<1>("h0"))
    node _T_52 = bits(_T_51, 0, 0)
    node _T_53 = add(UInt<1>("h1"), boundN) @[TPU.scala 256:37]
    node _T_54 = tail(_T_53, 1) @[TPU.scala 256:37]
    node _T_55 = or(_T_54, UInt<1>("h0"))
    node _T_56 = bits(_T_55, 0, 0)
    node _myOut_T_11 = add(UInt<1>("h0"), boundM) @[TPU.scala 256:60]
    node _myOut_T_12 = tail(_myOut_T_11, 1) @[TPU.scala 256:60]
    node _myOut_T_13 = or(_myOut_T_12, UInt<1>("h0"))
    node _myOut_T_14 = bits(_myOut_T_13, 0, 0)
    node _myOut_T_15 = add(UInt<1>("h1"), boundN) @[TPU.scala 256:74]
    node _myOut_T_16 = tail(_myOut_T_15, 1) @[TPU.scala 256:74]
    node _myOut_T_17 = or(_myOut_T_16, UInt<1>("h0"))
    node _myOut_T_18 = bits(_myOut_T_17, 0, 0)
    node _GEN_86 = validif(and(eq(UInt<1>("h0"), _myOut_T_14), eq(UInt<1>("h0"), _myOut_T_18)), myOut_0_0) @[TPU.scala 256:{84,84}]
    node _GEN_87 = mux(and(eq(UInt<1>("h0"), _myOut_T_14), eq(UInt<1>("h1"), _myOut_T_18)), myOut_0_1, _GEN_86) @[TPU.scala 256:{84,84}]
    node _GEN_88 = mux(and(eq(UInt<1>("h1"), _myOut_T_14), eq(UInt<1>("h0"), _myOut_T_18)), myOut_1_0, _GEN_87) @[TPU.scala 256:{84,84}]
    node _GEN_89 = mux(and(eq(UInt<1>("h1"), _myOut_T_14), eq(UInt<1>("h1"), _myOut_T_18)), myOut_1_1, _GEN_88) @[TPU.scala 256:{84,84}]
    node _myOut_myOut_T_14_myOut_T_18 = _GEN_89 @[TPU.scala 256:84]
    node _myOut_T_19 = add(_myOut_myOut_T_14_myOut_T_18, slicedOut_0_1) @[TPU.scala 256:84]
    node _myOut_T_20 = tail(_myOut_T_19, 1) @[TPU.scala 256:84]
    node _myOut_T_21 = asSInt(_myOut_T_20) @[TPU.scala 256:84]
    node _myOut_T_52_T_56 = _myOut_T_21 @[TPU.scala 256:{47,47}]
    node _GEN_90 = mux(and(eq(UInt<1>("h0"), _T_52), eq(UInt<1>("h0"), _T_56)), _myOut_T_52_T_56, _GEN_82) @[TPU.scala 256:{47,47}]
    node _GEN_91 = mux(and(eq(UInt<1>("h0"), _T_52), eq(UInt<1>("h1"), _T_56)), _myOut_T_52_T_56, _GEN_83) @[TPU.scala 256:{47,47}]
    node _GEN_92 = mux(and(eq(UInt<1>("h1"), _T_52), eq(UInt<1>("h0"), _T_56)), _myOut_T_52_T_56, _GEN_84) @[TPU.scala 256:{47,47}]
    node _GEN_93 = mux(and(eq(UInt<1>("h1"), _T_52), eq(UInt<1>("h1"), _T_56)), _myOut_T_52_T_56, _GEN_85) @[TPU.scala 256:{47,47}]
    node _GEN_94 = mux(_T_48, _GEN_90, _GEN_82) @[TPU.scala 255:61]
    node _GEN_95 = mux(_T_48, _GEN_91, _GEN_83) @[TPU.scala 255:61]
    node _GEN_96 = mux(_T_48, _GEN_92, _GEN_84) @[TPU.scala 255:61]
    node _GEN_97 = mux(_T_48, _GEN_93, _GEN_85) @[TPU.scala 255:61]
    node _T_57 = add(UInt<1>("h1"), boundM) @[TPU.scala 255:20]
    node _T_58 = tail(_T_57, 1) @[TPU.scala 255:20]
    node _T_59 = lt(_T_58, UInt<2>("h2")) @[TPU.scala 255:29]
    node _T_60 = add(UInt<1>("h0"), boundN) @[TPU.scala 255:44]
    node _T_61 = tail(_T_60, 1) @[TPU.scala 255:44]
    node _T_62 = lt(_T_61, UInt<2>("h2")) @[TPU.scala 255:53]
    node _T_63 = and(_T_59, _T_62) @[TPU.scala 255:37]
    node _T_64 = add(UInt<1>("h1"), boundM) @[TPU.scala 256:23]
    node _T_65 = tail(_T_64, 1) @[TPU.scala 256:23]
    node _T_66 = or(_T_65, UInt<1>("h0"))
    node _T_67 = bits(_T_66, 0, 0)
    node _T_68 = add(UInt<1>("h0"), boundN) @[TPU.scala 256:37]
    node _T_69 = tail(_T_68, 1) @[TPU.scala 256:37]
    node _T_70 = or(_T_69, UInt<1>("h0"))
    node _T_71 = bits(_T_70, 0, 0)
    node _myOut_T_22 = add(UInt<1>("h1"), boundM) @[TPU.scala 256:60]
    node _myOut_T_23 = tail(_myOut_T_22, 1) @[TPU.scala 256:60]
    node _myOut_T_24 = or(_myOut_T_23, UInt<1>("h0"))
    node _myOut_T_25 = bits(_myOut_T_24, 0, 0)
    node _myOut_T_26 = add(UInt<1>("h0"), boundN) @[TPU.scala 256:74]
    node _myOut_T_27 = tail(_myOut_T_26, 1) @[TPU.scala 256:74]
    node _myOut_T_28 = or(_myOut_T_27, UInt<1>("h0"))
    node _myOut_T_29 = bits(_myOut_T_28, 0, 0)
    node _GEN_98 = validif(and(eq(UInt<1>("h0"), _myOut_T_25), eq(UInt<1>("h0"), _myOut_T_29)), myOut_0_0) @[TPU.scala 256:{84,84}]
    node _GEN_99 = mux(and(eq(UInt<1>("h0"), _myOut_T_25), eq(UInt<1>("h1"), _myOut_T_29)), myOut_0_1, _GEN_98) @[TPU.scala 256:{84,84}]
    node _GEN_100 = mux(and(eq(UInt<1>("h1"), _myOut_T_25), eq(UInt<1>("h0"), _myOut_T_29)), myOut_1_0, _GEN_99) @[TPU.scala 256:{84,84}]
    node _GEN_101 = mux(and(eq(UInt<1>("h1"), _myOut_T_25), eq(UInt<1>("h1"), _myOut_T_29)), myOut_1_1, _GEN_100) @[TPU.scala 256:{84,84}]
    node _myOut_myOut_T_25_myOut_T_29 = _GEN_101 @[TPU.scala 256:84]
    node _myOut_T_30 = add(_myOut_myOut_T_25_myOut_T_29, slicedOut_1_0) @[TPU.scala 256:84]
    node _myOut_T_31 = tail(_myOut_T_30, 1) @[TPU.scala 256:84]
    node _myOut_T_32 = asSInt(_myOut_T_31) @[TPU.scala 256:84]
    node _myOut_T_67_T_71 = _myOut_T_32 @[TPU.scala 256:{47,47}]
    node _GEN_102 = mux(and(eq(UInt<1>("h0"), _T_67), eq(UInt<1>("h0"), _T_71)), _myOut_T_67_T_71, _GEN_94) @[TPU.scala 256:{47,47}]
    node _GEN_103 = mux(and(eq(UInt<1>("h0"), _T_67), eq(UInt<1>("h1"), _T_71)), _myOut_T_67_T_71, _GEN_95) @[TPU.scala 256:{47,47}]
    node _GEN_104 = mux(and(eq(UInt<1>("h1"), _T_67), eq(UInt<1>("h0"), _T_71)), _myOut_T_67_T_71, _GEN_96) @[TPU.scala 256:{47,47}]
    node _GEN_105 = mux(and(eq(UInt<1>("h1"), _T_67), eq(UInt<1>("h1"), _T_71)), _myOut_T_67_T_71, _GEN_97) @[TPU.scala 256:{47,47}]
    node _GEN_106 = mux(_T_63, _GEN_102, _GEN_94) @[TPU.scala 255:61]
    node _GEN_107 = mux(_T_63, _GEN_103, _GEN_95) @[TPU.scala 255:61]
    node _GEN_108 = mux(_T_63, _GEN_104, _GEN_96) @[TPU.scala 255:61]
    node _GEN_109 = mux(_T_63, _GEN_105, _GEN_97) @[TPU.scala 255:61]
    node _T_72 = add(UInt<1>("h1"), boundM) @[TPU.scala 255:20]
    node _T_73 = tail(_T_72, 1) @[TPU.scala 255:20]
    node _T_74 = lt(_T_73, UInt<2>("h2")) @[TPU.scala 255:29]
    node _T_75 = add(UInt<1>("h1"), boundN) @[TPU.scala 255:44]
    node _T_76 = tail(_T_75, 1) @[TPU.scala 255:44]
    node _T_77 = lt(_T_76, UInt<2>("h2")) @[TPU.scala 255:53]
    node _T_78 = and(_T_74, _T_77) @[TPU.scala 255:37]
    node _T_79 = add(UInt<1>("h1"), boundM) @[TPU.scala 256:23]
    node _T_80 = tail(_T_79, 1) @[TPU.scala 256:23]
    node _T_81 = or(_T_80, UInt<1>("h0"))
    node _T_82 = bits(_T_81, 0, 0)
    node _T_83 = add(UInt<1>("h1"), boundN) @[TPU.scala 256:37]
    node _T_84 = tail(_T_83, 1) @[TPU.scala 256:37]
    node _T_85 = or(_T_84, UInt<1>("h0"))
    node _T_86 = bits(_T_85, 0, 0)
    node _myOut_T_33 = add(UInt<1>("h1"), boundM) @[TPU.scala 256:60]
    node _myOut_T_34 = tail(_myOut_T_33, 1) @[TPU.scala 256:60]
    node _myOut_T_35 = or(_myOut_T_34, UInt<1>("h0"))
    node _myOut_T_36 = bits(_myOut_T_35, 0, 0)
    node _myOut_T_37 = add(UInt<1>("h1"), boundN) @[TPU.scala 256:74]
    node _myOut_T_38 = tail(_myOut_T_37, 1) @[TPU.scala 256:74]
    node _myOut_T_39 = or(_myOut_T_38, UInt<1>("h0"))
    node _myOut_T_40 = bits(_myOut_T_39, 0, 0)
    node _GEN_110 = validif(and(eq(UInt<1>("h0"), _myOut_T_36), eq(UInt<1>("h0"), _myOut_T_40)), myOut_0_0) @[TPU.scala 256:{84,84}]
    node _GEN_111 = mux(and(eq(UInt<1>("h0"), _myOut_T_36), eq(UInt<1>("h1"), _myOut_T_40)), myOut_0_1, _GEN_110) @[TPU.scala 256:{84,84}]
    node _GEN_112 = mux(and(eq(UInt<1>("h1"), _myOut_T_36), eq(UInt<1>("h0"), _myOut_T_40)), myOut_1_0, _GEN_111) @[TPU.scala 256:{84,84}]
    node _GEN_113 = mux(and(eq(UInt<1>("h1"), _myOut_T_36), eq(UInt<1>("h1"), _myOut_T_40)), myOut_1_1, _GEN_112) @[TPU.scala 256:{84,84}]
    node _myOut_myOut_T_36_myOut_T_40 = _GEN_113 @[TPU.scala 256:84]
    node _myOut_T_41 = add(_myOut_myOut_T_36_myOut_T_40, slicedOut_1_1) @[TPU.scala 256:84]
    node _myOut_T_42 = tail(_myOut_T_41, 1) @[TPU.scala 256:84]
    node _myOut_T_43 = asSInt(_myOut_T_42) @[TPU.scala 256:84]
    node _myOut_T_82_T_86 = _myOut_T_43 @[TPU.scala 256:{47,47}]
    node _GEN_114 = mux(and(eq(UInt<1>("h0"), _T_82), eq(UInt<1>("h0"), _T_86)), _myOut_T_82_T_86, _GEN_106) @[TPU.scala 256:{47,47}]
    node _GEN_115 = mux(and(eq(UInt<1>("h0"), _T_82), eq(UInt<1>("h1"), _T_86)), _myOut_T_82_T_86, _GEN_107) @[TPU.scala 256:{47,47}]
    node _GEN_116 = mux(and(eq(UInt<1>("h1"), _T_82), eq(UInt<1>("h0"), _T_86)), _myOut_T_82_T_86, _GEN_108) @[TPU.scala 256:{47,47}]
    node _GEN_117 = mux(and(eq(UInt<1>("h1"), _T_82), eq(UInt<1>("h1"), _T_86)), _myOut_T_82_T_86, _GEN_109) @[TPU.scala 256:{47,47}]
    node _GEN_118 = mux(_T_78, _GEN_114, _GEN_106) @[TPU.scala 255:61]
    node _GEN_119 = mux(_T_78, _GEN_115, _GEN_107) @[TPU.scala 255:61]
    node _GEN_120 = mux(_T_78, _GEN_116, _GEN_108) @[TPU.scala 255:61]
    node _GEN_121 = mux(_T_78, _GEN_117, _GEN_109) @[TPU.scala 255:61]
    node _T_87 = eq(cycle, UInt<3>("h6")) @[TPU.scala 267:21]
    node _T_88 = add(UInt<1>("h0"), boundM) @[TPU.scala 271:20]
    node _T_89 = tail(_T_88, 1) @[TPU.scala 271:20]
    node _T_90 = lt(_T_89, UInt<2>("h2")) @[TPU.scala 271:29]
    node _T_91 = add(UInt<1>("h0"), boundN) @[TPU.scala 271:44]
    node _T_92 = tail(_T_91, 1) @[TPU.scala 271:44]
    node _T_93 = lt(_T_92, UInt<2>("h2")) @[TPU.scala 271:53]
    node _T_94 = and(_T_90, _T_93) @[TPU.scala 271:37]
    node _T_95 = add(UInt<1>("h0"), boundM) @[TPU.scala 272:23]
    node _T_96 = tail(_T_95, 1) @[TPU.scala 272:23]
    node _T_97 = or(_T_96, UInt<1>("h0"))
    node _T_98 = bits(_T_97, 0, 0)
    node _T_99 = add(UInt<1>("h0"), boundN) @[TPU.scala 272:37]
    node _T_100 = tail(_T_99, 1) @[TPU.scala 272:37]
    node _T_101 = or(_T_100, UInt<1>("h0"))
    node _T_102 = bits(_T_101, 0, 0)
    node _myOut_T_44 = add(UInt<1>("h0"), boundM) @[TPU.scala 272:60]
    node _myOut_T_45 = tail(_myOut_T_44, 1) @[TPU.scala 272:60]
    node _myOut_T_46 = or(_myOut_T_45, UInt<1>("h0"))
    node _myOut_T_47 = bits(_myOut_T_46, 0, 0)
    node _myOut_T_48 = add(UInt<1>("h0"), boundN) @[TPU.scala 272:74]
    node _myOut_T_49 = tail(_myOut_T_48, 1) @[TPU.scala 272:74]
    node _myOut_T_50 = or(_myOut_T_49, UInt<1>("h0"))
    node _myOut_T_51 = bits(_myOut_T_50, 0, 0)
    node _GEN_122 = validif(and(eq(UInt<1>("h0"), _myOut_T_47), eq(UInt<1>("h0"), _myOut_T_51)), myOut_0_0) @[TPU.scala 272:{84,84}]
    node _GEN_123 = mux(and(eq(UInt<1>("h0"), _myOut_T_47), eq(UInt<1>("h1"), _myOut_T_51)), myOut_0_1, _GEN_122) @[TPU.scala 272:{84,84}]
    node _GEN_124 = mux(and(eq(UInt<1>("h1"), _myOut_T_47), eq(UInt<1>("h0"), _myOut_T_51)), myOut_1_0, _GEN_123) @[TPU.scala 272:{84,84}]
    node _GEN_125 = mux(and(eq(UInt<1>("h1"), _myOut_T_47), eq(UInt<1>("h1"), _myOut_T_51)), myOut_1_1, _GEN_124) @[TPU.scala 272:{84,84}]
    node _myOut_myOut_T_47_myOut_T_51 = _GEN_125 @[TPU.scala 272:84]
    node _myOut_T_52 = add(_myOut_myOut_T_47_myOut_T_51, slicedOut_0_0) @[TPU.scala 272:84]
    node _myOut_T_53 = tail(_myOut_T_52, 1) @[TPU.scala 272:84]
    node _myOut_T_54 = asSInt(_myOut_T_53) @[TPU.scala 272:84]
    node _myOut_T_98_T_102 = _myOut_T_54 @[TPU.scala 272:{47,47}]
    node _GEN_126 = mux(and(eq(UInt<1>("h0"), _T_98), eq(UInt<1>("h0"), _T_102)), _myOut_T_98_T_102, myOut_0_0) @[TPU.scala 272:{47,47} 50:22]
    node _GEN_127 = mux(and(eq(UInt<1>("h0"), _T_98), eq(UInt<1>("h1"), _T_102)), _myOut_T_98_T_102, myOut_0_1) @[TPU.scala 272:{47,47} 50:22]
    node _GEN_128 = mux(and(eq(UInt<1>("h1"), _T_98), eq(UInt<1>("h0"), _T_102)), _myOut_T_98_T_102, myOut_1_0) @[TPU.scala 272:{47,47} 50:22]
    node _GEN_129 = mux(and(eq(UInt<1>("h1"), _T_98), eq(UInt<1>("h1"), _T_102)), _myOut_T_98_T_102, myOut_1_1) @[TPU.scala 272:{47,47} 50:22]
    node _GEN_130 = mux(_T_94, _GEN_126, myOut_0_0) @[TPU.scala 271:61 50:22]
    node _GEN_131 = mux(_T_94, _GEN_127, myOut_0_1) @[TPU.scala 271:61 50:22]
    node _GEN_132 = mux(_T_94, _GEN_128, myOut_1_0) @[TPU.scala 271:61 50:22]
    node _GEN_133 = mux(_T_94, _GEN_129, myOut_1_1) @[TPU.scala 271:61 50:22]
    node _T_103 = add(UInt<1>("h0"), boundM) @[TPU.scala 271:20]
    node _T_104 = tail(_T_103, 1) @[TPU.scala 271:20]
    node _T_105 = lt(_T_104, UInt<2>("h2")) @[TPU.scala 271:29]
    node _T_106 = add(UInt<1>("h1"), boundN) @[TPU.scala 271:44]
    node _T_107 = tail(_T_106, 1) @[TPU.scala 271:44]
    node _T_108 = lt(_T_107, UInt<2>("h2")) @[TPU.scala 271:53]
    node _T_109 = and(_T_105, _T_108) @[TPU.scala 271:37]
    node _T_110 = add(UInt<1>("h0"), boundM) @[TPU.scala 272:23]
    node _T_111 = tail(_T_110, 1) @[TPU.scala 272:23]
    node _T_112 = or(_T_111, UInt<1>("h0"))
    node _T_113 = bits(_T_112, 0, 0)
    node _T_114 = add(UInt<1>("h1"), boundN) @[TPU.scala 272:37]
    node _T_115 = tail(_T_114, 1) @[TPU.scala 272:37]
    node _T_116 = or(_T_115, UInt<1>("h0"))
    node _T_117 = bits(_T_116, 0, 0)
    node _myOut_T_55 = add(UInt<1>("h0"), boundM) @[TPU.scala 272:60]
    node _myOut_T_56 = tail(_myOut_T_55, 1) @[TPU.scala 272:60]
    node _myOut_T_57 = or(_myOut_T_56, UInt<1>("h0"))
    node _myOut_T_58 = bits(_myOut_T_57, 0, 0)
    node _myOut_T_59 = add(UInt<1>("h1"), boundN) @[TPU.scala 272:74]
    node _myOut_T_60 = tail(_myOut_T_59, 1) @[TPU.scala 272:74]
    node _myOut_T_61 = or(_myOut_T_60, UInt<1>("h0"))
    node _myOut_T_62 = bits(_myOut_T_61, 0, 0)
    node _GEN_134 = validif(and(eq(UInt<1>("h0"), _myOut_T_58), eq(UInt<1>("h0"), _myOut_T_62)), myOut_0_0) @[TPU.scala 272:{84,84}]
    node _GEN_135 = mux(and(eq(UInt<1>("h0"), _myOut_T_58), eq(UInt<1>("h1"), _myOut_T_62)), myOut_0_1, _GEN_134) @[TPU.scala 272:{84,84}]
    node _GEN_136 = mux(and(eq(UInt<1>("h1"), _myOut_T_58), eq(UInt<1>("h0"), _myOut_T_62)), myOut_1_0, _GEN_135) @[TPU.scala 272:{84,84}]
    node _GEN_137 = mux(and(eq(UInt<1>("h1"), _myOut_T_58), eq(UInt<1>("h1"), _myOut_T_62)), myOut_1_1, _GEN_136) @[TPU.scala 272:{84,84}]
    node _myOut_myOut_T_58_myOut_T_62 = _GEN_137 @[TPU.scala 272:84]
    node _myOut_T_63 = add(_myOut_myOut_T_58_myOut_T_62, slicedOut_0_1) @[TPU.scala 272:84]
    node _myOut_T_64 = tail(_myOut_T_63, 1) @[TPU.scala 272:84]
    node _myOut_T_65 = asSInt(_myOut_T_64) @[TPU.scala 272:84]
    node _myOut_T_113_T_117 = _myOut_T_65 @[TPU.scala 272:{47,47}]
    node _GEN_138 = mux(and(eq(UInt<1>("h0"), _T_113), eq(UInt<1>("h0"), _T_117)), _myOut_T_113_T_117, _GEN_130) @[TPU.scala 272:{47,47}]
    node _GEN_139 = mux(and(eq(UInt<1>("h0"), _T_113), eq(UInt<1>("h1"), _T_117)), _myOut_T_113_T_117, _GEN_131) @[TPU.scala 272:{47,47}]
    node _GEN_140 = mux(and(eq(UInt<1>("h1"), _T_113), eq(UInt<1>("h0"), _T_117)), _myOut_T_113_T_117, _GEN_132) @[TPU.scala 272:{47,47}]
    node _GEN_141 = mux(and(eq(UInt<1>("h1"), _T_113), eq(UInt<1>("h1"), _T_117)), _myOut_T_113_T_117, _GEN_133) @[TPU.scala 272:{47,47}]
    node _GEN_142 = mux(_T_109, _GEN_138, _GEN_130) @[TPU.scala 271:61]
    node _GEN_143 = mux(_T_109, _GEN_139, _GEN_131) @[TPU.scala 271:61]
    node _GEN_144 = mux(_T_109, _GEN_140, _GEN_132) @[TPU.scala 271:61]
    node _GEN_145 = mux(_T_109, _GEN_141, _GEN_133) @[TPU.scala 271:61]
    node _T_118 = add(UInt<1>("h1"), boundM) @[TPU.scala 271:20]
    node _T_119 = tail(_T_118, 1) @[TPU.scala 271:20]
    node _T_120 = lt(_T_119, UInt<2>("h2")) @[TPU.scala 271:29]
    node _T_121 = add(UInt<1>("h0"), boundN) @[TPU.scala 271:44]
    node _T_122 = tail(_T_121, 1) @[TPU.scala 271:44]
    node _T_123 = lt(_T_122, UInt<2>("h2")) @[TPU.scala 271:53]
    node _T_124 = and(_T_120, _T_123) @[TPU.scala 271:37]
    node _T_125 = add(UInt<1>("h1"), boundM) @[TPU.scala 272:23]
    node _T_126 = tail(_T_125, 1) @[TPU.scala 272:23]
    node _T_127 = or(_T_126, UInt<1>("h0"))
    node _T_128 = bits(_T_127, 0, 0)
    node _T_129 = add(UInt<1>("h0"), boundN) @[TPU.scala 272:37]
    node _T_130 = tail(_T_129, 1) @[TPU.scala 272:37]
    node _T_131 = or(_T_130, UInt<1>("h0"))
    node _T_132 = bits(_T_131, 0, 0)
    node _myOut_T_66 = add(UInt<1>("h1"), boundM) @[TPU.scala 272:60]
    node _myOut_T_67 = tail(_myOut_T_66, 1) @[TPU.scala 272:60]
    node _myOut_T_68 = or(_myOut_T_67, UInt<1>("h0"))
    node _myOut_T_69 = bits(_myOut_T_68, 0, 0)
    node _myOut_T_70 = add(UInt<1>("h0"), boundN) @[TPU.scala 272:74]
    node _myOut_T_71 = tail(_myOut_T_70, 1) @[TPU.scala 272:74]
    node _myOut_T_72 = or(_myOut_T_71, UInt<1>("h0"))
    node _myOut_T_73 = bits(_myOut_T_72, 0, 0)
    node _GEN_146 = validif(and(eq(UInt<1>("h0"), _myOut_T_69), eq(UInt<1>("h0"), _myOut_T_73)), myOut_0_0) @[TPU.scala 272:{84,84}]
    node _GEN_147 = mux(and(eq(UInt<1>("h0"), _myOut_T_69), eq(UInt<1>("h1"), _myOut_T_73)), myOut_0_1, _GEN_146) @[TPU.scala 272:{84,84}]
    node _GEN_148 = mux(and(eq(UInt<1>("h1"), _myOut_T_69), eq(UInt<1>("h0"), _myOut_T_73)), myOut_1_0, _GEN_147) @[TPU.scala 272:{84,84}]
    node _GEN_149 = mux(and(eq(UInt<1>("h1"), _myOut_T_69), eq(UInt<1>("h1"), _myOut_T_73)), myOut_1_1, _GEN_148) @[TPU.scala 272:{84,84}]
    node _myOut_myOut_T_69_myOut_T_73 = _GEN_149 @[TPU.scala 272:84]
    node _myOut_T_74 = add(_myOut_myOut_T_69_myOut_T_73, slicedOut_1_0) @[TPU.scala 272:84]
    node _myOut_T_75 = tail(_myOut_T_74, 1) @[TPU.scala 272:84]
    node _myOut_T_76 = asSInt(_myOut_T_75) @[TPU.scala 272:84]
    node _myOut_T_128_T_132 = _myOut_T_76 @[TPU.scala 272:{47,47}]
    node _GEN_150 = mux(and(eq(UInt<1>("h0"), _T_128), eq(UInt<1>("h0"), _T_132)), _myOut_T_128_T_132, _GEN_142) @[TPU.scala 272:{47,47}]
    node _GEN_151 = mux(and(eq(UInt<1>("h0"), _T_128), eq(UInt<1>("h1"), _T_132)), _myOut_T_128_T_132, _GEN_143) @[TPU.scala 272:{47,47}]
    node _GEN_152 = mux(and(eq(UInt<1>("h1"), _T_128), eq(UInt<1>("h0"), _T_132)), _myOut_T_128_T_132, _GEN_144) @[TPU.scala 272:{47,47}]
    node _GEN_153 = mux(and(eq(UInt<1>("h1"), _T_128), eq(UInt<1>("h1"), _T_132)), _myOut_T_128_T_132, _GEN_145) @[TPU.scala 272:{47,47}]
    node _GEN_154 = mux(_T_124, _GEN_150, _GEN_142) @[TPU.scala 271:61]
    node _GEN_155 = mux(_T_124, _GEN_151, _GEN_143) @[TPU.scala 271:61]
    node _GEN_156 = mux(_T_124, _GEN_152, _GEN_144) @[TPU.scala 271:61]
    node _GEN_157 = mux(_T_124, _GEN_153, _GEN_145) @[TPU.scala 271:61]
    node _T_133 = add(UInt<1>("h1"), boundM) @[TPU.scala 271:20]
    node _T_134 = tail(_T_133, 1) @[TPU.scala 271:20]
    node _T_135 = lt(_T_134, UInt<2>("h2")) @[TPU.scala 271:29]
    node _T_136 = add(UInt<1>("h1"), boundN) @[TPU.scala 271:44]
    node _T_137 = tail(_T_136, 1) @[TPU.scala 271:44]
    node _T_138 = lt(_T_137, UInt<2>("h2")) @[TPU.scala 271:53]
    node _T_139 = and(_T_135, _T_138) @[TPU.scala 271:37]
    node _T_140 = add(UInt<1>("h1"), boundM) @[TPU.scala 272:23]
    node _T_141 = tail(_T_140, 1) @[TPU.scala 272:23]
    node _T_142 = or(_T_141, UInt<1>("h0"))
    node _T_143 = bits(_T_142, 0, 0)
    node _T_144 = add(UInt<1>("h1"), boundN) @[TPU.scala 272:37]
    node _T_145 = tail(_T_144, 1) @[TPU.scala 272:37]
    node _T_146 = or(_T_145, UInt<1>("h0"))
    node _T_147 = bits(_T_146, 0, 0)
    node _myOut_T_77 = add(UInt<1>("h1"), boundM) @[TPU.scala 272:60]
    node _myOut_T_78 = tail(_myOut_T_77, 1) @[TPU.scala 272:60]
    node _myOut_T_79 = or(_myOut_T_78, UInt<1>("h0"))
    node _myOut_T_80 = bits(_myOut_T_79, 0, 0)
    node _myOut_T_81 = add(UInt<1>("h1"), boundN) @[TPU.scala 272:74]
    node _myOut_T_82 = tail(_myOut_T_81, 1) @[TPU.scala 272:74]
    node _myOut_T_83 = or(_myOut_T_82, UInt<1>("h0"))
    node _myOut_T_84 = bits(_myOut_T_83, 0, 0)
    node _GEN_158 = validif(and(eq(UInt<1>("h0"), _myOut_T_80), eq(UInt<1>("h0"), _myOut_T_84)), myOut_0_0) @[TPU.scala 272:{84,84}]
    node _GEN_159 = mux(and(eq(UInt<1>("h0"), _myOut_T_80), eq(UInt<1>("h1"), _myOut_T_84)), myOut_0_1, _GEN_158) @[TPU.scala 272:{84,84}]
    node _GEN_160 = mux(and(eq(UInt<1>("h1"), _myOut_T_80), eq(UInt<1>("h0"), _myOut_T_84)), myOut_1_0, _GEN_159) @[TPU.scala 272:{84,84}]
    node _GEN_161 = mux(and(eq(UInt<1>("h1"), _myOut_T_80), eq(UInt<1>("h1"), _myOut_T_84)), myOut_1_1, _GEN_160) @[TPU.scala 272:{84,84}]
    node _myOut_myOut_T_80_myOut_T_84 = _GEN_161 @[TPU.scala 272:84]
    node _myOut_T_85 = add(_myOut_myOut_T_80_myOut_T_84, slicedOut_1_1) @[TPU.scala 272:84]
    node _myOut_T_86 = tail(_myOut_T_85, 1) @[TPU.scala 272:84]
    node _myOut_T_87 = asSInt(_myOut_T_86) @[TPU.scala 272:84]
    node _myOut_T_143_T_147 = _myOut_T_87 @[TPU.scala 272:{47,47}]
    node _GEN_162 = mux(and(eq(UInt<1>("h0"), _T_143), eq(UInt<1>("h0"), _T_147)), _myOut_T_143_T_147, _GEN_154) @[TPU.scala 272:{47,47}]
    node _GEN_163 = mux(and(eq(UInt<1>("h0"), _T_143), eq(UInt<1>("h1"), _T_147)), _myOut_T_143_T_147, _GEN_155) @[TPU.scala 272:{47,47}]
    node _GEN_164 = mux(and(eq(UInt<1>("h1"), _T_143), eq(UInt<1>("h0"), _T_147)), _myOut_T_143_T_147, _GEN_156) @[TPU.scala 272:{47,47}]
    node _GEN_165 = mux(and(eq(UInt<1>("h1"), _T_143), eq(UInt<1>("h1"), _T_147)), _myOut_T_143_T_147, _GEN_157) @[TPU.scala 272:{47,47}]
    node _GEN_166 = mux(_T_139, _GEN_162, _GEN_154) @[TPU.scala 271:61]
    node _GEN_167 = mux(_T_139, _GEN_163, _GEN_155) @[TPU.scala 271:61]
    node _GEN_168 = mux(_T_139, _GEN_164, _GEN_156) @[TPU.scala 271:61]
    node _GEN_169 = mux(_T_139, _GEN_165, _GEN_157) @[TPU.scala 271:61]
    node _GEN_170 = mux(_T_87, _GEN_166, myOut_0_0) @[TPU.scala 267:41 50:22]
    node _GEN_171 = mux(_T_87, _GEN_167, myOut_0_1) @[TPU.scala 267:41 50:22]
    node _GEN_172 = mux(_T_87, _GEN_168, myOut_1_0) @[TPU.scala 267:41 50:22]
    node _GEN_173 = mux(_T_87, _GEN_169, myOut_1_1) @[TPU.scala 267:41 50:22]
    node _GEN_174 = mux(_T_87, UInt<3>("h2"), state) @[TPU.scala 267:41 281:13 44:22]
    node _GEN_175 = mux(_T_24, _GEN_118, _GEN_170) @[TPU.scala 250:96]
    node _GEN_176 = mux(_T_24, _GEN_119, _GEN_171) @[TPU.scala 250:96]
    node _GEN_177 = mux(_T_24, _GEN_120, _GEN_172) @[TPU.scala 250:96]
    node _GEN_178 = mux(_T_24, _GEN_121, _GEN_173) @[TPU.scala 250:96]
    node _GEN_179 = mux(_T_24, UInt<3>("h4"), _GEN_174) @[TPU.scala 250:96 265:13]
    node _T_148 = eq(state, UInt<3>("h4")) @[TPU.scala 333:19]
    node _GEN_180 = mux(_T_148, UInt<3>("h1"), state) @[TPU.scala 333:29 339:11 44:22]
    node _GEN_181 = mux(_T_148, UInt<1>("h1"), b_ready) @[TPU.scala 333:29 340:13 52:24]
    node _WIRE_1_0_0 = asSInt(UInt<32>("h0")) @[TPU.scala 342:{36,36}]
    node _GEN_182 = mux(_T_148, _WIRE_1_0_0, myOut_0_0) @[TPU.scala 333:29 342:11 50:22]
    node _WIRE_1_0_1 = asSInt(UInt<32>("h0")) @[TPU.scala 342:{36,36}]
    node _GEN_183 = mux(_T_148, _WIRE_1_0_1, myOut_0_1) @[TPU.scala 333:29 342:11 50:22]
    node _WIRE_1_1_0 = asSInt(UInt<32>("h0")) @[TPU.scala 342:{36,36}]
    node _GEN_184 = mux(_T_148, _WIRE_1_1_0, myOut_1_0) @[TPU.scala 333:29 342:11 50:22]
    node _WIRE_1_1_1 = asSInt(UInt<32>("h0")) @[TPU.scala 342:{36,36}]
    node _GEN_185 = mux(_T_148, _WIRE_1_1_1, myOut_1_1) @[TPU.scala 333:29 342:11 50:22]
    node _GEN_188 = mux(_T_13, _GEN_70, slicedOut_0_0) @[TPU.scala 212:32 71:26]
    node _GEN_189 = mux(_T_13, _GEN_71, slicedOut_1_0) @[TPU.scala 212:32 71:26]
    node _GEN_190 = mux(_T_13, _GEN_72, slicedOut_0_1) @[TPU.scala 212:32 71:26]
    node _GEN_191 = mux(_T_13, _GEN_73, slicedOut_1_1) @[TPU.scala 212:32 71:26]
    node _GEN_192 = mux(_T_13, _GEN_175, _GEN_182) @[TPU.scala 212:32]
    node _GEN_193 = mux(_T_13, _GEN_176, _GEN_183) @[TPU.scala 212:32]
    node _GEN_194 = mux(_T_13, _GEN_177, _GEN_184) @[TPU.scala 212:32]
    node _GEN_195 = mux(_T_13, _GEN_178, _GEN_185) @[TPU.scala 212:32]
    node _GEN_196 = mux(_T_13, _GEN_179, _GEN_180) @[TPU.scala 212:32]
    node _GEN_197 = mux(_T_13, b_ready, _GEN_181) @[TPU.scala 212:32 52:24]
    node _GEN_198 = mux(_T_6, UInt<3>("h3"), _GEN_196) @[TPU.scala 191:29 195:11]
    node _GEN_199 = mux(_T_6, UInt<1>("h1"), UInt<1>("h0")) @[TPU.scala 191:29 200:28 85:26]
    node _GEN_200 = mux(_T_6, UInt<1>("h0"), _GEN_1) @[TPU.scala 191:29 209:11]
    node _WIRE__0_0 = asSInt(UInt<32>("h0")) @[TPU.scala 210:{58,58}]
    node _GEN_201 = mux(_T_6, _WIRE__0_0, _GEN_188) @[TPU.scala 191:29 210:15]
    node _WIRE__0_1 = asSInt(UInt<32>("h0")) @[TPU.scala 210:{58,58}]
    node _GEN_202 = mux(_T_6, _WIRE__0_1, _GEN_190) @[TPU.scala 191:29 210:15]
    node _WIRE__1_0 = asSInt(UInt<32>("h0")) @[TPU.scala 210:{58,58}]
    node _GEN_203 = mux(_T_6, _WIRE__1_0, _GEN_189) @[TPU.scala 191:29 210:15]
    node _WIRE__1_1 = asSInt(UInt<32>("h0")) @[TPU.scala 210:{58,58}]
    node _GEN_204 = mux(_T_6, _WIRE__1_1, _GEN_191) @[TPU.scala 191:29 210:15]
    node _GEN_207 = mux(_T_6, myOut_0_0, _GEN_192) @[TPU.scala 191:29 50:22]
    node _GEN_208 = mux(_T_6, myOut_0_1, _GEN_193) @[TPU.scala 191:29 50:22]
    node _GEN_209 = mux(_T_6, myOut_1_0, _GEN_194) @[TPU.scala 191:29 50:22]
    node _GEN_210 = mux(_T_6, myOut_1_1, _GEN_195) @[TPU.scala 191:29 50:22]
    node _GEN_211 = mux(_T_6, b_ready, _GEN_197) @[TPU.scala 191:29 52:24]
    node _GEN_212 = mux(_T_3, _GEN_52, paddedB_0_0) @[TPU.scala 172:28 67:24]
    node _GEN_213 = mux(_T_3, _GEN_53, paddedB_0_1) @[TPU.scala 172:28 67:24]
    node _GEN_214 = mux(_T_3, _GEN_54, paddedB_1_0) @[TPU.scala 172:28 67:24]
    node _GEN_215 = mux(_T_3, _GEN_55, paddedB_1_1) @[TPU.scala 172:28 67:24]
    node _GEN_216 = mux(_T_3, _GEN_56, slicedB_0_0) @[TPU.scala 172:28 87:19]
    node _GEN_217 = mux(_T_3, _GEN_57, slicedB_0_1) @[TPU.scala 172:28 87:19]
    node _GEN_218 = mux(_T_3, _GEN_58, slicedB_1_0) @[TPU.scala 172:28 87:19]
    node _GEN_219 = mux(_T_3, _GEN_59, slicedB_1_1) @[TPU.scala 172:28 87:19]
    node _GEN_220 = mux(_T_3, _GEN_60, _GEN_199) @[TPU.scala 172:28]
    node _GEN_221 = mux(_T_3, _GEN_61, _GEN_211) @[TPU.scala 172:28]
    node _GEN_223 = mux(_T_3, _GEN_62, _GEN_200) @[TPU.scala 172:28]
    node _GEN_224 = mux(_T_3, _GEN_63, _GEN_198) @[TPU.scala 172:28]
    node _GEN_226 = mux(_T_3, slicedOut_0_0, _GEN_201) @[TPU.scala 172:28 71:26]
    node _GEN_227 = mux(_T_3, slicedOut_0_1, _GEN_202) @[TPU.scala 172:28 71:26]
    node _GEN_228 = mux(_T_3, slicedOut_1_0, _GEN_203) @[TPU.scala 172:28 71:26]
    node _GEN_229 = mux(_T_3, slicedOut_1_1, _GEN_204) @[TPU.scala 172:28 71:26]
    node _GEN_231 = mux(_T_3, myOut_0_0, _GEN_207) @[TPU.scala 172:28 50:22]
    node _GEN_232 = mux(_T_3, myOut_0_1, _GEN_208) @[TPU.scala 172:28 50:22]
    node _GEN_233 = mux(_T_3, myOut_1_0, _GEN_209) @[TPU.scala 172:28 50:22]
    node _GEN_234 = mux(_T_3, myOut_1_1, _GEN_210) @[TPU.scala 172:28 50:22]
    node _GEN_235 = mux(_T_1, _GEN_36, _GEN_224) @[TPU.scala 162:23]
    node _GEN_236 = mux(_T_1, _GEN_37, act_in_0_0) @[TPU.scala 162:23 84:23]
    node _GEN_237 = mux(_T_1, _GEN_38, act_in_0_1) @[TPU.scala 162:23 84:23]
    node _GEN_238 = mux(_T_1, _GEN_39, act_in_1_0) @[TPU.scala 162:23 84:23]
    node _GEN_239 = mux(_T_1, _GEN_40, act_in_1_1) @[TPU.scala 162:23 84:23]
    node _GEN_240 = mux(_T_1, _GEN_41, paddedA_0_0) @[TPU.scala 162:23 66:24]
    node _GEN_241 = mux(_T_1, _GEN_42, paddedA_0_1) @[TPU.scala 162:23 66:24]
    node _GEN_242 = mux(_T_1, _GEN_43, paddedA_1_0) @[TPU.scala 162:23 66:24]
    node _GEN_243 = mux(_T_1, _GEN_44, paddedA_1_1) @[TPU.scala 162:23 66:24]
    node _GEN_244 = mux(_T_1, _GEN_45, slicedA_0_0) @[TPU.scala 162:23 86:15]
    node _GEN_245 = mux(_T_1, _GEN_46, slicedA_0_1) @[TPU.scala 162:23 86:15]
    node _GEN_246 = mux(_T_1, _GEN_47, slicedA_1_0) @[TPU.scala 162:23 86:15]
    node _GEN_247 = mux(_T_1, _GEN_48, slicedA_1_1) @[TPU.scala 162:23 86:15]
    node _GEN_248 = mux(_T_1, _GEN_49, a_ready) @[TPU.scala 162:23 51:24]
    node _GEN_249 = mux(_T_1, UInt<1>("h0"), _GEN_223) @[TPU.scala 162:23 170:13]
    node _GEN_250 = mux(_T_1, paddedB_0_0, _GEN_212) @[TPU.scala 162:23 67:24]
    node _GEN_251 = mux(_T_1, paddedB_0_1, _GEN_213) @[TPU.scala 162:23 67:24]
    node _GEN_252 = mux(_T_1, paddedB_1_0, _GEN_214) @[TPU.scala 162:23 67:24]
    node _GEN_253 = mux(_T_1, paddedB_1_1, _GEN_215) @[TPU.scala 162:23 67:24]
    node _GEN_254 = mux(_T_1, slicedB_0_0, _GEN_216) @[TPU.scala 162:23 87:19]
    node _GEN_255 = mux(_T_1, slicedB_0_1, _GEN_217) @[TPU.scala 162:23 87:19]
    node _GEN_256 = mux(_T_1, slicedB_1_0, _GEN_218) @[TPU.scala 162:23 87:19]
    node _GEN_257 = mux(_T_1, slicedB_1_1, _GEN_219) @[TPU.scala 162:23 87:19]
    node _GEN_258 = mux(_T_1, UInt<1>("h0"), _GEN_220) @[TPU.scala 162:23 85:26]
    node _GEN_259 = mux(_T_1, b_ready, _GEN_221) @[TPU.scala 162:23 52:24]
    node _GEN_262 = mux(_T_1, slicedOut_0_0, _GEN_226) @[TPU.scala 162:23 71:26]
    node _GEN_263 = mux(_T_1, slicedOut_0_1, _GEN_227) @[TPU.scala 162:23 71:26]
    node _GEN_264 = mux(_T_1, slicedOut_1_0, _GEN_228) @[TPU.scala 162:23 71:26]
    node _GEN_265 = mux(_T_1, slicedOut_1_1, _GEN_229) @[TPU.scala 162:23 71:26]
    node _GEN_267 = mux(_T_1, myOut_0_0, _GEN_231) @[TPU.scala 162:23 50:22]
    node _GEN_268 = mux(_T_1, myOut_0_1, _GEN_232) @[TPU.scala 162:23 50:22]
    node _GEN_269 = mux(_T_1, myOut_1_0, _GEN_233) @[TPU.scala 162:23 50:22]
    node _GEN_270 = mux(_T_1, myOut_1_1, _GEN_234) @[TPU.scala 162:23 50:22]
    node wrap = _GEN_2
    node _myOut_WIRE_0_0 = asSInt(UInt<32>("h0")) @[TPU.scala 50:{45,45}]
    node _myOut_WIRE_0_1 = asSInt(UInt<32>("h0")) @[TPU.scala 50:{45,45}]
    node _myOut_WIRE_1_0 = asSInt(UInt<32>("h0")) @[TPU.scala 50:{45,45}]
    node _myOut_WIRE_1_1 = asSInt(UInt<32>("h0")) @[TPU.scala 50:{45,45}]
    node _paddedA_WIRE_0_0 = asSInt(UInt<32>("h0")) @[TPU.scala 66:{61,61}]
    node _paddedA_WIRE_0_1 = asSInt(UInt<32>("h0")) @[TPU.scala 66:{61,61}]
    node _paddedA_WIRE_1_0 = asSInt(UInt<32>("h0")) @[TPU.scala 66:{61,61}]
    node _paddedA_WIRE_1_1 = asSInt(UInt<32>("h0")) @[TPU.scala 66:{61,61}]
    node _paddedB_WIRE_0_0 = asSInt(UInt<32>("h0")) @[TPU.scala 67:{61,61}]
    node _paddedB_WIRE_0_1 = asSInt(UInt<32>("h0")) @[TPU.scala 67:{61,61}]
    node _paddedB_WIRE_1_0 = asSInt(UInt<32>("h0")) @[TPU.scala 67:{61,61}]
    node _paddedB_WIRE_1_1 = asSInt(UInt<32>("h0")) @[TPU.scala 67:{61,61}]
    node _paddedOut_WIRE_0_0 = asSInt(UInt<32>("h0")) @[TPU.scala 68:{63,63}]
    node _paddedOut_WIRE_0_1 = asSInt(UInt<32>("h0")) @[TPU.scala 68:{63,63}]
    node _paddedOut_WIRE_1_0 = asSInt(UInt<32>("h0")) @[TPU.scala 68:{63,63}]
    node _paddedOut_WIRE_1_1 = asSInt(UInt<32>("h0")) @[TPU.scala 68:{63,63}]
    node _slicedA_WIRE_0_0 = asSInt(UInt<32>("h0")) @[TPU.scala 69:{65,65}]
    node _slicedA_WIRE_0_1 = asSInt(UInt<32>("h0")) @[TPU.scala 69:{65,65}]
    node _slicedA_WIRE_1_0 = asSInt(UInt<32>("h0")) @[TPU.scala 69:{65,65}]
    node _slicedA_WIRE_1_1 = asSInt(UInt<32>("h0")) @[TPU.scala 69:{65,65}]
    node _slicedB_WIRE_0_0 = asSInt(UInt<32>("h0")) @[TPU.scala 70:{65,65}]
    node _slicedB_WIRE_0_1 = asSInt(UInt<32>("h0")) @[TPU.scala 70:{65,65}]
    node _slicedB_WIRE_1_0 = asSInt(UInt<32>("h0")) @[TPU.scala 70:{65,65}]
    node _slicedB_WIRE_1_1 = asSInt(UInt<32>("h0")) @[TPU.scala 70:{65,65}]
    node _slicedOut_WIRE_0_0 = asSInt(UInt<32>("h0")) @[TPU.scala 71:{67,67}]
    node _slicedOut_WIRE_0_1 = asSInt(UInt<32>("h0")) @[TPU.scala 71:{67,67}]
    node _slicedOut_WIRE_1_0 = asSInt(UInt<32>("h0")) @[TPU.scala 71:{67,67}]
    node _slicedOut_WIRE_1_1 = asSInt(UInt<32>("h0")) @[TPU.scala 71:{67,67}]
    node sliceWrap = _GEN_3
    node _act_in_WIRE_0_0 = asSInt(UInt<32>("h0")) @[TPU.scala 84:{46,46}]
    node _act_in_WIRE_0_1 = asSInt(UInt<32>("h0")) @[TPU.scala 84:{46,46}]
    node _act_in_WIRE_1_0 = asSInt(UInt<32>("h0")) @[TPU.scala 84:{46,46}]
    node _act_in_WIRE_1_1 = asSInt(UInt<32>("h0")) @[TPU.scala 84:{46,46}]
    node cycleIdxCols_0 = UInt<32>("h0") @[TPU.scala 91:26 96:21]
    node cycleIdxCols_1 = UInt<32>("h0") @[TPU.scala 91:26 96:21]
    node cycleIdxRows_0 = UInt<32>("h0") @[TPU.scala 92:26 97:21]
    node cycleIdxRows_1 = UInt<32>("h0") @[TPU.scala 92:26 97:21]
    node _paddedA_slicedA_0_0_T_3_slicedA_0_0_T_7 = _GEN_7 @[TPU.scala 140:23]
    node _paddedA_slicedA_1_0_T_3_slicedA_1_0_T_7 = _GEN_11 @[TPU.scala 140:23]
    node _paddedB_slicedB_0_0_T_3_slicedB_0_0_T_7 = _GEN_15 @[TPU.scala 143:23]
    node _paddedB_slicedB_0_1_T_3_slicedB_0_1_T_7 = _GEN_19 @[TPU.scala 143:23]
    node _paddedA_slicedA_0_1_T_3_slicedA_0_1_T_7 = _GEN_23 @[TPU.scala 140:23]
    node _paddedA_slicedA_1_1_T_3_slicedA_1_1_T_7 = _GEN_27 @[TPU.scala 140:23]
    node _paddedB_slicedB_1_0_T_3_slicedB_1_0_T_7 = _GEN_31 @[TPU.scala 143:23]
    node _paddedB_slicedB_1_1_T_3_slicedB_1_1_T_7 = _GEN_35 @[TPU.scala 143:23]
    io_a_ready <= a_ready @[TPU.scala 76:14]
    io_b_ready <= b_ready @[TPU.scala 77:14]
    io_out_0_0 <= myOut_0_0 @[TPU.scala 78:10]
    io_out_0_1 <= myOut_0_1 @[TPU.scala 78:10]
    io_out_1_0 <= myOut_1_0 @[TPU.scala 78:10]
    io_out_1_1 <= myOut_1_1 @[TPU.scala 78:10]
    state <= mux(reset, UInt<3>("h0"), _GEN_235) @[TPU.scala 44:{22,22}]
    cycle <= mux(reset, UInt<3>("h0"), _GEN_249) @[Counter.scala 61:{40,40}]
    actReg.clock <= clock
    actReg.reset <= reset
    actReg.io_index <= _actReg_io_index_T_1 @[TPU.scala 82:19]
    actReg.io_a_0_0 <= _GEN_244
    actReg.io_a_0_1 <= _GEN_245
    actReg.io_a_1_0 <= _GEN_246
    actReg.io_a_1_1 <= _GEN_247
    systArr.clock <= clock
    systArr.reset <= reset
    systArr.io_a_in_0 <= actReg.io_a_out_0 @[TPU.scala 83:19]
    systArr.io_a_in_1 <= actReg.io_a_out_1 @[TPU.scala 83:19]
    systArr.io_b_in_0_0 <= _GEN_254
    systArr.io_b_in_0_1 <= _GEN_255
    systArr.io_b_in_1_0 <= _GEN_256
    systArr.io_b_in_1_1 <= _GEN_257
    systArr.io_b_readingin <= _GEN_258
    myOut_0_0 <= mux(reset, _myOut_WIRE_0_0, _GEN_267) @[TPU.scala 50:{22,22}]
    myOut_0_1 <= mux(reset, _myOut_WIRE_0_1, _GEN_268) @[TPU.scala 50:{22,22}]
    myOut_1_0 <= mux(reset, _myOut_WIRE_1_0, _GEN_269) @[TPU.scala 50:{22,22}]
    myOut_1_1 <= mux(reset, _myOut_WIRE_1_1, _GEN_270) @[TPU.scala 50:{22,22}]
    a_ready <= mux(reset, UInt<1>("h1"), _GEN_248) @[TPU.scala 51:{24,24}]
    b_ready <= mux(reset, UInt<1>("h1"), _GEN_259) @[TPU.scala 52:{24,24}]
    paddedA_0_0 <= mux(reset, _paddedA_WIRE_0_0, _GEN_240) @[TPU.scala 66:{24,24}]
    paddedA_0_1 <= mux(reset, _paddedA_WIRE_0_1, _GEN_241) @[TPU.scala 66:{24,24}]
    paddedA_1_0 <= mux(reset, _paddedA_WIRE_1_0, _GEN_242) @[TPU.scala 66:{24,24}]
    paddedA_1_1 <= mux(reset, _paddedA_WIRE_1_1, _GEN_243) @[TPU.scala 66:{24,24}]
    paddedB_0_0 <= mux(reset, _paddedB_WIRE_0_0, _GEN_250) @[TPU.scala 67:{24,24}]
    paddedB_0_1 <= mux(reset, _paddedB_WIRE_0_1, _GEN_251) @[TPU.scala 67:{24,24}]
    paddedB_1_0 <= mux(reset, _paddedB_WIRE_1_0, _GEN_252) @[TPU.scala 67:{24,24}]
    paddedB_1_1 <= mux(reset, _paddedB_WIRE_1_1, _GEN_253) @[TPU.scala 67:{24,24}]
    paddedOut_0_0 <= mux(reset, _paddedOut_WIRE_0_0, paddedOut_0_0) @[TPU.scala 68:{26,26,26}]
    paddedOut_0_1 <= mux(reset, _paddedOut_WIRE_0_1, paddedOut_0_1) @[TPU.scala 68:{26,26,26}]
    paddedOut_1_0 <= mux(reset, _paddedOut_WIRE_1_0, paddedOut_1_0) @[TPU.scala 68:{26,26,26}]
    paddedOut_1_1 <= mux(reset, _paddedOut_WIRE_1_1, paddedOut_1_1) @[TPU.scala 68:{26,26,26}]
    slicedA_0_0 <= mux(reset, _slicedA_WIRE_0_0, _paddedA_slicedA_0_0_T_3_slicedA_0_0_T_7) @[TPU.scala 140:23 69:{24,24}]
    slicedA_0_1 <= mux(reset, _slicedA_WIRE_0_1, _paddedA_slicedA_0_1_T_3_slicedA_0_1_T_7) @[TPU.scala 140:23 69:{24,24}]
    slicedA_1_0 <= mux(reset, _slicedA_WIRE_1_0, _paddedA_slicedA_1_0_T_3_slicedA_1_0_T_7) @[TPU.scala 140:23 69:{24,24}]
    slicedA_1_1 <= mux(reset, _slicedA_WIRE_1_1, _paddedA_slicedA_1_1_T_3_slicedA_1_1_T_7) @[TPU.scala 140:23 69:{24,24}]
    slicedB_0_0 <= mux(reset, _slicedB_WIRE_0_0, _paddedB_slicedB_0_0_T_3_slicedB_0_0_T_7) @[TPU.scala 143:23 70:{24,24}]
    slicedB_0_1 <= mux(reset, _slicedB_WIRE_0_1, _paddedB_slicedB_0_1_T_3_slicedB_0_1_T_7) @[TPU.scala 143:23 70:{24,24}]
    slicedB_1_0 <= mux(reset, _slicedB_WIRE_1_0, _paddedB_slicedB_1_0_T_3_slicedB_1_0_T_7) @[TPU.scala 143:23 70:{24,24}]
    slicedB_1_1 <= mux(reset, _slicedB_WIRE_1_1, _paddedB_slicedB_1_1_T_3_slicedB_1_1_T_7) @[TPU.scala 143:23 70:{24,24}]
    slicedOut_0_0 <= mux(reset, _slicedOut_WIRE_0_0, _GEN_262) @[TPU.scala 71:{26,26}]
    slicedOut_0_1 <= mux(reset, _slicedOut_WIRE_0_1, _GEN_263) @[TPU.scala 71:{26,26}]
    slicedOut_1_0 <= mux(reset, _slicedOut_WIRE_1_0, _GEN_264) @[TPU.scala 71:{26,26}]
    slicedOut_1_1 <= mux(reset, _slicedOut_WIRE_1_1, _GEN_265) @[TPU.scala 71:{26,26}]
    act_in_0_0 <= mux(reset, _act_in_WIRE_0_0, _GEN_236) @[TPU.scala 84:{23,23}]
    act_in_0_1 <= mux(reset, _act_in_WIRE_0_1, _GEN_237) @[TPU.scala 84:{23,23}]
    act_in_1_0 <= mux(reset, _act_in_WIRE_1_0, _GEN_238) @[TPU.scala 84:{23,23}]
    act_in_1_1 <= mux(reset, _act_in_WIRE_1_1, _GEN_239) @[TPU.scala 84:{23,23}]
    printf(clock, and(and(and(and(and(UInt<1>("h1"), eq(_T_1, UInt<1>("h0"))), eq(_T_3, UInt<1>("h0"))), _T_6), _T_8), UInt<1>("h1")), "PADDED THING\n") : printf @[TPU.scala 196:11]
    printf(clock, and(and(and(and(and(UInt<1>("h1"), eq(_T_1, UInt<1>("h0"))), eq(_T_3, UInt<1>("h0"))), _T_6), _T_10), UInt<1>("h1")), "Vec(%d, %d)\n", io_out_0_0, io_out_0_1) : printf_1 @[TPU.scala 198:13]
    printf(clock, and(and(and(and(and(UInt<1>("h1"), eq(_T_1, UInt<1>("h0"))), eq(_T_3, UInt<1>("h0"))), _T_6), _T_12), UInt<1>("h1")), "Vec(%d, %d)\n", io_out_1_0, io_out_1_1) : printf_2 @[TPU.scala 198:13]
    printf(clock, and(and(and(and(and(and(and(UInt<1>("h1"), eq(_T_1, UInt<1>("h0"))), eq(_T_3, UInt<1>("h0"))), eq(_T_6, UInt<1>("h0"))), _T_13), _T_24), _T_26), UInt<1>("h1")), "%d %d\n", boundM, boundN) : printf_3 @[TPU.scala 252:13]
