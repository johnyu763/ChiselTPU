circuit ChiselTPU :
  module ActReg :
    input clock : Clock
    input reset : UInt<1>
    input io_index : UInt<4>
    input io_a_0_0 : SInt<32>
    input io_a_0_1 : SInt<32>
    input io_a_0_2 : SInt<32>
    input io_a_1_0 : SInt<32>
    input io_a_1_1 : SInt<32>
    input io_a_1_2 : SInt<32>
    input io_a_2_0 : SInt<32>
    input io_a_2_1 : SInt<32>
    input io_a_2_2 : SInt<32>
    output io_a_out_0 : SInt<32>
    output io_a_out_1 : SInt<32>
    output io_a_out_2 : SInt<32>

    node _T = lt(io_index, UInt<3>("h5")) @[TPU.scala 282:19]
    node _T_1 = asSInt(io_index) @[TPU.scala 284:35]
    node _T_2 = sub(asSInt(UInt<3>("h2")), _T_1) @[TPU.scala 284:25]
    node _T_3 = tail(_T_2, 1) @[TPU.scala 284:25]
    node _T_4 = asSInt(_T_3) @[TPU.scala 284:25]
    node _T_5 = leq(_T_4, asSInt(UInt<1>("h0"))) @[TPU.scala 284:42]
    node _T_6 = asSInt(io_index) @[TPU.scala 284:77]
    node _T_7 = sub(asSInt(UInt<4>("h5")), _T_6) @[TPU.scala 284:67]
    node _T_8 = tail(_T_7, 1) @[TPU.scala 284:67]
    node _T_9 = asSInt(_T_8) @[TPU.scala 284:67]
    node _T_10 = gt(_T_9, asSInt(UInt<1>("h0"))) @[TPU.scala 284:84]
    node _T_11 = and(_T_5, _T_10) @[TPU.scala 284:49]
    node _io_a_out_0_T = sub(UInt<3>("h4"), io_index) @[TPU.scala 285:55]
    node _io_a_out_0_T_1 = tail(_io_a_out_0_T, 1) @[TPU.scala 285:55]
    node _io_a_out_0_T_2 = bits(_io_a_out_0_T_1, 1, 0)
    node _GEN_0 = validif(eq(UInt<1>("h0"), _io_a_out_0_T_2), io_a_0_0) @[TPU.scala 285:{25,25}]
    node _GEN_1 = mux(eq(UInt<1>("h1"), _io_a_out_0_T_2), io_a_1_0, _GEN_0) @[TPU.scala 285:{25,25}]
    node _GEN_2 = mux(eq(UInt<2>("h2"), _io_a_out_0_T_2), io_a_2_0, _GEN_1) @[TPU.scala 285:{25,25}]
    node _io_a_io_a_out_0_T_2_0 = _GEN_2 @[TPU.scala 285:25]
    node _GEN_3 = mux(_T_11, _io_a_io_a_out_0_T_2_0, asSInt(UInt<1>("h0"))) @[TPU.scala 284:90 285:25 288:25]
    node _T_12 = asSInt(io_index) @[TPU.scala 284:35]
    node _T_13 = sub(asSInt(UInt<2>("h1")), _T_12) @[TPU.scala 284:25]
    node _T_14 = tail(_T_13, 1) @[TPU.scala 284:25]
    node _T_15 = asSInt(_T_14) @[TPU.scala 284:25]
    node _T_16 = leq(_T_15, asSInt(UInt<1>("h0"))) @[TPU.scala 284:42]
    node _T_17 = asSInt(io_index) @[TPU.scala 284:77]
    node _T_18 = sub(asSInt(UInt<4>("h4")), _T_17) @[TPU.scala 284:67]
    node _T_19 = tail(_T_18, 1) @[TPU.scala 284:67]
    node _T_20 = asSInt(_T_19) @[TPU.scala 284:67]
    node _T_21 = gt(_T_20, asSInt(UInt<1>("h0"))) @[TPU.scala 284:84]
    node _T_22 = and(_T_16, _T_21) @[TPU.scala 284:49]
    node _io_a_out_1_T = sub(UInt<2>("h3"), io_index) @[TPU.scala 285:55]
    node _io_a_out_1_T_1 = tail(_io_a_out_1_T, 1) @[TPU.scala 285:55]
    node _io_a_out_1_T_2 = bits(_io_a_out_1_T_1, 1, 0)
    node _GEN_4 = validif(eq(UInt<1>("h0"), _io_a_out_1_T_2), io_a_0_1) @[TPU.scala 285:{25,25}]
    node _GEN_5 = mux(eq(UInt<1>("h1"), _io_a_out_1_T_2), io_a_1_1, _GEN_4) @[TPU.scala 285:{25,25}]
    node _GEN_6 = mux(eq(UInt<2>("h2"), _io_a_out_1_T_2), io_a_2_1, _GEN_5) @[TPU.scala 285:{25,25}]
    node _io_a_io_a_out_1_T_2_1 = _GEN_6 @[TPU.scala 285:25]
    node _GEN_7 = mux(_T_22, _io_a_io_a_out_1_T_2_1, asSInt(UInt<1>("h0"))) @[TPU.scala 284:90 285:25 288:25]
    node _T_23 = asSInt(io_index) @[TPU.scala 284:35]
    node _T_24 = sub(asSInt(UInt<1>("h0")), _T_23) @[TPU.scala 284:25]
    node _T_25 = tail(_T_24, 1) @[TPU.scala 284:25]
    node _T_26 = asSInt(_T_25) @[TPU.scala 284:25]
    node _T_27 = leq(_T_26, asSInt(UInt<1>("h0"))) @[TPU.scala 284:42]
    node _T_28 = asSInt(io_index) @[TPU.scala 284:77]
    node _T_29 = sub(asSInt(UInt<3>("h3")), _T_28) @[TPU.scala 284:67]
    node _T_30 = tail(_T_29, 1) @[TPU.scala 284:67]
    node _T_31 = asSInt(_T_30) @[TPU.scala 284:67]
    node _T_32 = gt(_T_31, asSInt(UInt<1>("h0"))) @[TPU.scala 284:84]
    node _T_33 = and(_T_27, _T_32) @[TPU.scala 284:49]
    node _io_a_out_2_T = sub(UInt<2>("h2"), io_index) @[TPU.scala 285:55]
    node _io_a_out_2_T_1 = tail(_io_a_out_2_T, 1) @[TPU.scala 285:55]
    node _io_a_out_2_T_2 = bits(_io_a_out_2_T_1, 1, 0)
    node _GEN_8 = validif(eq(UInt<1>("h0"), _io_a_out_2_T_2), io_a_0_2) @[TPU.scala 285:{25,25}]
    node _GEN_9 = mux(eq(UInt<1>("h1"), _io_a_out_2_T_2), io_a_1_2, _GEN_8) @[TPU.scala 285:{25,25}]
    node _GEN_10 = mux(eq(UInt<2>("h2"), _io_a_out_2_T_2), io_a_2_2, _GEN_9) @[TPU.scala 285:{25,25}]
    node _io_a_io_a_out_2_T_2_2 = _GEN_10 @[TPU.scala 285:25]
    node _GEN_11 = mux(_T_33, _io_a_io_a_out_2_T_2_2, asSInt(UInt<1>("h0"))) @[TPU.scala 284:90 285:25 288:25]
    node _GEN_12 = mux(_T, _GEN_3, asSInt(UInt<1>("h0"))) @[TPU.scala 282:35 294:23]
    node _GEN_13 = mux(_T, _GEN_7, asSInt(UInt<1>("h0"))) @[TPU.scala 282:35 294:23]
    node _GEN_14 = mux(_T, _GEN_11, asSInt(UInt<1>("h0"))) @[TPU.scala 282:35 294:23]
    io_a_out_0 <= _GEN_12
    io_a_out_1 <= _GEN_13
    io_a_out_2 <= _GEN_14

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
      reset => (UInt<1>("h0"), a_reg_0_0) @[TPU.scala 311:20]
    reg a_reg_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_0_1) @[TPU.scala 311:20]
    reg a_reg_0_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_0_2) @[TPU.scala 311:20]
    reg a_reg_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_1_0) @[TPU.scala 311:20]
    reg a_reg_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_1_1) @[TPU.scala 311:20]
    reg a_reg_1_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_1_2) @[TPU.scala 311:20]
    reg a_reg_2_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_2_0) @[TPU.scala 311:20]
    reg a_reg_2_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_2_1) @[TPU.scala 311:20]
    reg a_reg_2_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_2_2) @[TPU.scala 311:20]
    reg b_reg_0_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_0_0) @[TPU.scala 312:20]
    reg b_reg_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_0_1) @[TPU.scala 312:20]
    reg b_reg_0_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_0_2) @[TPU.scala 312:20]
    reg b_reg_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_1_0) @[TPU.scala 312:20]
    reg b_reg_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_1_1) @[TPU.scala 312:20]
    reg b_reg_1_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_1_2) @[TPU.scala 312:20]
    reg b_reg_2_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_2_0) @[TPU.scala 312:20]
    reg b_reg_2_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_2_1) @[TPU.scala 312:20]
    reg b_reg_2_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_2_2) @[TPU.scala 312:20]
    reg cms_reg_0_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_0_0) @[TPU.scala 313:22]
    reg cms_reg_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_0_1) @[TPU.scala 313:22]
    reg cms_reg_0_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_0_2) @[TPU.scala 313:22]
    reg cms_reg_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_1_0) @[TPU.scala 313:22]
    reg cms_reg_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_1_1) @[TPU.scala 313:22]
    reg cms_reg_1_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_1_2) @[TPU.scala 313:22]
    reg cms_reg_2_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_2_0) @[TPU.scala 313:22]
    reg cms_reg_2_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_2_1) @[TPU.scala 313:22]
    reg cms_reg_2_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_2_2) @[TPU.scala 313:22]
    node _io_debug_00_T = mul(io_a_in_0, b_reg_0_0) @[TPU.scala 317:31]
    node _GEN_0 = mux(io_b_readingin, io_b_in_0_0, b_reg_0_0) @[TPU.scala 318:25 319:13 321:13]
    node _GEN_1 = mux(io_b_readingin, io_b_in_0_1, b_reg_0_1) @[TPU.scala 318:25 319:13 321:13]
    node _GEN_2 = mux(io_b_readingin, io_b_in_0_2, b_reg_0_2) @[TPU.scala 318:25 319:13 321:13]
    node _GEN_3 = mux(io_b_readingin, io_b_in_1_0, b_reg_1_0) @[TPU.scala 318:25 319:13 321:13]
    node _GEN_4 = mux(io_b_readingin, io_b_in_1_1, b_reg_1_1) @[TPU.scala 318:25 319:13 321:13]
    node _GEN_5 = mux(io_b_readingin, io_b_in_1_2, b_reg_1_2) @[TPU.scala 318:25 319:13 321:13]
    node _GEN_6 = mux(io_b_readingin, io_b_in_2_0, b_reg_2_0) @[TPU.scala 318:25 319:13 321:13]
    node _GEN_7 = mux(io_b_readingin, io_b_in_2_1, b_reg_2_1) @[TPU.scala 318:25 319:13 321:13]
    node _GEN_8 = mux(io_b_readingin, io_b_in_2_2, b_reg_2_2) @[TPU.scala 318:25 319:13 321:13]
    node _cmp_input_0_0_T = mul(io_a_in_0, b_reg_0_0) @[TPU.scala 331:60]
    node _cmp_input_0_1_T = mul(a_reg_0_0, b_reg_0_1) @[TPU.scala 334:66]
    node _cmp_input_0_2_T = mul(a_reg_0_1, b_reg_0_2) @[TPU.scala 334:66]
    node _cmp_input_1_0_T = mul(io_a_in_1, b_reg_1_0) @[TPU.scala 339:61]
    node _cmp_input_1_0_T_1 = add(_cmp_input_1_0_T, cms_reg_0_0) @[TPU.scala 339:82]
    node _cmp_input_1_0_T_2 = tail(_cmp_input_1_0_T_1, 1) @[TPU.scala 339:82]
    node _cmp_input_1_0_T_3 = asSInt(_cmp_input_1_0_T_2) @[TPU.scala 339:82]
    node _cmp_input_1_1_T = mul(a_reg_1_0, b_reg_1_1) @[TPU.scala 342:67]
    node _cmp_input_1_1_T_1 = add(_cmp_input_1_1_T, cms_reg_0_1) @[TPU.scala 342:88]
    node _cmp_input_1_1_T_2 = tail(_cmp_input_1_1_T_1, 1) @[TPU.scala 342:88]
    node _cmp_input_1_1_T_3 = asSInt(_cmp_input_1_1_T_2) @[TPU.scala 342:88]
    node _cmp_input_1_2_T = mul(a_reg_1_1, b_reg_1_2) @[TPU.scala 342:67]
    node _cmp_input_1_2_T_1 = add(_cmp_input_1_2_T, cms_reg_0_2) @[TPU.scala 342:88]
    node _cmp_input_1_2_T_2 = tail(_cmp_input_1_2_T_1, 1) @[TPU.scala 342:88]
    node _cmp_input_1_2_T_3 = asSInt(_cmp_input_1_2_T_2) @[TPU.scala 342:88]
    node _cmp_input_2_0_T = mul(io_a_in_2, b_reg_2_0) @[TPU.scala 339:61]
    node _cmp_input_2_0_T_1 = add(_cmp_input_2_0_T, cms_reg_1_0) @[TPU.scala 339:82]
    node _cmp_input_2_0_T_2 = tail(_cmp_input_2_0_T_1, 1) @[TPU.scala 339:82]
    node _cmp_input_2_0_T_3 = asSInt(_cmp_input_2_0_T_2) @[TPU.scala 339:82]
    node _cmp_input_2_1_T = mul(a_reg_2_0, b_reg_2_1) @[TPU.scala 342:67]
    node _cmp_input_2_1_T_1 = add(_cmp_input_2_1_T, cms_reg_1_1) @[TPU.scala 342:88]
    node _cmp_input_2_1_T_2 = tail(_cmp_input_2_1_T_1, 1) @[TPU.scala 342:88]
    node _cmp_input_2_1_T_3 = asSInt(_cmp_input_2_1_T_2) @[TPU.scala 342:88]
    node _cmp_input_2_2_T = mul(a_reg_2_1, b_reg_2_2) @[TPU.scala 342:67]
    node _cmp_input_2_2_T_1 = add(_cmp_input_2_2_T, cms_reg_1_2) @[TPU.scala 342:88]
    node _cmp_input_2_2_T_2 = tail(_cmp_input_2_2_T_1, 1) @[TPU.scala 342:88]
    node _cmp_input_2_2_T_3 = asSInt(_cmp_input_2_2_T_2) @[TPU.scala 342:88]
    node cmp_input_0_0 = asSInt(bits(_cmp_input_0_0_T, 31, 0)) @[TPU.scala 326:25 331:43]
    node cmp_input_0_1 = asSInt(bits(_cmp_input_0_1_T, 31, 0)) @[TPU.scala 326:25 334:43]
    node cmp_input_0_2 = asSInt(bits(_cmp_input_0_2_T, 31, 0)) @[TPU.scala 326:25 334:43]
    node cmp_input_1_0 = asSInt(bits(_cmp_input_1_0_T_3, 31, 0)) @[TPU.scala 326:25 339:43]
    node cmp_input_1_1 = asSInt(bits(_cmp_input_1_1_T_3, 31, 0)) @[TPU.scala 326:25 342:43]
    node cmp_input_1_2 = asSInt(bits(_cmp_input_1_2_T_3, 31, 0)) @[TPU.scala 326:25 342:43]
    node cmp_input_2_0 = asSInt(bits(_cmp_input_2_0_T_3, 31, 0)) @[TPU.scala 326:25 339:43]
    node cmp_input_2_1 = asSInt(bits(_cmp_input_2_1_T_3, 31, 0)) @[TPU.scala 326:25 342:43]
    node cmp_input_2_2 = asSInt(bits(_cmp_input_2_2_T_3, 31, 0)) @[TPU.scala 326:25 342:43]
    io_out_0 <= cms_reg_2_0 @[TPU.scala 324:19]
    io_out_1 <= cms_reg_2_1 @[TPU.scala 324:19]
    io_out_2 <= cms_reg_2_2 @[TPU.scala 324:19]
    io_cmp_debug_0_0 <= cms_reg_0_0 @[TPU.scala 314:18]
    io_cmp_debug_0_1 <= cms_reg_0_1 @[TPU.scala 314:18]
    io_cmp_debug_0_2 <= cms_reg_0_2 @[TPU.scala 314:18]
    io_cmp_debug_1_0 <= cms_reg_1_0 @[TPU.scala 314:18]
    io_cmp_debug_1_1 <= cms_reg_1_1 @[TPU.scala 314:18]
    io_cmp_debug_1_2 <= cms_reg_1_2 @[TPU.scala 314:18]
    io_cmp_debug_2_0 <= cms_reg_2_0 @[TPU.scala 314:18]
    io_cmp_debug_2_1 <= cms_reg_2_1 @[TPU.scala 314:18]
    io_cmp_debug_2_2 <= cms_reg_2_2 @[TPU.scala 314:18]
    io_debug_b_regs_0_0 <= b_reg_0_0 @[TPU.scala 315:21]
    io_debug_b_regs_0_1 <= b_reg_0_1 @[TPU.scala 315:21]
    io_debug_b_regs_0_2 <= b_reg_0_2 @[TPU.scala 315:21]
    io_debug_b_regs_1_0 <= b_reg_1_0 @[TPU.scala 315:21]
    io_debug_b_regs_1_1 <= b_reg_1_1 @[TPU.scala 315:21]
    io_debug_b_regs_1_2 <= b_reg_1_2 @[TPU.scala 315:21]
    io_debug_b_regs_2_0 <= b_reg_2_0 @[TPU.scala 315:21]
    io_debug_b_regs_2_1 <= b_reg_2_1 @[TPU.scala 315:21]
    io_debug_b_regs_2_2 <= b_reg_2_2 @[TPU.scala 315:21]
    io_debug_a_regs_0_0 <= a_reg_0_0 @[TPU.scala 316:21]
    io_debug_a_regs_0_1 <= a_reg_0_1 @[TPU.scala 316:21]
    io_debug_a_regs_0_2 <= a_reg_0_2 @[TPU.scala 316:21]
    io_debug_a_regs_1_0 <= a_reg_1_0 @[TPU.scala 316:21]
    io_debug_a_regs_1_1 <= a_reg_1_1 @[TPU.scala 316:21]
    io_debug_a_regs_1_2 <= a_reg_1_2 @[TPU.scala 316:21]
    io_debug_a_regs_2_0 <= a_reg_2_0 @[TPU.scala 316:21]
    io_debug_a_regs_2_1 <= a_reg_2_1 @[TPU.scala 316:21]
    io_debug_a_regs_2_2 <= a_reg_2_2 @[TPU.scala 316:21]
    io_debug_00 <= asSInt(bits(_io_debug_00_T, 31, 0)) @[TPU.scala 317:17]
    a_reg_0_0 <= io_a_in_0 @[TPU.scala 332:39]
    a_reg_0_1 <= a_reg_0_0 @[TPU.scala 335:39]
    a_reg_0_2 <= a_reg_0_1 @[TPU.scala 335:39]
    a_reg_1_0 <= io_a_in_1 @[TPU.scala 340:39]
    a_reg_1_1 <= a_reg_1_0 @[TPU.scala 343:39]
    a_reg_1_2 <= a_reg_1_1 @[TPU.scala 343:39]
    a_reg_2_0 <= io_a_in_2 @[TPU.scala 340:39]
    a_reg_2_1 <= a_reg_2_0 @[TPU.scala 343:39]
    a_reg_2_2 <= a_reg_2_1 @[TPU.scala 343:39]
    b_reg_0_0 <= _GEN_0
    b_reg_0_1 <= _GEN_1
    b_reg_0_2 <= _GEN_2
    b_reg_1_0 <= _GEN_3
    b_reg_1_1 <= _GEN_4
    b_reg_1_2 <= _GEN_5
    b_reg_2_0 <= _GEN_6
    b_reg_2_1 <= _GEN_7
    b_reg_2_2 <= _GEN_8
    cms_reg_0_0 <= cmp_input_0_0 @[TPU.scala 346:21]
    cms_reg_0_1 <= cmp_input_0_1 @[TPU.scala 346:21]
    cms_reg_0_2 <= cmp_input_0_2 @[TPU.scala 346:21]
    cms_reg_1_0 <= cmp_input_1_0 @[TPU.scala 346:21]
    cms_reg_1_1 <= cmp_input_1_1 @[TPU.scala 346:21]
    cms_reg_1_2 <= cmp_input_1_2 @[TPU.scala 346:21]
    cms_reg_2_0 <= cmp_input_2_0 @[TPU.scala 346:21]
    cms_reg_2_1 <= cmp_input_2_1 @[TPU.scala 346:21]
    cms_reg_2_2 <= cmp_input_2_2 @[TPU.scala 346:21]

  module ChiselTPU :
    input clock : Clock
    input reset : UInt<1>
    output io_a_ready : UInt<1>
    input io_a_valid : UInt<1>
    input io_a_bits_0_0 : SInt<32>
    input io_a_bits_0_1 : SInt<32>
    input io_a_bits_0_2 : SInt<32>
    input io_a_bits_1_0 : SInt<32>
    input io_a_bits_1_1 : SInt<32>
    input io_a_bits_1_2 : SInt<32>
    input io_a_bits_2_0 : SInt<32>
    input io_a_bits_2_1 : SInt<32>
    input io_a_bits_2_2 : SInt<32>
    output io_b_ready : UInt<1>
    input io_b_valid : UInt<1>
    input io_b_bits_0_0 : SInt<32>
    input io_b_bits_0_1 : SInt<32>
    input io_b_bits_0_2 : SInt<32>
    input io_b_bits_1_0 : SInt<32>
    input io_b_bits_1_1 : SInt<32>
    input io_b_bits_1_2 : SInt<32>
    input io_b_bits_2_0 : SInt<32>
    input io_b_bits_2_1 : SInt<32>
    input io_b_bits_2_2 : SInt<32>
    output io_out_0_0 : SInt<32>
    output io_out_0_1 : SInt<32>
    output io_out_0_2 : SInt<32>
    output io_out_1_0 : SInt<32>
    output io_out_1_1 : SInt<32>
    output io_out_1_2 : SInt<32>
    output io_out_2_0 : SInt<32>
    output io_out_2_1 : SInt<32>
    output io_out_2_2 : SInt<32>
    output io_debug_1_0_0 : SInt<32>
    output io_debug_1_0_1 : SInt<32>
    output io_debug_1_0_2 : SInt<32>
    output io_debug_1_1_0 : SInt<32>
    output io_debug_1_1_1 : SInt<32>
    output io_debug_1_1_2 : SInt<32>
    output io_debug_1_2_0 : SInt<32>
    output io_debug_1_2_1 : SInt<32>
    output io_debug_1_2_2 : SInt<32>
    output io_debug_a_out_0 : SInt<32>
    output io_debug_a_out_1 : SInt<32>
    output io_debug_a_out_2 : SInt<32>
    output io_debug_systreg_out_0 : SInt<32>
    output io_debug_systreg_out_1 : SInt<32>
    output io_debug_systreg_out_2 : SInt<32>
    output io_debug_a_regs_0_0 : SInt<32>
    output io_debug_a_regs_0_1 : SInt<32>
    output io_debug_a_regs_0_2 : SInt<32>
    output io_debug_a_regs_1_0 : SInt<32>
    output io_debug_a_regs_1_1 : SInt<32>
    output io_debug_a_regs_1_2 : SInt<32>
    output io_debug_a_regs_2_0 : SInt<32>
    output io_debug_a_regs_2_1 : SInt<32>
    output io_debug_a_regs_2_2 : SInt<32>
    output io_debug_b_regs_0_0 : SInt<32>
    output io_debug_b_regs_0_1 : SInt<32>
    output io_debug_b_regs_0_2 : SInt<32>
    output io_debug_b_regs_1_0 : SInt<32>
    output io_debug_b_regs_1_1 : SInt<32>
    output io_debug_b_regs_1_2 : SInt<32>
    output io_debug_b_regs_2_0 : SInt<32>
    output io_debug_b_regs_2_1 : SInt<32>
    output io_debug_b_regs_2_2 : SInt<32>
    output io_debug_cycleOut : UInt<32>
    output io_debug_00 : SInt<32>
    output io_debug_cycleIdxCols_0 : UInt<32>
    output io_debug_cycleIdxCols_1 : UInt<32>
    output io_debug_cycleIdxCols_2 : UInt<32>
    output io_debug_cycleIdxRows_0 : UInt<32>
    output io_debug_cycleIdxRows_1 : UInt<32>
    output io_debug_cycleIdxRows_2 : UInt<32>
    output io_debug_cycleIdx : UInt<32>
    output io_debug_systout_upperLim : SInt<32>

    inst actReg of ActReg @[TPU.scala 58:22]
    inst systArr of SystArr @[TPU.scala 59:23]
    reg state : UInt<3>, clock with :
      reset => (UInt<1>("h0"), state) @[TPU.scala 53:22]
    reg cycle : UInt<4>, clock with :
      reset => (UInt<1>("h0"), cycle) @[Counter.scala 61:40]
    node wrap_wrap = eq(cycle, UInt<4>("h9")) @[Counter.scala 73:24]
    node _wrap_value_T = add(cycle, UInt<1>("h1")) @[Counter.scala 77:24]
    node _wrap_value_T_1 = tail(_wrap_value_T, 1) @[Counter.scala 77:24]
    node _GEN_0 = mux(wrap_wrap, UInt<1>("h0"), _wrap_value_T_1) @[Counter.scala 77:15 87:{20,28}]
    node _T = eq(state, UInt<3>("h0")) @[TPU.scala 125:14]
    node _T_2 = eq(state, UInt<3>("h1")) @[TPU.scala 134:19]
    node _T_3 = and(io_b_valid, io_b_ready) @[TPU.scala 135:23]
    node _GEN_56 = mux(_T_3, UInt<1>("h1"), UInt<1>("h0")) @[TPU.scala 135:37 140:32 88:26]
    node _T_4 = eq(state, UInt<3>("h3")) @[TPU.scala 149:19]
    node _GEN_90 = mux(_T_4, UInt<1>("h1"), UInt<1>("h0")) @[TPU.scala 149:32 153:17 81:15]
    node _GEN_131 = mux(_T_2, _GEN_56, _GEN_90) @[TPU.scala 134:28]
    node _GEN_199 = mux(_T, UInt<1>("h0"), _GEN_131) @[TPU.scala 125:23 81:15]
    node counterFlag = _GEN_199 @[TPU.scala 54:25]
    node _GEN_1 = mux(counterFlag, _GEN_0, cycle) @[Counter.scala 118:16 61:40]
    node _GEN_2 = mux(counterFlag, wrap_wrap, UInt<1>("h0")) @[Counter.scala 118:{16,23}]
    reg myOut_0_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), myOut_0_0) @[TPU.scala 60:22]
    reg myOut_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), myOut_0_1) @[TPU.scala 60:22]
    reg myOut_0_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), myOut_0_2) @[TPU.scala 60:22]
    reg myOut_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), myOut_1_0) @[TPU.scala 60:22]
    reg myOut_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), myOut_1_1) @[TPU.scala 60:22]
    reg myOut_1_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), myOut_1_2) @[TPU.scala 60:22]
    reg myOut_2_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), myOut_2_0) @[TPU.scala 60:22]
    reg myOut_2_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), myOut_2_1) @[TPU.scala 60:22]
    reg myOut_2_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), myOut_2_2) @[TPU.scala 60:22]
    reg a_ready : UInt<1>, clock with :
      reset => (UInt<1>("h0"), a_ready) @[TPU.scala 61:24]
    reg b_ready : UInt<1>, clock with :
      reset => (UInt<1>("h0"), b_ready) @[TPU.scala 62:24]
    reg paddedA_0_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), paddedA_0_0) @[TPU.scala 71:24]
    reg paddedA_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), paddedA_0_1) @[TPU.scala 71:24]
    reg paddedA_0_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), paddedA_0_2) @[TPU.scala 71:24]
    reg paddedA_0_3 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), paddedA_0_3) @[TPU.scala 71:24]
    reg paddedA_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), paddedA_1_0) @[TPU.scala 71:24]
    reg paddedA_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), paddedA_1_1) @[TPU.scala 71:24]
    reg paddedA_1_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), paddedA_1_2) @[TPU.scala 71:24]
    reg paddedA_1_3 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), paddedA_1_3) @[TPU.scala 71:24]
    reg paddedA_2_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), paddedA_2_0) @[TPU.scala 71:24]
    reg paddedA_2_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), paddedA_2_1) @[TPU.scala 71:24]
    reg paddedA_2_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), paddedA_2_2) @[TPU.scala 71:24]
    reg paddedA_2_3 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), paddedA_2_3) @[TPU.scala 71:24]
    reg paddedA_3_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), paddedA_3_0) @[TPU.scala 71:24]
    reg paddedA_3_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), paddedA_3_1) @[TPU.scala 71:24]
    reg paddedA_3_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), paddedA_3_2) @[TPU.scala 71:24]
    reg paddedA_3_3 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), paddedA_3_3) @[TPU.scala 71:24]
    reg paddedB_0_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), paddedB_0_0) @[TPU.scala 72:24]
    reg paddedB_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), paddedB_0_1) @[TPU.scala 72:24]
    reg paddedB_0_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), paddedB_0_2) @[TPU.scala 72:24]
    reg paddedB_0_3 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), paddedB_0_3) @[TPU.scala 72:24]
    reg paddedB_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), paddedB_1_0) @[TPU.scala 72:24]
    reg paddedB_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), paddedB_1_1) @[TPU.scala 72:24]
    reg paddedB_1_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), paddedB_1_2) @[TPU.scala 72:24]
    reg paddedB_1_3 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), paddedB_1_3) @[TPU.scala 72:24]
    reg paddedB_2_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), paddedB_2_0) @[TPU.scala 72:24]
    reg paddedB_2_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), paddedB_2_1) @[TPU.scala 72:24]
    reg paddedB_2_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), paddedB_2_2) @[TPU.scala 72:24]
    reg paddedB_2_3 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), paddedB_2_3) @[TPU.scala 72:24]
    reg paddedB_3_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), paddedB_3_0) @[TPU.scala 72:24]
    reg paddedB_3_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), paddedB_3_1) @[TPU.scala 72:24]
    reg paddedB_3_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), paddedB_3_2) @[TPU.scala 72:24]
    reg paddedB_3_3 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), paddedB_3_3) @[TPU.scala 72:24]
    reg ASlice_0_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), ASlice_0_0) @[TPU.scala 73:23]
    reg ASlice_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), ASlice_0_1) @[TPU.scala 73:23]
    reg ASlice_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), ASlice_1_0) @[TPU.scala 73:23]
    reg ASlice_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), ASlice_1_1) @[TPU.scala 73:23]
    reg BSlice_0_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), BSlice_0_0) @[TPU.scala 74:23]
    reg BSlice_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), BSlice_0_1) @[TPU.scala 74:23]
    reg BSlice_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), BSlice_1_0) @[TPU.scala 74:23]
    reg BSlice_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), BSlice_1_1) @[TPU.scala 74:23]
    node _actReg_io_index_T = sub(UInt<3>("h5"), cycle) @[TPU.scala 83:35]
    node _actReg_io_index_T_1 = tail(_actReg_io_index_T, 1) @[TPU.scala 83:35]
    reg act_in_0_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), act_in_0_0) @[TPU.scala 86:23]
    reg act_in_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), act_in_0_1) @[TPU.scala 86:23]
    reg act_in_0_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), act_in_0_2) @[TPU.scala 86:23]
    reg act_in_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), act_in_1_0) @[TPU.scala 86:23]
    reg act_in_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), act_in_1_1) @[TPU.scala 86:23]
    reg act_in_1_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), act_in_1_2) @[TPU.scala 86:23]
    reg act_in_2_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), act_in_2_0) @[TPU.scala 86:23]
    reg act_in_2_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), act_in_2_1) @[TPU.scala 86:23]
    reg act_in_2_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), act_in_2_2) @[TPU.scala 86:23]
    node _T_1 = and(io_a_valid, io_a_ready) @[TPU.scala 126:23]
    node _GEN_3 = mux(_T_1, UInt<3>("h1"), state) @[TPU.scala 126:37 127:15 53:22]
    node _GEN_4 = mux(_T_1, io_a_bits_0_0, act_in_0_0) @[TPU.scala 126:37 128:16 86:23]
    node _GEN_5 = mux(_T_1, io_a_bits_0_1, act_in_0_1) @[TPU.scala 126:37 128:16 86:23]
    node _GEN_6 = mux(_T_1, io_a_bits_0_2, act_in_0_2) @[TPU.scala 126:37 128:16 86:23]
    node _GEN_7 = mux(_T_1, io_a_bits_1_0, act_in_1_0) @[TPU.scala 126:37 128:16 86:23]
    node _GEN_8 = mux(_T_1, io_a_bits_1_1, act_in_1_1) @[TPU.scala 126:37 128:16 86:23]
    node _GEN_9 = mux(_T_1, io_a_bits_1_2, act_in_1_2) @[TPU.scala 126:37 128:16 86:23]
    node _GEN_10 = mux(_T_1, io_a_bits_2_0, act_in_2_0) @[TPU.scala 126:37 128:16 86:23]
    node _GEN_11 = mux(_T_1, io_a_bits_2_1, act_in_2_1) @[TPU.scala 126:37 128:16 86:23]
    node _GEN_12 = mux(_T_1, io_a_bits_2_2, act_in_2_2) @[TPU.scala 126:37 128:16 86:23]
    node _GEN_13 = mux(_T_1, io_a_bits_0_0, paddedA_0_0) @[TPU.scala 126:37 43:21 71:24]
    node _GEN_14 = mux(_T_1, io_a_bits_0_1, paddedA_0_1) @[TPU.scala 126:37 43:21 71:24]
    node _GEN_15 = mux(_T_1, io_a_bits_0_2, paddedA_0_2) @[TPU.scala 126:37 43:21 71:24]
    node _GEN_16 = mux(_T_1, asSInt(UInt<1>("h0")), paddedA_0_3) @[TPU.scala 126:37 46:21 71:24]
    node _GEN_17 = mux(_T_1, io_a_bits_1_0, paddedA_1_0) @[TPU.scala 126:37 43:21 71:24]
    node _GEN_18 = mux(_T_1, io_a_bits_1_1, paddedA_1_1) @[TPU.scala 126:37 43:21 71:24]
    node _GEN_19 = mux(_T_1, io_a_bits_1_2, paddedA_1_2) @[TPU.scala 126:37 43:21 71:24]
    node _GEN_20 = mux(_T_1, asSInt(UInt<1>("h0")), paddedA_1_3) @[TPU.scala 126:37 46:21 71:24]
    node _GEN_21 = mux(_T_1, io_a_bits_2_0, paddedA_2_0) @[TPU.scala 126:37 43:21 71:24]
    node _GEN_22 = mux(_T_1, io_a_bits_2_1, paddedA_2_1) @[TPU.scala 126:37 43:21 71:24]
    node _GEN_23 = mux(_T_1, io_a_bits_2_2, paddedA_2_2) @[TPU.scala 126:37 43:21 71:24]
    node _GEN_24 = mux(_T_1, asSInt(UInt<1>("h0")), paddedA_2_3) @[TPU.scala 126:37 46:21 71:24]
    node _GEN_25 = mux(_T_1, asSInt(UInt<1>("h0")), paddedA_3_0) @[TPU.scala 126:37 46:21 71:24]
    node _GEN_26 = mux(_T_1, asSInt(UInt<1>("h0")), paddedA_3_1) @[TPU.scala 126:37 46:21 71:24]
    node _GEN_27 = mux(_T_1, asSInt(UInt<1>("h0")), paddedA_3_2) @[TPU.scala 126:37 46:21 71:24]
    node _GEN_28 = mux(_T_1, asSInt(UInt<1>("h0")), paddedA_3_3) @[TPU.scala 126:37 46:21 71:24]
    node _GEN_29 = mux(_T_1, UInt<1>("h0"), a_ready) @[TPU.scala 126:37 130:17 61:24]
    node _GEN_30 = mux(_T_3, UInt<3>("h3"), state) @[TPU.scala 135:37 136:15 53:22]
    node _GEN_31 = mux(_T_3, io_b_bits_0_0, paddedB_0_0) @[TPU.scala 135:37 43:21 72:24]
    node _GEN_32 = mux(_T_3, io_b_bits_0_1, paddedB_0_1) @[TPU.scala 135:37 43:21 72:24]
    node _GEN_33 = mux(_T_3, io_b_bits_0_2, paddedB_0_2) @[TPU.scala 135:37 43:21 72:24]
    node _GEN_34 = mux(_T_3, asSInt(UInt<1>("h0")), paddedB_0_3) @[TPU.scala 135:37 46:21 72:24]
    node _GEN_35 = mux(_T_3, io_b_bits_1_0, paddedB_1_0) @[TPU.scala 135:37 43:21 72:24]
    node _GEN_36 = mux(_T_3, io_b_bits_1_1, paddedB_1_1) @[TPU.scala 135:37 43:21 72:24]
    node _GEN_37 = mux(_T_3, io_b_bits_1_2, paddedB_1_2) @[TPU.scala 135:37 43:21 72:24]
    node _GEN_38 = mux(_T_3, asSInt(UInt<1>("h0")), paddedB_1_3) @[TPU.scala 135:37 46:21 72:24]
    node _GEN_39 = mux(_T_3, io_b_bits_2_0, paddedB_2_0) @[TPU.scala 135:37 43:21 72:24]
    node _GEN_40 = mux(_T_3, io_b_bits_2_1, paddedB_2_1) @[TPU.scala 135:37 43:21 72:24]
    node _GEN_41 = mux(_T_3, io_b_bits_2_2, paddedB_2_2) @[TPU.scala 135:37 43:21 72:24]
    node _GEN_42 = mux(_T_3, asSInt(UInt<1>("h0")), paddedB_2_3) @[TPU.scala 135:37 46:21 72:24]
    node _GEN_43 = mux(_T_3, asSInt(UInt<1>("h0")), paddedB_3_0) @[TPU.scala 135:37 46:21 72:24]
    node _GEN_44 = mux(_T_3, asSInt(UInt<1>("h0")), paddedB_3_1) @[TPU.scala 135:37 46:21 72:24]
    node _GEN_45 = mux(_T_3, asSInt(UInt<1>("h0")), paddedB_3_2) @[TPU.scala 135:37 46:21 72:24]
    node _GEN_46 = mux(_T_3, asSInt(UInt<1>("h0")), paddedB_3_3) @[TPU.scala 135:37 46:21 72:24]
    node _GEN_47 = mux(_T_3, io_b_bits_0_0, io_b_bits_0_0) @[TPU.scala 135:37 139:25 91:19]
    node _GEN_48 = mux(_T_3, io_b_bits_0_1, io_b_bits_0_1) @[TPU.scala 135:37 139:25 91:19]
    node _GEN_49 = mux(_T_3, io_b_bits_0_2, io_b_bits_0_2) @[TPU.scala 135:37 139:25 91:19]
    node _GEN_50 = mux(_T_3, io_b_bits_1_0, io_b_bits_1_0) @[TPU.scala 135:37 139:25 91:19]
    node _GEN_51 = mux(_T_3, io_b_bits_1_1, io_b_bits_1_1) @[TPU.scala 135:37 139:25 91:19]
    node _GEN_52 = mux(_T_3, io_b_bits_1_2, io_b_bits_1_2) @[TPU.scala 135:37 139:25 91:19]
    node _GEN_53 = mux(_T_3, io_b_bits_2_0, io_b_bits_2_0) @[TPU.scala 135:37 139:25 91:19]
    node _GEN_54 = mux(_T_3, io_b_bits_2_1, io_b_bits_2_1) @[TPU.scala 135:37 139:25 91:19]
    node _GEN_55 = mux(_T_3, io_b_bits_2_2, io_b_bits_2_2) @[TPU.scala 135:37 139:25 91:19]
    node _GEN_57 = mux(_T_3, UInt<1>("h0"), b_ready) @[TPU.scala 135:37 141:17 62:24]
    node _GEN_58 = mux(_T_3, UInt<1>("h0"), _GEN_1) @[TPU.scala 135:37 143:15]
    node _T_5 = eq(cycle, UInt<4>("h9")) @[TPU.scala 154:16]
    node _GEN_59 = mux(_T_5, UInt<3>("h4"), state) @[TPU.scala 154:36 155:13 53:22]
    node _T_6 = geq(cycle, UInt<3>("h4")) @[TPU.scala 157:16]
    node _cycleIdx_T = sub(cycle, UInt<3>("h4")) @[TPU.scala 158:24]
    node _cycleIdx_T_1 = tail(_cycleIdx_T, 1) @[TPU.scala 158:24]
    node _GEN_60 = mux(_T_6, _cycleIdx_T_1, cycle) @[TPU.scala 157:31 158:16 161:16]
    node _T_7 = geq(cycle, UInt<3>("h4")) @[TPU.scala 163:16]
    node _GEN_92 = mux(_T_4, _GEN_60, UInt<1>("h0")) @[TPU.scala 100:12 149:32]
    node _GEN_133 = mux(_T_2, UInt<1>("h0"), _GEN_92) @[TPU.scala 100:12 134:28]
    node _GEN_200 = mux(_T, UInt<1>("h0"), _GEN_133) @[TPU.scala 100:12 125:23]
    node cycleIdx = pad(_GEN_200, 32) @[TPU.scala 94:22]
    node _T_8 = geq(cycleIdx, UInt<1>("h0")) @[TPU.scala 165:22]
    node _T_9 = lt(cycleIdx, UInt<2>("h3")) @[TPU.scala 165:39]
    node _T_10 = and(_T_8, _T_9) @[TPU.scala 165:28]
    node _GEN_61 = mux(_T_10, myOut_1_0, myOut_0_0) @[TPU.scala 165:50 170:31 60:22]
    node _GEN_62 = mux(_T_10, myOut_2_0, myOut_1_0) @[TPU.scala 165:50 170:31 60:22]
    node _GEN_63 = mux(_T_10, systArr.io_out_0, myOut_2_0) @[TPU.scala 165:50 168:31 60:22]
    node _T_11 = geq(cycleIdx, UInt<1>("h1")) @[TPU.scala 165:22]
    node _T_12 = lt(cycleIdx, UInt<3>("h4")) @[TPU.scala 165:39]
    node _T_13 = and(_T_11, _T_12) @[TPU.scala 165:28]
    node _GEN_64 = mux(_T_13, myOut_1_1, myOut_0_1) @[TPU.scala 165:50 170:31 60:22]
    node _GEN_65 = mux(_T_13, myOut_2_1, myOut_1_1) @[TPU.scala 165:50 170:31 60:22]
    node _GEN_66 = mux(_T_13, systArr.io_out_1, myOut_2_1) @[TPU.scala 165:50 168:31 60:22]
    node _T_14 = geq(cycleIdx, UInt<2>("h2")) @[TPU.scala 165:22]
    node _T_15 = lt(cycleIdx, UInt<3>("h5")) @[TPU.scala 165:39]
    node _T_16 = and(_T_14, _T_15) @[TPU.scala 165:28]
    node _GEN_67 = mux(_T_16, myOut_1_2, myOut_0_2) @[TPU.scala 165:50 170:31 60:22]
    node _GEN_68 = mux(_T_16, myOut_2_2, myOut_1_2) @[TPU.scala 165:50 170:31 60:22]
    node _GEN_69 = mux(_T_16, systArr.io_out_2, myOut_2_2) @[TPU.scala 165:50 168:31 60:22]
    node _GEN_70 = mux(_T_7, _GEN_61, myOut_0_0) @[TPU.scala 163:31 60:22]
    node _GEN_71 = mux(_T_7, _GEN_62, myOut_1_0) @[TPU.scala 163:31 60:22]
    node _GEN_72 = mux(_T_7, _GEN_63, myOut_2_0) @[TPU.scala 163:31 60:22]
    node _GEN_73 = mux(_T_7, _GEN_64, myOut_0_1) @[TPU.scala 163:31 60:22]
    node _GEN_74 = mux(_T_7, _GEN_65, myOut_1_1) @[TPU.scala 163:31 60:22]
    node _GEN_75 = mux(_T_7, _GEN_66, myOut_2_1) @[TPU.scala 163:31 60:22]
    node _GEN_76 = mux(_T_7, _GEN_67, myOut_0_2) @[TPU.scala 163:31 60:22]
    node _GEN_77 = mux(_T_7, _GEN_68, myOut_1_2) @[TPU.scala 163:31 60:22]
    node _GEN_78 = mux(_T_7, _GEN_69, myOut_2_2) @[TPU.scala 163:31 60:22]
    node _T_17 = bits(reset, 0, 0) @[TPU.scala 231:11]
    node _T_18 = eq(_T_17, UInt<1>("h0")) @[TPU.scala 231:11]
    node _T_19 = bits(reset, 0, 0) @[TPU.scala 232:11]
    node _T_20 = eq(_T_19, UInt<1>("h0")) @[TPU.scala 232:11]
    node _T_21 = bits(reset, 0, 0) @[TPU.scala 234:13]
    node _T_22 = eq(_T_21, UInt<1>("h0")) @[TPU.scala 234:13]
    node _T_23 = bits(reset, 0, 0) @[TPU.scala 234:13]
    node _T_24 = eq(_T_23, UInt<1>("h0")) @[TPU.scala 234:13]
    node _T_25 = bits(reset, 0, 0) @[TPU.scala 234:13]
    node _T_26 = eq(_T_25, UInt<1>("h0")) @[TPU.scala 234:13]
    node _T_27 = bits(reset, 0, 0) @[TPU.scala 236:11]
    node _T_28 = eq(_T_27, UInt<1>("h0")) @[TPU.scala 236:11]
    node _T_29 = bits(reset, 0, 0) @[TPU.scala 238:13]
    node _T_30 = eq(_T_29, UInt<1>("h0")) @[TPU.scala 238:13]
    node _T_31 = bits(reset, 0, 0) @[TPU.scala 238:13]
    node _T_32 = eq(_T_31, UInt<1>("h0")) @[TPU.scala 238:13]
    node _T_33 = bits(reset, 0, 0) @[TPU.scala 238:13]
    node _T_34 = eq(_T_33, UInt<1>("h0")) @[TPU.scala 238:13]
    node _T_35 = bits(reset, 0, 0) @[TPU.scala 240:11]
    node _T_36 = eq(_T_35, UInt<1>("h0")) @[TPU.scala 240:11]
    node _T_37 = bits(reset, 0, 0) @[TPU.scala 242:13]
    node _T_38 = eq(_T_37, UInt<1>("h0")) @[TPU.scala 242:13]
    node _T_39 = bits(reset, 0, 0) @[TPU.scala 242:13]
    node _T_40 = eq(_T_39, UInt<1>("h0")) @[TPU.scala 242:13]
    node _T_41 = bits(reset, 0, 0) @[TPU.scala 242:13]
    node _T_42 = eq(_T_41, UInt<1>("h0")) @[TPU.scala 242:13]
    node _T_43 = bits(reset, 0, 0) @[TPU.scala 244:11]
    node _T_44 = eq(_T_43, UInt<1>("h0")) @[TPU.scala 244:11]
    node _T_45 = bits(reset, 0, 0) @[TPU.scala 246:13]
    node _T_46 = eq(_T_45, UInt<1>("h0")) @[TPU.scala 246:13]
    node _T_47 = bits(reset, 0, 0) @[TPU.scala 246:13]
    node _T_48 = eq(_T_47, UInt<1>("h0")) @[TPU.scala 246:13]
    node _T_49 = bits(reset, 0, 0) @[TPU.scala 246:13]
    node _T_50 = eq(_T_49, UInt<1>("h0")) @[TPU.scala 246:13]
    node _T_51 = bits(reset, 0, 0) @[TPU.scala 246:13]
    node _T_52 = eq(_T_51, UInt<1>("h0")) @[TPU.scala 246:13]
    node _T_53 = bits(reset, 0, 0) @[TPU.scala 248:12]
    node _T_54 = eq(_T_53, UInt<1>("h0")) @[TPU.scala 248:12]
    node _T_55 = bits(reset, 0, 0) @[TPU.scala 250:13]
    node _T_56 = eq(_T_55, UInt<1>("h0")) @[TPU.scala 250:13]
    node _T_57 = bits(reset, 0, 0) @[TPU.scala 250:13]
    node _T_58 = eq(_T_57, UInt<1>("h0")) @[TPU.scala 250:13]
    node _T_59 = bits(reset, 0, 0) @[TPU.scala 250:13]
    node _T_60 = eq(_T_59, UInt<1>("h0")) @[TPU.scala 250:13]
    node _T_61 = bits(reset, 0, 0) @[TPU.scala 250:13]
    node _T_62 = eq(_T_61, UInt<1>("h0")) @[TPU.scala 250:13]
    node _T_63 = bits(reset, 0, 0) @[TPU.scala 252:11]
    node _T_64 = eq(_T_63, UInt<1>("h0")) @[TPU.scala 252:11]
    node _T_65 = bits(reset, 0, 0) @[TPU.scala 254:13]
    node _T_66 = eq(_T_65, UInt<1>("h0")) @[TPU.scala 254:13]
    node _T_67 = bits(reset, 0, 0) @[TPU.scala 254:13]
    node _T_68 = eq(_T_67, UInt<1>("h0")) @[TPU.scala 254:13]
    node _T_69 = bits(reset, 0, 0) @[TPU.scala 254:13]
    node _T_70 = eq(_T_69, UInt<1>("h0")) @[TPU.scala 254:13]
    node _T_71 = eq(state, UInt<3>("h4")) @[TPU.scala 266:19]
    node _T_72 = bits(reset, 0, 0) @[TPU.scala 267:11]
    node _T_73 = eq(_T_72, UInt<1>("h0")) @[TPU.scala 267:11]
    node _GEN_79 = mux(_T_71, UInt<3>("h1"), state) @[TPU.scala 266:29 268:11 53:22]
    node _GEN_80 = mux(_T_71, UInt<1>("h1"), b_ready) @[TPU.scala 266:29 270:16 78:14]
    node _WIRE_0_0 = asSInt(UInt<32>("h11")) @[TPU.scala 271:{36,36}]
    node _GEN_81 = mux(_T_71, _WIRE_0_0, myOut_0_0) @[TPU.scala 266:29 271:11 60:22]
    node _WIRE_0_1 = asSInt(UInt<32>("h11")) @[TPU.scala 271:{36,36}]
    node _GEN_82 = mux(_T_71, _WIRE_0_1, myOut_0_1) @[TPU.scala 266:29 271:11 60:22]
    node _WIRE_0_2 = asSInt(UInt<32>("h11")) @[TPU.scala 271:{36,36}]
    node _GEN_83 = mux(_T_71, _WIRE_0_2, myOut_0_2) @[TPU.scala 266:29 271:11 60:22]
    node _WIRE_1_0 = asSInt(UInt<32>("h11")) @[TPU.scala 271:{36,36}]
    node _GEN_84 = mux(_T_71, _WIRE_1_0, myOut_1_0) @[TPU.scala 266:29 271:11 60:22]
    node _WIRE_1_1 = asSInt(UInt<32>("h11")) @[TPU.scala 271:{36,36}]
    node _GEN_85 = mux(_T_71, _WIRE_1_1, myOut_1_1) @[TPU.scala 266:29 271:11 60:22]
    node _WIRE_1_2 = asSInt(UInt<32>("h11")) @[TPU.scala 271:{36,36}]
    node _GEN_86 = mux(_T_71, _WIRE_1_2, myOut_1_2) @[TPU.scala 266:29 271:11 60:22]
    node _WIRE_2_0 = asSInt(UInt<32>("h11")) @[TPU.scala 271:{36,36}]
    node _GEN_87 = mux(_T_71, _WIRE_2_0, myOut_2_0) @[TPU.scala 266:29 271:11 60:22]
    node _WIRE_2_1 = asSInt(UInt<32>("h11")) @[TPU.scala 271:{36,36}]
    node _GEN_88 = mux(_T_71, _WIRE_2_1, myOut_2_1) @[TPU.scala 266:29 271:11 60:22]
    node _WIRE_2_2 = asSInt(UInt<32>("h11")) @[TPU.scala 271:{36,36}]
    node _GEN_89 = mux(_T_71, _WIRE_2_2, myOut_2_2) @[TPU.scala 266:29 271:11 60:22]
    node _GEN_91 = mux(_T_4, _GEN_59, _GEN_79) @[TPU.scala 149:32]
    node _GEN_93 = mux(_T_4, _GEN_70, _GEN_81) @[TPU.scala 149:32]
    node _GEN_94 = mux(_T_4, _GEN_71, _GEN_84) @[TPU.scala 149:32]
    node _GEN_95 = mux(_T_4, _GEN_72, _GEN_87) @[TPU.scala 149:32]
    node _GEN_96 = mux(_T_4, _GEN_73, _GEN_82) @[TPU.scala 149:32]
    node _GEN_97 = mux(_T_4, _GEN_74, _GEN_85) @[TPU.scala 149:32]
    node _GEN_98 = mux(_T_4, _GEN_75, _GEN_88) @[TPU.scala 149:32]
    node _GEN_99 = mux(_T_4, _GEN_76, _GEN_83) @[TPU.scala 149:32]
    node _GEN_100 = mux(_T_4, _GEN_77, _GEN_86) @[TPU.scala 149:32]
    node _GEN_101 = mux(_T_4, _GEN_78, _GEN_89) @[TPU.scala 149:32]
    node _GEN_102 = mux(_T_4, b_ready, _GEN_80) @[TPU.scala 149:32 78:14]
    node _GEN_103 = mux(_T_2, _GEN_30, _GEN_91) @[TPU.scala 134:28]
    node _GEN_104 = mux(_T_2, _GEN_31, paddedB_0_0) @[TPU.scala 134:28 72:24]
    node _GEN_105 = mux(_T_2, _GEN_32, paddedB_0_1) @[TPU.scala 134:28 72:24]
    node _GEN_106 = mux(_T_2, _GEN_33, paddedB_0_2) @[TPU.scala 134:28 72:24]
    node _GEN_107 = mux(_T_2, _GEN_34, paddedB_0_3) @[TPU.scala 134:28 72:24]
    node _GEN_108 = mux(_T_2, _GEN_35, paddedB_1_0) @[TPU.scala 134:28 72:24]
    node _GEN_109 = mux(_T_2, _GEN_36, paddedB_1_1) @[TPU.scala 134:28 72:24]
    node _GEN_110 = mux(_T_2, _GEN_37, paddedB_1_2) @[TPU.scala 134:28 72:24]
    node _GEN_111 = mux(_T_2, _GEN_38, paddedB_1_3) @[TPU.scala 134:28 72:24]
    node _GEN_112 = mux(_T_2, _GEN_39, paddedB_2_0) @[TPU.scala 134:28 72:24]
    node _GEN_113 = mux(_T_2, _GEN_40, paddedB_2_1) @[TPU.scala 134:28 72:24]
    node _GEN_114 = mux(_T_2, _GEN_41, paddedB_2_2) @[TPU.scala 134:28 72:24]
    node _GEN_115 = mux(_T_2, _GEN_42, paddedB_2_3) @[TPU.scala 134:28 72:24]
    node _GEN_116 = mux(_T_2, _GEN_43, paddedB_3_0) @[TPU.scala 134:28 72:24]
    node _GEN_117 = mux(_T_2, _GEN_44, paddedB_3_1) @[TPU.scala 134:28 72:24]
    node _GEN_118 = mux(_T_2, _GEN_45, paddedB_3_2) @[TPU.scala 134:28 72:24]
    node _GEN_119 = mux(_T_2, _GEN_46, paddedB_3_3) @[TPU.scala 134:28 72:24]
    node _GEN_120 = mux(_T_2, _GEN_47, io_b_bits_0_0) @[TPU.scala 134:28 91:19]
    node _GEN_121 = mux(_T_2, _GEN_48, io_b_bits_0_1) @[TPU.scala 134:28 91:19]
    node _GEN_122 = mux(_T_2, _GEN_49, io_b_bits_0_2) @[TPU.scala 134:28 91:19]
    node _GEN_123 = mux(_T_2, _GEN_50, io_b_bits_1_0) @[TPU.scala 134:28 91:19]
    node _GEN_124 = mux(_T_2, _GEN_51, io_b_bits_1_1) @[TPU.scala 134:28 91:19]
    node _GEN_125 = mux(_T_2, _GEN_52, io_b_bits_1_2) @[TPU.scala 134:28 91:19]
    node _GEN_126 = mux(_T_2, _GEN_53, io_b_bits_2_0) @[TPU.scala 134:28 91:19]
    node _GEN_127 = mux(_T_2, _GEN_54, io_b_bits_2_1) @[TPU.scala 134:28 91:19]
    node _GEN_128 = mux(_T_2, _GEN_55, io_b_bits_2_2) @[TPU.scala 134:28 91:19]
    node _GEN_129 = mux(_T_2, _GEN_56, UInt<1>("h0")) @[TPU.scala 134:28 88:26]
    node _GEN_130 = mux(_T_2, _GEN_57, b_ready) @[TPU.scala 134:28 62:24]
    node _GEN_132 = mux(_T_2, _GEN_58, _GEN_1) @[TPU.scala 134:28]
    node _GEN_134 = mux(_T_2, myOut_0_0, _GEN_93) @[TPU.scala 134:28 60:22]
    node _GEN_135 = mux(_T_2, myOut_1_0, _GEN_94) @[TPU.scala 134:28 60:22]
    node _GEN_136 = mux(_T_2, myOut_2_0, _GEN_95) @[TPU.scala 134:28 60:22]
    node _GEN_137 = mux(_T_2, myOut_0_1, _GEN_96) @[TPU.scala 134:28 60:22]
    node _GEN_138 = mux(_T_2, myOut_1_1, _GEN_97) @[TPU.scala 134:28 60:22]
    node _GEN_139 = mux(_T_2, myOut_2_1, _GEN_98) @[TPU.scala 134:28 60:22]
    node _GEN_140 = mux(_T_2, myOut_0_2, _GEN_99) @[TPU.scala 134:28 60:22]
    node _GEN_141 = mux(_T_2, myOut_1_2, _GEN_100) @[TPU.scala 134:28 60:22]
    node _GEN_142 = mux(_T_2, myOut_2_2, _GEN_101) @[TPU.scala 134:28 60:22]
    node _GEN_143 = mux(_T_2, b_ready, _GEN_102) @[TPU.scala 134:28 78:14]
    node _GEN_144 = mux(_T, _GEN_3, _GEN_103) @[TPU.scala 125:23]
    node _GEN_145 = mux(_T, _GEN_4, act_in_0_0) @[TPU.scala 125:23 86:23]
    node _GEN_146 = mux(_T, _GEN_5, act_in_0_1) @[TPU.scala 125:23 86:23]
    node _GEN_147 = mux(_T, _GEN_6, act_in_0_2) @[TPU.scala 125:23 86:23]
    node _GEN_148 = mux(_T, _GEN_7, act_in_1_0) @[TPU.scala 125:23 86:23]
    node _GEN_149 = mux(_T, _GEN_8, act_in_1_1) @[TPU.scala 125:23 86:23]
    node _GEN_150 = mux(_T, _GEN_9, act_in_1_2) @[TPU.scala 125:23 86:23]
    node _GEN_151 = mux(_T, _GEN_10, act_in_2_0) @[TPU.scala 125:23 86:23]
    node _GEN_152 = mux(_T, _GEN_11, act_in_2_1) @[TPU.scala 125:23 86:23]
    node _GEN_153 = mux(_T, _GEN_12, act_in_2_2) @[TPU.scala 125:23 86:23]
    node _GEN_154 = mux(_T, _GEN_13, paddedA_0_0) @[TPU.scala 125:23 71:24]
    node _GEN_155 = mux(_T, _GEN_14, paddedA_0_1) @[TPU.scala 125:23 71:24]
    node _GEN_156 = mux(_T, _GEN_15, paddedA_0_2) @[TPU.scala 125:23 71:24]
    node _GEN_157 = mux(_T, _GEN_16, paddedA_0_3) @[TPU.scala 125:23 71:24]
    node _GEN_158 = mux(_T, _GEN_17, paddedA_1_0) @[TPU.scala 125:23 71:24]
    node _GEN_159 = mux(_T, _GEN_18, paddedA_1_1) @[TPU.scala 125:23 71:24]
    node _GEN_160 = mux(_T, _GEN_19, paddedA_1_2) @[TPU.scala 125:23 71:24]
    node _GEN_161 = mux(_T, _GEN_20, paddedA_1_3) @[TPU.scala 125:23 71:24]
    node _GEN_162 = mux(_T, _GEN_21, paddedA_2_0) @[TPU.scala 125:23 71:24]
    node _GEN_163 = mux(_T, _GEN_22, paddedA_2_1) @[TPU.scala 125:23 71:24]
    node _GEN_164 = mux(_T, _GEN_23, paddedA_2_2) @[TPU.scala 125:23 71:24]
    node _GEN_165 = mux(_T, _GEN_24, paddedA_2_3) @[TPU.scala 125:23 71:24]
    node _GEN_166 = mux(_T, _GEN_25, paddedA_3_0) @[TPU.scala 125:23 71:24]
    node _GEN_167 = mux(_T, _GEN_26, paddedA_3_1) @[TPU.scala 125:23 71:24]
    node _GEN_168 = mux(_T, _GEN_27, paddedA_3_2) @[TPU.scala 125:23 71:24]
    node _GEN_169 = mux(_T, _GEN_28, paddedA_3_3) @[TPU.scala 125:23 71:24]
    node _GEN_170 = mux(_T, _GEN_29, a_ready) @[TPU.scala 125:23 61:24]
    node _GEN_171 = mux(_T, UInt<1>("h0"), _GEN_132) @[TPU.scala 125:23 132:13]
    node _GEN_172 = mux(_T, paddedB_0_0, _GEN_104) @[TPU.scala 125:23 72:24]
    node _GEN_173 = mux(_T, paddedB_0_1, _GEN_105) @[TPU.scala 125:23 72:24]
    node _GEN_174 = mux(_T, paddedB_0_2, _GEN_106) @[TPU.scala 125:23 72:24]
    node _GEN_175 = mux(_T, paddedB_0_3, _GEN_107) @[TPU.scala 125:23 72:24]
    node _GEN_176 = mux(_T, paddedB_1_0, _GEN_108) @[TPU.scala 125:23 72:24]
    node _GEN_177 = mux(_T, paddedB_1_1, _GEN_109) @[TPU.scala 125:23 72:24]
    node _GEN_178 = mux(_T, paddedB_1_2, _GEN_110) @[TPU.scala 125:23 72:24]
    node _GEN_179 = mux(_T, paddedB_1_3, _GEN_111) @[TPU.scala 125:23 72:24]
    node _GEN_180 = mux(_T, paddedB_2_0, _GEN_112) @[TPU.scala 125:23 72:24]
    node _GEN_181 = mux(_T, paddedB_2_1, _GEN_113) @[TPU.scala 125:23 72:24]
    node _GEN_182 = mux(_T, paddedB_2_2, _GEN_114) @[TPU.scala 125:23 72:24]
    node _GEN_183 = mux(_T, paddedB_2_3, _GEN_115) @[TPU.scala 125:23 72:24]
    node _GEN_184 = mux(_T, paddedB_3_0, _GEN_116) @[TPU.scala 125:23 72:24]
    node _GEN_185 = mux(_T, paddedB_3_1, _GEN_117) @[TPU.scala 125:23 72:24]
    node _GEN_186 = mux(_T, paddedB_3_2, _GEN_118) @[TPU.scala 125:23 72:24]
    node _GEN_187 = mux(_T, paddedB_3_3, _GEN_119) @[TPU.scala 125:23 72:24]
    node _GEN_188 = mux(_T, io_b_bits_0_0, _GEN_120) @[TPU.scala 125:23 91:19]
    node _GEN_189 = mux(_T, io_b_bits_0_1, _GEN_121) @[TPU.scala 125:23 91:19]
    node _GEN_190 = mux(_T, io_b_bits_0_2, _GEN_122) @[TPU.scala 125:23 91:19]
    node _GEN_191 = mux(_T, io_b_bits_1_0, _GEN_123) @[TPU.scala 125:23 91:19]
    node _GEN_192 = mux(_T, io_b_bits_1_1, _GEN_124) @[TPU.scala 125:23 91:19]
    node _GEN_193 = mux(_T, io_b_bits_1_2, _GEN_125) @[TPU.scala 125:23 91:19]
    node _GEN_194 = mux(_T, io_b_bits_2_0, _GEN_126) @[TPU.scala 125:23 91:19]
    node _GEN_195 = mux(_T, io_b_bits_2_1, _GEN_127) @[TPU.scala 125:23 91:19]
    node _GEN_196 = mux(_T, io_b_bits_2_2, _GEN_128) @[TPU.scala 125:23 91:19]
    node _GEN_197 = mux(_T, UInt<1>("h0"), _GEN_129) @[TPU.scala 125:23 88:26]
    node _GEN_198 = mux(_T, b_ready, _GEN_130) @[TPU.scala 125:23 62:24]
    node _GEN_201 = mux(_T, myOut_0_0, _GEN_134) @[TPU.scala 125:23 60:22]
    node _GEN_202 = mux(_T, myOut_1_0, _GEN_135) @[TPU.scala 125:23 60:22]
    node _GEN_203 = mux(_T, myOut_2_0, _GEN_136) @[TPU.scala 125:23 60:22]
    node _GEN_204 = mux(_T, myOut_0_1, _GEN_137) @[TPU.scala 125:23 60:22]
    node _GEN_205 = mux(_T, myOut_1_1, _GEN_138) @[TPU.scala 125:23 60:22]
    node _GEN_206 = mux(_T, myOut_2_1, _GEN_139) @[TPU.scala 125:23 60:22]
    node _GEN_207 = mux(_T, myOut_0_2, _GEN_140) @[TPU.scala 125:23 60:22]
    node _GEN_208 = mux(_T, myOut_1_2, _GEN_141) @[TPU.scala 125:23 60:22]
    node _GEN_209 = mux(_T, myOut_2_2, _GEN_142) @[TPU.scala 125:23 60:22]
    node _GEN_210 = mux(_T, b_ready, _GEN_143) @[TPU.scala 125:23 78:14]
    node wrap = _GEN_2
    node _myOut_WIRE_0_0 = asSInt(UInt<32>("h0")) @[TPU.scala 60:{45,45}]
    node _myOut_WIRE_0_1 = asSInt(UInt<32>("h0")) @[TPU.scala 60:{45,45}]
    node _myOut_WIRE_0_2 = asSInt(UInt<32>("h0")) @[TPU.scala 60:{45,45}]
    node _myOut_WIRE_1_0 = asSInt(UInt<32>("h0")) @[TPU.scala 60:{45,45}]
    node _myOut_WIRE_1_1 = asSInt(UInt<32>("h0")) @[TPU.scala 60:{45,45}]
    node _myOut_WIRE_1_2 = asSInt(UInt<32>("h0")) @[TPU.scala 60:{45,45}]
    node _myOut_WIRE_2_0 = asSInt(UInt<32>("h0")) @[TPU.scala 60:{45,45}]
    node _myOut_WIRE_2_1 = asSInt(UInt<32>("h0")) @[TPU.scala 60:{45,45}]
    node _myOut_WIRE_2_2 = asSInt(UInt<32>("h0")) @[TPU.scala 60:{45,45}]
    node _paddedA_WIRE_0_0 = asSInt(UInt<32>("h0")) @[TPU.scala 71:{61,61}]
    node _paddedA_WIRE_0_1 = asSInt(UInt<32>("h0")) @[TPU.scala 71:{61,61}]
    node _paddedA_WIRE_0_2 = asSInt(UInt<32>("h0")) @[TPU.scala 71:{61,61}]
    node _paddedA_WIRE_0_3 = asSInt(UInt<32>("h0")) @[TPU.scala 71:{61,61}]
    node _paddedA_WIRE_1_0 = asSInt(UInt<32>("h0")) @[TPU.scala 71:{61,61}]
    node _paddedA_WIRE_1_1 = asSInt(UInt<32>("h0")) @[TPU.scala 71:{61,61}]
    node _paddedA_WIRE_1_2 = asSInt(UInt<32>("h0")) @[TPU.scala 71:{61,61}]
    node _paddedA_WIRE_1_3 = asSInt(UInt<32>("h0")) @[TPU.scala 71:{61,61}]
    node _paddedA_WIRE_2_0 = asSInt(UInt<32>("h0")) @[TPU.scala 71:{61,61}]
    node _paddedA_WIRE_2_1 = asSInt(UInt<32>("h0")) @[TPU.scala 71:{61,61}]
    node _paddedA_WIRE_2_2 = asSInt(UInt<32>("h0")) @[TPU.scala 71:{61,61}]
    node _paddedA_WIRE_2_3 = asSInt(UInt<32>("h0")) @[TPU.scala 71:{61,61}]
    node _paddedA_WIRE_3_0 = asSInt(UInt<32>("h0")) @[TPU.scala 71:{61,61}]
    node _paddedA_WIRE_3_1 = asSInt(UInt<32>("h0")) @[TPU.scala 71:{61,61}]
    node _paddedA_WIRE_3_2 = asSInt(UInt<32>("h0")) @[TPU.scala 71:{61,61}]
    node _paddedA_WIRE_3_3 = asSInt(UInt<32>("h0")) @[TPU.scala 71:{61,61}]
    node _paddedB_WIRE_0_0 = asSInt(UInt<32>("h0")) @[TPU.scala 72:{61,61}]
    node _paddedB_WIRE_0_1 = asSInt(UInt<32>("h0")) @[TPU.scala 72:{61,61}]
    node _paddedB_WIRE_0_2 = asSInt(UInt<32>("h0")) @[TPU.scala 72:{61,61}]
    node _paddedB_WIRE_0_3 = asSInt(UInt<32>("h0")) @[TPU.scala 72:{61,61}]
    node _paddedB_WIRE_1_0 = asSInt(UInt<32>("h0")) @[TPU.scala 72:{61,61}]
    node _paddedB_WIRE_1_1 = asSInt(UInt<32>("h0")) @[TPU.scala 72:{61,61}]
    node _paddedB_WIRE_1_2 = asSInt(UInt<32>("h0")) @[TPU.scala 72:{61,61}]
    node _paddedB_WIRE_1_3 = asSInt(UInt<32>("h0")) @[TPU.scala 72:{61,61}]
    node _paddedB_WIRE_2_0 = asSInt(UInt<32>("h0")) @[TPU.scala 72:{61,61}]
    node _paddedB_WIRE_2_1 = asSInt(UInt<32>("h0")) @[TPU.scala 72:{61,61}]
    node _paddedB_WIRE_2_2 = asSInt(UInt<32>("h0")) @[TPU.scala 72:{61,61}]
    node _paddedB_WIRE_2_3 = asSInt(UInt<32>("h0")) @[TPU.scala 72:{61,61}]
    node _paddedB_WIRE_3_0 = asSInt(UInt<32>("h0")) @[TPU.scala 72:{61,61}]
    node _paddedB_WIRE_3_1 = asSInt(UInt<32>("h0")) @[TPU.scala 72:{61,61}]
    node _paddedB_WIRE_3_2 = asSInt(UInt<32>("h0")) @[TPU.scala 72:{61,61}]
    node _paddedB_WIRE_3_3 = asSInt(UInt<32>("h0")) @[TPU.scala 72:{61,61}]
    node _ASlice_WIRE_0_0 = asSInt(UInt<32>("h0")) @[TPU.scala 73:{64,64}]
    node _ASlice_WIRE_0_1 = asSInt(UInt<32>("h0")) @[TPU.scala 73:{64,64}]
    node _ASlice_WIRE_1_0 = asSInt(UInt<32>("h0")) @[TPU.scala 73:{64,64}]
    node _ASlice_WIRE_1_1 = asSInt(UInt<32>("h0")) @[TPU.scala 73:{64,64}]
    node _BSlice_WIRE_0_0 = asSInt(UInt<32>("h0")) @[TPU.scala 74:{64,64}]
    node _BSlice_WIRE_0_1 = asSInt(UInt<32>("h0")) @[TPU.scala 74:{64,64}]
    node _BSlice_WIRE_1_0 = asSInt(UInt<32>("h0")) @[TPU.scala 74:{64,64}]
    node _BSlice_WIRE_1_1 = asSInt(UInt<32>("h0")) @[TPU.scala 74:{64,64}]
    node _act_in_WIRE_0_0 = asSInt(UInt<32>("h0")) @[TPU.scala 86:{46,46}]
    node _act_in_WIRE_0_1 = asSInt(UInt<32>("h0")) @[TPU.scala 86:{46,46}]
    node _act_in_WIRE_0_2 = asSInt(UInt<32>("h0")) @[TPU.scala 86:{46,46}]
    node _act_in_WIRE_1_0 = asSInt(UInt<32>("h0")) @[TPU.scala 86:{46,46}]
    node _act_in_WIRE_1_1 = asSInt(UInt<32>("h0")) @[TPU.scala 86:{46,46}]
    node _act_in_WIRE_1_2 = asSInt(UInt<32>("h0")) @[TPU.scala 86:{46,46}]
    node _act_in_WIRE_2_0 = asSInt(UInt<32>("h0")) @[TPU.scala 86:{46,46}]
    node _act_in_WIRE_2_1 = asSInt(UInt<32>("h0")) @[TPU.scala 86:{46,46}]
    node _act_in_WIRE_2_2 = asSInt(UInt<32>("h0")) @[TPU.scala 86:{46,46}]
    node cycleIdxCols_0 = UInt<32>("h0") @[TPU.scala 102:21 95:26]
    node cycleIdxCols_1 = UInt<32>("h0") @[TPU.scala 102:21 95:26]
    node cycleIdxCols_2 = UInt<32>("h0") @[TPU.scala 102:21 95:26]
    node cycleIdxRows_0 = UInt<32>("h0") @[TPU.scala 103:21 96:26]
    node cycleIdxRows_1 = UInt<32>("h0") @[TPU.scala 103:21 96:26]
    node cycleIdxRows_2 = UInt<32>("h0") @[TPU.scala 103:21 96:26]
    node systArrOutOffset = UInt<32>("h0") @[TPU.scala 119:20 97:30]
    io_a_ready <= a_ready @[TPU.scala 77:14]
    io_b_ready <= _GEN_210
    io_out_0_0 <= myOut_0_0 @[TPU.scala 79:10]
    io_out_0_1 <= myOut_0_1 @[TPU.scala 79:10]
    io_out_0_2 <= myOut_0_2 @[TPU.scala 79:10]
    io_out_1_0 <= myOut_1_0 @[TPU.scala 79:10]
    io_out_1_1 <= myOut_1_1 @[TPU.scala 79:10]
    io_out_1_2 <= myOut_1_2 @[TPU.scala 79:10]
    io_out_2_0 <= myOut_2_0 @[TPU.scala 79:10]
    io_out_2_1 <= myOut_2_1 @[TPU.scala 79:10]
    io_out_2_2 <= myOut_2_2 @[TPU.scala 79:10]
    io_debug_1_0_0 <= systArr.io_cmp_debug_0_0 @[TPU.scala 107:14]
    io_debug_1_0_1 <= systArr.io_cmp_debug_0_1 @[TPU.scala 107:14]
    io_debug_1_0_2 <= systArr.io_cmp_debug_0_2 @[TPU.scala 107:14]
    io_debug_1_1_0 <= systArr.io_cmp_debug_1_0 @[TPU.scala 107:14]
    io_debug_1_1_1 <= systArr.io_cmp_debug_1_1 @[TPU.scala 107:14]
    io_debug_1_1_2 <= systArr.io_cmp_debug_1_2 @[TPU.scala 107:14]
    io_debug_1_2_0 <= systArr.io_cmp_debug_2_0 @[TPU.scala 107:14]
    io_debug_1_2_1 <= systArr.io_cmp_debug_2_1 @[TPU.scala 107:14]
    io_debug_1_2_2 <= systArr.io_cmp_debug_2_2 @[TPU.scala 107:14]
    io_debug_a_out_0 <= actReg.io_a_out_0 @[TPU.scala 108:18]
    io_debug_a_out_1 <= actReg.io_a_out_1 @[TPU.scala 108:18]
    io_debug_a_out_2 <= actReg.io_a_out_2 @[TPU.scala 108:18]
    io_debug_systreg_out_0 <= systArr.io_out_0 @[TPU.scala 113:24]
    io_debug_systreg_out_1 <= systArr.io_out_1 @[TPU.scala 113:24]
    io_debug_systreg_out_2 <= systArr.io_out_2 @[TPU.scala 113:24]
    io_debug_a_regs_0_0 <= systArr.io_debug_a_regs_0_0 @[TPU.scala 109:19]
    io_debug_a_regs_0_1 <= systArr.io_debug_a_regs_0_1 @[TPU.scala 109:19]
    io_debug_a_regs_0_2 <= systArr.io_debug_a_regs_0_2 @[TPU.scala 109:19]
    io_debug_a_regs_1_0 <= systArr.io_debug_a_regs_1_0 @[TPU.scala 109:19]
    io_debug_a_regs_1_1 <= systArr.io_debug_a_regs_1_1 @[TPU.scala 109:19]
    io_debug_a_regs_1_2 <= systArr.io_debug_a_regs_1_2 @[TPU.scala 109:19]
    io_debug_a_regs_2_0 <= systArr.io_debug_a_regs_2_0 @[TPU.scala 109:19]
    io_debug_a_regs_2_1 <= systArr.io_debug_a_regs_2_1 @[TPU.scala 109:19]
    io_debug_a_regs_2_2 <= systArr.io_debug_a_regs_2_2 @[TPU.scala 109:19]
    io_debug_b_regs_0_0 <= systArr.io_debug_b_regs_0_0 @[TPU.scala 110:19]
    io_debug_b_regs_0_1 <= systArr.io_debug_b_regs_0_1 @[TPU.scala 110:19]
    io_debug_b_regs_0_2 <= systArr.io_debug_b_regs_0_2 @[TPU.scala 110:19]
    io_debug_b_regs_1_0 <= systArr.io_debug_b_regs_1_0 @[TPU.scala 110:19]
    io_debug_b_regs_1_1 <= systArr.io_debug_b_regs_1_1 @[TPU.scala 110:19]
    io_debug_b_regs_1_2 <= systArr.io_debug_b_regs_1_2 @[TPU.scala 110:19]
    io_debug_b_regs_2_0 <= systArr.io_debug_b_regs_2_0 @[TPU.scala 110:19]
    io_debug_b_regs_2_1 <= systArr.io_debug_b_regs_2_1 @[TPU.scala 110:19]
    io_debug_b_regs_2_2 <= systArr.io_debug_b_regs_2_2 @[TPU.scala 110:19]
    io_debug_cycleOut <= pad(cycle, 32) @[TPU.scala 111:21]
    io_debug_00 <= systArr.io_debug_00 @[TPU.scala 112:15]
    io_debug_cycleIdxCols_0 <= cycleIdxCols_0 @[TPU.scala 114:25]
    io_debug_cycleIdxCols_1 <= cycleIdxCols_1 @[TPU.scala 114:25]
    io_debug_cycleIdxCols_2 <= cycleIdxCols_2 @[TPU.scala 114:25]
    io_debug_cycleIdxRows_0 <= cycleIdxRows_0 @[TPU.scala 115:25]
    io_debug_cycleIdxRows_1 <= cycleIdxRows_1 @[TPU.scala 115:25]
    io_debug_cycleIdxRows_2 <= cycleIdxRows_2 @[TPU.scala 115:25]
    io_debug_cycleIdx <= cycleIdx @[TPU.scala 116:21]
    io_debug_systout_upperLim <= SInt<32>("h0") @[TPU.scala 117:29]
    state <= mux(reset, UInt<3>("h0"), _GEN_144) @[TPU.scala 53:{22,22}]
    cycle <= mux(reset, UInt<4>("h0"), _GEN_171) @[Counter.scala 61:{40,40}]
    actReg.clock <= clock
    actReg.reset <= reset
    actReg.io_index <= _actReg_io_index_T_1 @[TPU.scala 83:19]
    actReg.io_a_0_0 <= act_in_0_0 @[TPU.scala 89:15]
    actReg.io_a_0_1 <= act_in_0_1 @[TPU.scala 89:15]
    actReg.io_a_0_2 <= act_in_0_2 @[TPU.scala 89:15]
    actReg.io_a_1_0 <= act_in_1_0 @[TPU.scala 89:15]
    actReg.io_a_1_1 <= act_in_1_1 @[TPU.scala 89:15]
    actReg.io_a_1_2 <= act_in_1_2 @[TPU.scala 89:15]
    actReg.io_a_2_0 <= act_in_2_0 @[TPU.scala 89:15]
    actReg.io_a_2_1 <= act_in_2_1 @[TPU.scala 89:15]
    actReg.io_a_2_2 <= act_in_2_2 @[TPU.scala 89:15]
    systArr.clock <= clock
    systArr.reset <= reset
    systArr.io_a_in_0 <= actReg.io_a_out_0 @[TPU.scala 85:19]
    systArr.io_a_in_1 <= actReg.io_a_out_1 @[TPU.scala 85:19]
    systArr.io_a_in_2 <= actReg.io_a_out_2 @[TPU.scala 85:19]
    systArr.io_b_in_0_0 <= _GEN_188
    systArr.io_b_in_0_1 <= _GEN_189
    systArr.io_b_in_0_2 <= _GEN_190
    systArr.io_b_in_1_0 <= _GEN_191
    systArr.io_b_in_1_1 <= _GEN_192
    systArr.io_b_in_1_2 <= _GEN_193
    systArr.io_b_in_2_0 <= _GEN_194
    systArr.io_b_in_2_1 <= _GEN_195
    systArr.io_b_in_2_2 <= _GEN_196
    systArr.io_b_readingin <= _GEN_197
    myOut_0_0 <= mux(reset, _myOut_WIRE_0_0, _GEN_201) @[TPU.scala 60:{22,22}]
    myOut_0_1 <= mux(reset, _myOut_WIRE_0_1, _GEN_204) @[TPU.scala 60:{22,22}]
    myOut_0_2 <= mux(reset, _myOut_WIRE_0_2, _GEN_207) @[TPU.scala 60:{22,22}]
    myOut_1_0 <= mux(reset, _myOut_WIRE_1_0, _GEN_202) @[TPU.scala 60:{22,22}]
    myOut_1_1 <= mux(reset, _myOut_WIRE_1_1, _GEN_205) @[TPU.scala 60:{22,22}]
    myOut_1_2 <= mux(reset, _myOut_WIRE_1_2, _GEN_208) @[TPU.scala 60:{22,22}]
    myOut_2_0 <= mux(reset, _myOut_WIRE_2_0, _GEN_203) @[TPU.scala 60:{22,22}]
    myOut_2_1 <= mux(reset, _myOut_WIRE_2_1, _GEN_206) @[TPU.scala 60:{22,22}]
    myOut_2_2 <= mux(reset, _myOut_WIRE_2_2, _GEN_209) @[TPU.scala 60:{22,22}]
    a_ready <= mux(reset, UInt<1>("h1"), _GEN_170) @[TPU.scala 61:{24,24}]
    b_ready <= mux(reset, UInt<1>("h1"), _GEN_198) @[TPU.scala 62:{24,24}]
    paddedA_0_0 <= mux(reset, _paddedA_WIRE_0_0, _GEN_154) @[TPU.scala 71:{24,24}]
    paddedA_0_1 <= mux(reset, _paddedA_WIRE_0_1, _GEN_155) @[TPU.scala 71:{24,24}]
    paddedA_0_2 <= mux(reset, _paddedA_WIRE_0_2, _GEN_156) @[TPU.scala 71:{24,24}]
    paddedA_0_3 <= mux(reset, _paddedA_WIRE_0_3, _GEN_157) @[TPU.scala 71:{24,24}]
    paddedA_1_0 <= mux(reset, _paddedA_WIRE_1_0, _GEN_158) @[TPU.scala 71:{24,24}]
    paddedA_1_1 <= mux(reset, _paddedA_WIRE_1_1, _GEN_159) @[TPU.scala 71:{24,24}]
    paddedA_1_2 <= mux(reset, _paddedA_WIRE_1_2, _GEN_160) @[TPU.scala 71:{24,24}]
    paddedA_1_3 <= mux(reset, _paddedA_WIRE_1_3, _GEN_161) @[TPU.scala 71:{24,24}]
    paddedA_2_0 <= mux(reset, _paddedA_WIRE_2_0, _GEN_162) @[TPU.scala 71:{24,24}]
    paddedA_2_1 <= mux(reset, _paddedA_WIRE_2_1, _GEN_163) @[TPU.scala 71:{24,24}]
    paddedA_2_2 <= mux(reset, _paddedA_WIRE_2_2, _GEN_164) @[TPU.scala 71:{24,24}]
    paddedA_2_3 <= mux(reset, _paddedA_WIRE_2_3, _GEN_165) @[TPU.scala 71:{24,24}]
    paddedA_3_0 <= mux(reset, _paddedA_WIRE_3_0, _GEN_166) @[TPU.scala 71:{24,24}]
    paddedA_3_1 <= mux(reset, _paddedA_WIRE_3_1, _GEN_167) @[TPU.scala 71:{24,24}]
    paddedA_3_2 <= mux(reset, _paddedA_WIRE_3_2, _GEN_168) @[TPU.scala 71:{24,24}]
    paddedA_3_3 <= mux(reset, _paddedA_WIRE_3_3, _GEN_169) @[TPU.scala 71:{24,24}]
    paddedB_0_0 <= mux(reset, _paddedB_WIRE_0_0, _GEN_172) @[TPU.scala 72:{24,24}]
    paddedB_0_1 <= mux(reset, _paddedB_WIRE_0_1, _GEN_173) @[TPU.scala 72:{24,24}]
    paddedB_0_2 <= mux(reset, _paddedB_WIRE_0_2, _GEN_174) @[TPU.scala 72:{24,24}]
    paddedB_0_3 <= mux(reset, _paddedB_WIRE_0_3, _GEN_175) @[TPU.scala 72:{24,24}]
    paddedB_1_0 <= mux(reset, _paddedB_WIRE_1_0, _GEN_176) @[TPU.scala 72:{24,24}]
    paddedB_1_1 <= mux(reset, _paddedB_WIRE_1_1, _GEN_177) @[TPU.scala 72:{24,24}]
    paddedB_1_2 <= mux(reset, _paddedB_WIRE_1_2, _GEN_178) @[TPU.scala 72:{24,24}]
    paddedB_1_3 <= mux(reset, _paddedB_WIRE_1_3, _GEN_179) @[TPU.scala 72:{24,24}]
    paddedB_2_0 <= mux(reset, _paddedB_WIRE_2_0, _GEN_180) @[TPU.scala 72:{24,24}]
    paddedB_2_1 <= mux(reset, _paddedB_WIRE_2_1, _GEN_181) @[TPU.scala 72:{24,24}]
    paddedB_2_2 <= mux(reset, _paddedB_WIRE_2_2, _GEN_182) @[TPU.scala 72:{24,24}]
    paddedB_2_3 <= mux(reset, _paddedB_WIRE_2_3, _GEN_183) @[TPU.scala 72:{24,24}]
    paddedB_3_0 <= mux(reset, _paddedB_WIRE_3_0, _GEN_184) @[TPU.scala 72:{24,24}]
    paddedB_3_1 <= mux(reset, _paddedB_WIRE_3_1, _GEN_185) @[TPU.scala 72:{24,24}]
    paddedB_3_2 <= mux(reset, _paddedB_WIRE_3_2, _GEN_186) @[TPU.scala 72:{24,24}]
    paddedB_3_3 <= mux(reset, _paddedB_WIRE_3_3, _GEN_187) @[TPU.scala 72:{24,24}]
    ASlice_0_0 <= mux(reset, _ASlice_WIRE_0_0, ASlice_0_0) @[TPU.scala 73:{23,23,23}]
    ASlice_0_1 <= mux(reset, _ASlice_WIRE_0_1, ASlice_0_1) @[TPU.scala 73:{23,23,23}]
    ASlice_1_0 <= mux(reset, _ASlice_WIRE_1_0, ASlice_1_0) @[TPU.scala 73:{23,23,23}]
    ASlice_1_1 <= mux(reset, _ASlice_WIRE_1_1, ASlice_1_1) @[TPU.scala 73:{23,23,23}]
    BSlice_0_0 <= mux(reset, _BSlice_WIRE_0_0, BSlice_0_0) @[TPU.scala 74:{23,23,23}]
    BSlice_0_1 <= mux(reset, _BSlice_WIRE_0_1, BSlice_0_1) @[TPU.scala 74:{23,23,23}]
    BSlice_1_0 <= mux(reset, _BSlice_WIRE_1_0, BSlice_1_0) @[TPU.scala 74:{23,23,23}]
    BSlice_1_1 <= mux(reset, _BSlice_WIRE_1_1, BSlice_1_1) @[TPU.scala 74:{23,23,23}]
    act_in_0_0 <= mux(reset, _act_in_WIRE_0_0, _GEN_145) @[TPU.scala 86:{23,23}]
    act_in_0_1 <= mux(reset, _act_in_WIRE_0_1, _GEN_146) @[TPU.scala 86:{23,23}]
    act_in_0_2 <= mux(reset, _act_in_WIRE_0_2, _GEN_147) @[TPU.scala 86:{23,23}]
    act_in_1_0 <= mux(reset, _act_in_WIRE_1_0, _GEN_148) @[TPU.scala 86:{23,23}]
    act_in_1_1 <= mux(reset, _act_in_WIRE_1_1, _GEN_149) @[TPU.scala 86:{23,23}]
    act_in_1_2 <= mux(reset, _act_in_WIRE_1_2, _GEN_150) @[TPU.scala 86:{23,23}]
    act_in_2_0 <= mux(reset, _act_in_WIRE_2_0, _GEN_151) @[TPU.scala 86:{23,23}]
    act_in_2_1 <= mux(reset, _act_in_WIRE_2_1, _GEN_152) @[TPU.scala 86:{23,23}]
    act_in_2_2 <= mux(reset, _act_in_WIRE_2_2, _GEN_153) @[TPU.scala 86:{23,23}]
    printf(clock, and(and(and(and(and(UInt<1>("h1"), eq(_T, UInt<1>("h0"))), eq(_T_2, UInt<1>("h0"))), _T_4), _T_18), UInt<1>("h1")), "\n-------------------------\n") : printf @[TPU.scala 231:11]
    printf(clock, and(and(and(and(and(UInt<1>("h1"), eq(_T, UInt<1>("h0"))), eq(_T_2, UInt<1>("h0"))), _T_4), _T_20), UInt<1>("h1")), "MY TPU OUT: \n") : printf_1 @[TPU.scala 232:11]
    printf(clock, and(and(and(and(and(UInt<1>("h1"), eq(_T, UInt<1>("h0"))), eq(_T_2, UInt<1>("h0"))), _T_4), _T_22), UInt<1>("h1")), "Vec(%d, %d, %d)\n", myOut_0_0, myOut_0_1, myOut_0_2) : printf_2 @[TPU.scala 234:13]
    printf(clock, and(and(and(and(and(UInt<1>("h1"), eq(_T, UInt<1>("h0"))), eq(_T_2, UInt<1>("h0"))), _T_4), _T_24), UInt<1>("h1")), "Vec(%d, %d, %d)\n", myOut_1_0, myOut_1_1, myOut_1_2) : printf_3 @[TPU.scala 234:13]
    printf(clock, and(and(and(and(and(UInt<1>("h1"), eq(_T, UInt<1>("h0"))), eq(_T_2, UInt<1>("h0"))), _T_4), _T_26), UInt<1>("h1")), "Vec(%d, %d, %d)\n", myOut_2_0, myOut_2_1, myOut_2_2) : printf_4 @[TPU.scala 234:13]
    printf(clock, and(and(and(and(and(UInt<1>("h1"), eq(_T, UInt<1>("h0"))), eq(_T_2, UInt<1>("h0"))), _T_4), _T_28), UInt<1>("h1")), "NORMAL A: \n") : printf_5 @[TPU.scala 236:11]
    printf(clock, and(and(and(and(and(UInt<1>("h1"), eq(_T, UInt<1>("h0"))), eq(_T_2, UInt<1>("h0"))), _T_4), _T_30), UInt<1>("h1")), "Vec(%d, %d, %d)\n", io_a_bits_0_0, io_a_bits_0_1, io_a_bits_0_2) : printf_6 @[TPU.scala 238:13]
    printf(clock, and(and(and(and(and(UInt<1>("h1"), eq(_T, UInt<1>("h0"))), eq(_T_2, UInt<1>("h0"))), _T_4), _T_32), UInt<1>("h1")), "Vec(%d, %d, %d)\n", io_a_bits_1_0, io_a_bits_1_1, io_a_bits_1_2) : printf_7 @[TPU.scala 238:13]
    printf(clock, and(and(and(and(and(UInt<1>("h1"), eq(_T, UInt<1>("h0"))), eq(_T_2, UInt<1>("h0"))), _T_4), _T_34), UInt<1>("h1")), "Vec(%d, %d, %d)\n", io_a_bits_2_0, io_a_bits_2_1, io_a_bits_2_2) : printf_8 @[TPU.scala 238:13]
    printf(clock, and(and(and(and(and(UInt<1>("h1"), eq(_T, UInt<1>("h0"))), eq(_T_2, UInt<1>("h0"))), _T_4), _T_36), UInt<1>("h1")), "NORMAL B: \n") : printf_9 @[TPU.scala 240:11]
    printf(clock, and(and(and(and(and(UInt<1>("h1"), eq(_T, UInt<1>("h0"))), eq(_T_2, UInt<1>("h0"))), _T_4), _T_38), UInt<1>("h1")), "Vec(%d, %d, %d)\n", io_b_bits_0_0, io_b_bits_0_1, io_b_bits_0_2) : printf_10 @[TPU.scala 242:13]
    printf(clock, and(and(and(and(and(UInt<1>("h1"), eq(_T, UInt<1>("h0"))), eq(_T_2, UInt<1>("h0"))), _T_4), _T_40), UInt<1>("h1")), "Vec(%d, %d, %d)\n", io_b_bits_1_0, io_b_bits_1_1, io_b_bits_1_2) : printf_11 @[TPU.scala 242:13]
    printf(clock, and(and(and(and(and(UInt<1>("h1"), eq(_T, UInt<1>("h0"))), eq(_T_2, UInt<1>("h0"))), _T_4), _T_42), UInt<1>("h1")), "Vec(%d, %d, %d)\n", io_b_bits_2_0, io_b_bits_2_1, io_b_bits_2_2) : printf_12 @[TPU.scala 242:13]
    printf(clock, and(and(and(and(and(UInt<1>("h1"), eq(_T, UInt<1>("h0"))), eq(_T_2, UInt<1>("h0"))), _T_4), _T_44), UInt<1>("h1")), "PADDED A: \n") : printf_13 @[TPU.scala 244:11]
    printf(clock, and(and(and(and(and(UInt<1>("h1"), eq(_T, UInt<1>("h0"))), eq(_T_2, UInt<1>("h0"))), _T_4), _T_46), UInt<1>("h1")), "Vec(%d, %d, %d, %d)\n", paddedA_0_0, paddedA_0_1, paddedA_0_2, paddedA_0_3) : printf_14 @[TPU.scala 246:13]
    printf(clock, and(and(and(and(and(UInt<1>("h1"), eq(_T, UInt<1>("h0"))), eq(_T_2, UInt<1>("h0"))), _T_4), _T_48), UInt<1>("h1")), "Vec(%d, %d, %d, %d)\n", paddedA_1_0, paddedA_1_1, paddedA_1_2, paddedA_1_3) : printf_15 @[TPU.scala 246:13]
    printf(clock, and(and(and(and(and(UInt<1>("h1"), eq(_T, UInt<1>("h0"))), eq(_T_2, UInt<1>("h0"))), _T_4), _T_50), UInt<1>("h1")), "Vec(%d, %d, %d, %d)\n", paddedA_2_0, paddedA_2_1, paddedA_2_2, paddedA_2_3) : printf_16 @[TPU.scala 246:13]
    printf(clock, and(and(and(and(and(UInt<1>("h1"), eq(_T, UInt<1>("h0"))), eq(_T_2, UInt<1>("h0"))), _T_4), _T_52), UInt<1>("h1")), "Vec(%d, %d, %d, %d)\n", paddedA_3_0, paddedA_3_1, paddedA_3_2, paddedA_3_3) : printf_17 @[TPU.scala 246:13]
    printf(clock, and(and(and(and(and(UInt<1>("h1"), eq(_T, UInt<1>("h0"))), eq(_T_2, UInt<1>("h0"))), _T_4), _T_54), UInt<1>("h1")), "PADDED B: \n") : printf_18 @[TPU.scala 248:12]
    printf(clock, and(and(and(and(and(UInt<1>("h1"), eq(_T, UInt<1>("h0"))), eq(_T_2, UInt<1>("h0"))), _T_4), _T_56), UInt<1>("h1")), "Vec(%d, %d, %d, %d)\n", paddedB_0_0, paddedB_0_1, paddedB_0_2, paddedB_0_3) : printf_19 @[TPU.scala 250:13]
    printf(clock, and(and(and(and(and(UInt<1>("h1"), eq(_T, UInt<1>("h0"))), eq(_T_2, UInt<1>("h0"))), _T_4), _T_58), UInt<1>("h1")), "Vec(%d, %d, %d, %d)\n", paddedB_1_0, paddedB_1_1, paddedB_1_2, paddedB_1_3) : printf_20 @[TPU.scala 250:13]
    printf(clock, and(and(and(and(and(UInt<1>("h1"), eq(_T, UInt<1>("h0"))), eq(_T_2, UInt<1>("h0"))), _T_4), _T_60), UInt<1>("h1")), "Vec(%d, %d, %d, %d)\n", paddedB_2_0, paddedB_2_1, paddedB_2_2, paddedB_2_3) : printf_21 @[TPU.scala 250:13]
    printf(clock, and(and(and(and(and(UInt<1>("h1"), eq(_T, UInt<1>("h0"))), eq(_T_2, UInt<1>("h0"))), _T_4), _T_62), UInt<1>("h1")), "Vec(%d, %d, %d, %d)\n", paddedB_3_0, paddedB_3_1, paddedB_3_2, paddedB_3_3) : printf_22 @[TPU.scala 250:13]
    printf(clock, and(and(and(and(and(UInt<1>("h1"), eq(_T, UInt<1>("h0"))), eq(_T_2, UInt<1>("h0"))), _T_4), _T_64), UInt<1>("h1")), "actreg out:\n") : printf_23 @[TPU.scala 252:11]
    printf(clock, and(and(and(and(and(UInt<1>("h1"), eq(_T, UInt<1>("h0"))), eq(_T_2, UInt<1>("h0"))), _T_4), _T_66), UInt<1>("h1")), "%d  ", actReg.io_a_out_0) : printf_24 @[TPU.scala 254:13]
    printf(clock, and(and(and(and(and(UInt<1>("h1"), eq(_T, UInt<1>("h0"))), eq(_T_2, UInt<1>("h0"))), _T_4), _T_68), UInt<1>("h1")), "%d  ", actReg.io_a_out_1) : printf_25 @[TPU.scala 254:13]
    printf(clock, and(and(and(and(and(UInt<1>("h1"), eq(_T, UInt<1>("h0"))), eq(_T_2, UInt<1>("h0"))), _T_4), _T_70), UInt<1>("h1")), "%d  ", actReg.io_a_out_2) : printf_26 @[TPU.scala 254:13]
    printf(clock, and(and(and(and(and(and(UInt<1>("h1"), eq(_T, UInt<1>("h0"))), eq(_T_2, UInt<1>("h0"))), eq(_T_4, UInt<1>("h0"))), _T_71), _T_73), UInt<1>("h1")), "FINISH\n") : printf_27 @[TPU.scala 267:11]
