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

    node _T = lt(io_index, UInt<3>("h5")) @[TPU.scala 234:19]
    node _T_1 = asSInt(io_index) @[TPU.scala 236:35]
    node _T_2 = sub(asSInt(UInt<3>("h2")), _T_1) @[TPU.scala 236:25]
    node _T_3 = tail(_T_2, 1) @[TPU.scala 236:25]
    node _T_4 = asSInt(_T_3) @[TPU.scala 236:25]
    node _T_5 = leq(_T_4, asSInt(UInt<1>("h0"))) @[TPU.scala 236:42]
    node _T_6 = asSInt(io_index) @[TPU.scala 236:77]
    node _T_7 = sub(asSInt(UInt<4>("h5")), _T_6) @[TPU.scala 236:67]
    node _T_8 = tail(_T_7, 1) @[TPU.scala 236:67]
    node _T_9 = asSInt(_T_8) @[TPU.scala 236:67]
    node _T_10 = gt(_T_9, asSInt(UInt<1>("h0"))) @[TPU.scala 236:84]
    node _T_11 = and(_T_5, _T_10) @[TPU.scala 236:49]
    node _io_a_out_0_T = sub(UInt<3>("h4"), io_index) @[TPU.scala 237:55]
    node _io_a_out_0_T_1 = tail(_io_a_out_0_T, 1) @[TPU.scala 237:55]
    node _io_a_out_0_T_2 = bits(_io_a_out_0_T_1, 1, 0)
    node _GEN_0 = validif(eq(UInt<1>("h0"), _io_a_out_0_T_2), io_a_0_0) @[TPU.scala 237:{25,25}]
    node _GEN_1 = mux(eq(UInt<1>("h1"), _io_a_out_0_T_2), io_a_1_0, _GEN_0) @[TPU.scala 237:{25,25}]
    node _GEN_2 = mux(eq(UInt<2>("h2"), _io_a_out_0_T_2), io_a_2_0, _GEN_1) @[TPU.scala 237:{25,25}]
    node _io_a_io_a_out_0_T_2_0 = _GEN_2 @[TPU.scala 237:25]
    node _GEN_3 = mux(_T_11, _io_a_io_a_out_0_T_2_0, asSInt(UInt<1>("h0"))) @[TPU.scala 236:90 237:25 240:25]
    node _T_12 = asSInt(io_index) @[TPU.scala 236:35]
    node _T_13 = sub(asSInt(UInt<2>("h1")), _T_12) @[TPU.scala 236:25]
    node _T_14 = tail(_T_13, 1) @[TPU.scala 236:25]
    node _T_15 = asSInt(_T_14) @[TPU.scala 236:25]
    node _T_16 = leq(_T_15, asSInt(UInt<1>("h0"))) @[TPU.scala 236:42]
    node _T_17 = asSInt(io_index) @[TPU.scala 236:77]
    node _T_18 = sub(asSInt(UInt<4>("h4")), _T_17) @[TPU.scala 236:67]
    node _T_19 = tail(_T_18, 1) @[TPU.scala 236:67]
    node _T_20 = asSInt(_T_19) @[TPU.scala 236:67]
    node _T_21 = gt(_T_20, asSInt(UInt<1>("h0"))) @[TPU.scala 236:84]
    node _T_22 = and(_T_16, _T_21) @[TPU.scala 236:49]
    node _io_a_out_1_T = sub(UInt<2>("h3"), io_index) @[TPU.scala 237:55]
    node _io_a_out_1_T_1 = tail(_io_a_out_1_T, 1) @[TPU.scala 237:55]
    node _io_a_out_1_T_2 = bits(_io_a_out_1_T_1, 1, 0)
    node _GEN_4 = validif(eq(UInt<1>("h0"), _io_a_out_1_T_2), io_a_0_1) @[TPU.scala 237:{25,25}]
    node _GEN_5 = mux(eq(UInt<1>("h1"), _io_a_out_1_T_2), io_a_1_1, _GEN_4) @[TPU.scala 237:{25,25}]
    node _GEN_6 = mux(eq(UInt<2>("h2"), _io_a_out_1_T_2), io_a_2_1, _GEN_5) @[TPU.scala 237:{25,25}]
    node _io_a_io_a_out_1_T_2_1 = _GEN_6 @[TPU.scala 237:25]
    node _GEN_7 = mux(_T_22, _io_a_io_a_out_1_T_2_1, asSInt(UInt<1>("h0"))) @[TPU.scala 236:90 237:25 240:25]
    node _T_23 = asSInt(io_index) @[TPU.scala 236:35]
    node _T_24 = sub(asSInt(UInt<1>("h0")), _T_23) @[TPU.scala 236:25]
    node _T_25 = tail(_T_24, 1) @[TPU.scala 236:25]
    node _T_26 = asSInt(_T_25) @[TPU.scala 236:25]
    node _T_27 = leq(_T_26, asSInt(UInt<1>("h0"))) @[TPU.scala 236:42]
    node _T_28 = asSInt(io_index) @[TPU.scala 236:77]
    node _T_29 = sub(asSInt(UInt<3>("h3")), _T_28) @[TPU.scala 236:67]
    node _T_30 = tail(_T_29, 1) @[TPU.scala 236:67]
    node _T_31 = asSInt(_T_30) @[TPU.scala 236:67]
    node _T_32 = gt(_T_31, asSInt(UInt<1>("h0"))) @[TPU.scala 236:84]
    node _T_33 = and(_T_27, _T_32) @[TPU.scala 236:49]
    node _io_a_out_2_T = sub(UInt<2>("h2"), io_index) @[TPU.scala 237:55]
    node _io_a_out_2_T_1 = tail(_io_a_out_2_T, 1) @[TPU.scala 237:55]
    node _io_a_out_2_T_2 = bits(_io_a_out_2_T_1, 1, 0)
    node _GEN_8 = validif(eq(UInt<1>("h0"), _io_a_out_2_T_2), io_a_0_2) @[TPU.scala 237:{25,25}]
    node _GEN_9 = mux(eq(UInt<1>("h1"), _io_a_out_2_T_2), io_a_1_2, _GEN_8) @[TPU.scala 237:{25,25}]
    node _GEN_10 = mux(eq(UInt<2>("h2"), _io_a_out_2_T_2), io_a_2_2, _GEN_9) @[TPU.scala 237:{25,25}]
    node _io_a_io_a_out_2_T_2_2 = _GEN_10 @[TPU.scala 237:25]
    node _GEN_11 = mux(_T_33, _io_a_io_a_out_2_T_2_2, asSInt(UInt<1>("h0"))) @[TPU.scala 236:90 237:25 240:25]
    node _GEN_12 = mux(_T, _GEN_3, asSInt(UInt<1>("h0"))) @[TPU.scala 234:35 246:23]
    node _GEN_13 = mux(_T, _GEN_7, asSInt(UInt<1>("h0"))) @[TPU.scala 234:35 246:23]
    node _GEN_14 = mux(_T, _GEN_11, asSInt(UInt<1>("h0"))) @[TPU.scala 234:35 246:23]
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
      reset => (UInt<1>("h0"), a_reg_0_0) @[TPU.scala 263:20]
    reg a_reg_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_0_1) @[TPU.scala 263:20]
    reg a_reg_0_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_0_2) @[TPU.scala 263:20]
    reg a_reg_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_1_0) @[TPU.scala 263:20]
    reg a_reg_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_1_1) @[TPU.scala 263:20]
    reg a_reg_1_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_1_2) @[TPU.scala 263:20]
    reg a_reg_2_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_2_0) @[TPU.scala 263:20]
    reg a_reg_2_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_2_1) @[TPU.scala 263:20]
    reg a_reg_2_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_2_2) @[TPU.scala 263:20]
    reg b_reg_0_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_0_0) @[TPU.scala 264:20]
    reg b_reg_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_0_1) @[TPU.scala 264:20]
    reg b_reg_0_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_0_2) @[TPU.scala 264:20]
    reg b_reg_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_1_0) @[TPU.scala 264:20]
    reg b_reg_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_1_1) @[TPU.scala 264:20]
    reg b_reg_1_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_1_2) @[TPU.scala 264:20]
    reg b_reg_2_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_2_0) @[TPU.scala 264:20]
    reg b_reg_2_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_2_1) @[TPU.scala 264:20]
    reg b_reg_2_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_2_2) @[TPU.scala 264:20]
    reg cms_reg_0_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_0_0) @[TPU.scala 265:22]
    reg cms_reg_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_0_1) @[TPU.scala 265:22]
    reg cms_reg_0_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_0_2) @[TPU.scala 265:22]
    reg cms_reg_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_1_0) @[TPU.scala 265:22]
    reg cms_reg_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_1_1) @[TPU.scala 265:22]
    reg cms_reg_1_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_1_2) @[TPU.scala 265:22]
    reg cms_reg_2_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_2_0) @[TPU.scala 265:22]
    reg cms_reg_2_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_2_1) @[TPU.scala 265:22]
    reg cms_reg_2_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_2_2) @[TPU.scala 265:22]
    node _io_debug_00_T = mul(io_a_in_0, b_reg_0_0) @[TPU.scala 269:31]
    node _GEN_0 = mux(io_b_readingin, io_b_in_0_0, b_reg_0_0) @[TPU.scala 270:25 271:13 273:13]
    node _GEN_1 = mux(io_b_readingin, io_b_in_0_1, b_reg_0_1) @[TPU.scala 270:25 271:13 273:13]
    node _GEN_2 = mux(io_b_readingin, io_b_in_0_2, b_reg_0_2) @[TPU.scala 270:25 271:13 273:13]
    node _GEN_3 = mux(io_b_readingin, io_b_in_1_0, b_reg_1_0) @[TPU.scala 270:25 271:13 273:13]
    node _GEN_4 = mux(io_b_readingin, io_b_in_1_1, b_reg_1_1) @[TPU.scala 270:25 271:13 273:13]
    node _GEN_5 = mux(io_b_readingin, io_b_in_1_2, b_reg_1_2) @[TPU.scala 270:25 271:13 273:13]
    node _GEN_6 = mux(io_b_readingin, io_b_in_2_0, b_reg_2_0) @[TPU.scala 270:25 271:13 273:13]
    node _GEN_7 = mux(io_b_readingin, io_b_in_2_1, b_reg_2_1) @[TPU.scala 270:25 271:13 273:13]
    node _GEN_8 = mux(io_b_readingin, io_b_in_2_2, b_reg_2_2) @[TPU.scala 270:25 271:13 273:13]
    node _cmp_input_0_0_T = mul(io_a_in_0, b_reg_0_0) @[TPU.scala 283:60]
    node _cmp_input_0_1_T = mul(a_reg_0_0, b_reg_0_1) @[TPU.scala 286:66]
    node _cmp_input_0_2_T = mul(a_reg_0_1, b_reg_0_2) @[TPU.scala 286:66]
    node _cmp_input_1_0_T = mul(io_a_in_1, b_reg_1_0) @[TPU.scala 291:61]
    node _cmp_input_1_0_T_1 = add(_cmp_input_1_0_T, cms_reg_0_0) @[TPU.scala 291:82]
    node _cmp_input_1_0_T_2 = tail(_cmp_input_1_0_T_1, 1) @[TPU.scala 291:82]
    node _cmp_input_1_0_T_3 = asSInt(_cmp_input_1_0_T_2) @[TPU.scala 291:82]
    node _cmp_input_1_1_T = mul(a_reg_1_0, b_reg_1_1) @[TPU.scala 294:67]
    node _cmp_input_1_1_T_1 = add(_cmp_input_1_1_T, cms_reg_0_1) @[TPU.scala 294:88]
    node _cmp_input_1_1_T_2 = tail(_cmp_input_1_1_T_1, 1) @[TPU.scala 294:88]
    node _cmp_input_1_1_T_3 = asSInt(_cmp_input_1_1_T_2) @[TPU.scala 294:88]
    node _cmp_input_1_2_T = mul(a_reg_1_1, b_reg_1_2) @[TPU.scala 294:67]
    node _cmp_input_1_2_T_1 = add(_cmp_input_1_2_T, cms_reg_0_2) @[TPU.scala 294:88]
    node _cmp_input_1_2_T_2 = tail(_cmp_input_1_2_T_1, 1) @[TPU.scala 294:88]
    node _cmp_input_1_2_T_3 = asSInt(_cmp_input_1_2_T_2) @[TPU.scala 294:88]
    node _cmp_input_2_0_T = mul(io_a_in_2, b_reg_2_0) @[TPU.scala 291:61]
    node _cmp_input_2_0_T_1 = add(_cmp_input_2_0_T, cms_reg_1_0) @[TPU.scala 291:82]
    node _cmp_input_2_0_T_2 = tail(_cmp_input_2_0_T_1, 1) @[TPU.scala 291:82]
    node _cmp_input_2_0_T_3 = asSInt(_cmp_input_2_0_T_2) @[TPU.scala 291:82]
    node _cmp_input_2_1_T = mul(a_reg_2_0, b_reg_2_1) @[TPU.scala 294:67]
    node _cmp_input_2_1_T_1 = add(_cmp_input_2_1_T, cms_reg_1_1) @[TPU.scala 294:88]
    node _cmp_input_2_1_T_2 = tail(_cmp_input_2_1_T_1, 1) @[TPU.scala 294:88]
    node _cmp_input_2_1_T_3 = asSInt(_cmp_input_2_1_T_2) @[TPU.scala 294:88]
    node _cmp_input_2_2_T = mul(a_reg_2_1, b_reg_2_2) @[TPU.scala 294:67]
    node _cmp_input_2_2_T_1 = add(_cmp_input_2_2_T, cms_reg_1_2) @[TPU.scala 294:88]
    node _cmp_input_2_2_T_2 = tail(_cmp_input_2_2_T_1, 1) @[TPU.scala 294:88]
    node _cmp_input_2_2_T_3 = asSInt(_cmp_input_2_2_T_2) @[TPU.scala 294:88]
    node cmp_input_0_0 = asSInt(bits(_cmp_input_0_0_T, 31, 0)) @[TPU.scala 278:25 283:43]
    node cmp_input_0_1 = asSInt(bits(_cmp_input_0_1_T, 31, 0)) @[TPU.scala 278:25 286:43]
    node cmp_input_0_2 = asSInt(bits(_cmp_input_0_2_T, 31, 0)) @[TPU.scala 278:25 286:43]
    node cmp_input_1_0 = asSInt(bits(_cmp_input_1_0_T_3, 31, 0)) @[TPU.scala 278:25 291:43]
    node cmp_input_1_1 = asSInt(bits(_cmp_input_1_1_T_3, 31, 0)) @[TPU.scala 278:25 294:43]
    node cmp_input_1_2 = asSInt(bits(_cmp_input_1_2_T_3, 31, 0)) @[TPU.scala 278:25 294:43]
    node cmp_input_2_0 = asSInt(bits(_cmp_input_2_0_T_3, 31, 0)) @[TPU.scala 278:25 291:43]
    node cmp_input_2_1 = asSInt(bits(_cmp_input_2_1_T_3, 31, 0)) @[TPU.scala 278:25 294:43]
    node cmp_input_2_2 = asSInt(bits(_cmp_input_2_2_T_3, 31, 0)) @[TPU.scala 278:25 294:43]
    io_out_0 <= cms_reg_2_0 @[TPU.scala 276:19]
    io_out_1 <= cms_reg_2_1 @[TPU.scala 276:19]
    io_out_2 <= cms_reg_2_2 @[TPU.scala 276:19]
    io_cmp_debug_0_0 <= cms_reg_0_0 @[TPU.scala 266:18]
    io_cmp_debug_0_1 <= cms_reg_0_1 @[TPU.scala 266:18]
    io_cmp_debug_0_2 <= cms_reg_0_2 @[TPU.scala 266:18]
    io_cmp_debug_1_0 <= cms_reg_1_0 @[TPU.scala 266:18]
    io_cmp_debug_1_1 <= cms_reg_1_1 @[TPU.scala 266:18]
    io_cmp_debug_1_2 <= cms_reg_1_2 @[TPU.scala 266:18]
    io_cmp_debug_2_0 <= cms_reg_2_0 @[TPU.scala 266:18]
    io_cmp_debug_2_1 <= cms_reg_2_1 @[TPU.scala 266:18]
    io_cmp_debug_2_2 <= cms_reg_2_2 @[TPU.scala 266:18]
    io_debug_b_regs_0_0 <= b_reg_0_0 @[TPU.scala 267:21]
    io_debug_b_regs_0_1 <= b_reg_0_1 @[TPU.scala 267:21]
    io_debug_b_regs_0_2 <= b_reg_0_2 @[TPU.scala 267:21]
    io_debug_b_regs_1_0 <= b_reg_1_0 @[TPU.scala 267:21]
    io_debug_b_regs_1_1 <= b_reg_1_1 @[TPU.scala 267:21]
    io_debug_b_regs_1_2 <= b_reg_1_2 @[TPU.scala 267:21]
    io_debug_b_regs_2_0 <= b_reg_2_0 @[TPU.scala 267:21]
    io_debug_b_regs_2_1 <= b_reg_2_1 @[TPU.scala 267:21]
    io_debug_b_regs_2_2 <= b_reg_2_2 @[TPU.scala 267:21]
    io_debug_a_regs_0_0 <= a_reg_0_0 @[TPU.scala 268:21]
    io_debug_a_regs_0_1 <= a_reg_0_1 @[TPU.scala 268:21]
    io_debug_a_regs_0_2 <= a_reg_0_2 @[TPU.scala 268:21]
    io_debug_a_regs_1_0 <= a_reg_1_0 @[TPU.scala 268:21]
    io_debug_a_regs_1_1 <= a_reg_1_1 @[TPU.scala 268:21]
    io_debug_a_regs_1_2 <= a_reg_1_2 @[TPU.scala 268:21]
    io_debug_a_regs_2_0 <= a_reg_2_0 @[TPU.scala 268:21]
    io_debug_a_regs_2_1 <= a_reg_2_1 @[TPU.scala 268:21]
    io_debug_a_regs_2_2 <= a_reg_2_2 @[TPU.scala 268:21]
    io_debug_00 <= asSInt(bits(_io_debug_00_T, 31, 0)) @[TPU.scala 269:17]
    a_reg_0_0 <= io_a_in_0 @[TPU.scala 284:39]
    a_reg_0_1 <= a_reg_0_0 @[TPU.scala 287:39]
    a_reg_0_2 <= a_reg_0_1 @[TPU.scala 287:39]
    a_reg_1_0 <= io_a_in_1 @[TPU.scala 292:39]
    a_reg_1_1 <= a_reg_1_0 @[TPU.scala 295:39]
    a_reg_1_2 <= a_reg_1_1 @[TPU.scala 295:39]
    a_reg_2_0 <= io_a_in_2 @[TPU.scala 292:39]
    a_reg_2_1 <= a_reg_2_0 @[TPU.scala 295:39]
    a_reg_2_2 <= a_reg_2_1 @[TPU.scala 295:39]
    b_reg_0_0 <= _GEN_0
    b_reg_0_1 <= _GEN_1
    b_reg_0_2 <= _GEN_2
    b_reg_1_0 <= _GEN_3
    b_reg_1_1 <= _GEN_4
    b_reg_1_2 <= _GEN_5
    b_reg_2_0 <= _GEN_6
    b_reg_2_1 <= _GEN_7
    b_reg_2_2 <= _GEN_8
    cms_reg_0_0 <= cmp_input_0_0 @[TPU.scala 298:21]
    cms_reg_0_1 <= cmp_input_0_1 @[TPU.scala 298:21]
    cms_reg_0_2 <= cmp_input_0_2 @[TPU.scala 298:21]
    cms_reg_1_0 <= cmp_input_1_0 @[TPU.scala 298:21]
    cms_reg_1_1 <= cmp_input_1_1 @[TPU.scala 298:21]
    cms_reg_1_2 <= cmp_input_1_2 @[TPU.scala 298:21]
    cms_reg_2_0 <= cmp_input_2_0 @[TPU.scala 298:21]
    cms_reg_2_1 <= cmp_input_2_1 @[TPU.scala 298:21]
    cms_reg_2_2 <= cmp_input_2_2 @[TPU.scala 298:21]

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

    inst actReg of ActReg @[TPU.scala 41:22]
    inst systArr of SystArr @[TPU.scala 42:23]
    reg state : UInt<3>, clock with :
      reset => (UInt<1>("h0"), state) @[TPU.scala 36:22]
    reg cycle : UInt<4>, clock with :
      reset => (UInt<1>("h0"), cycle) @[Counter.scala 61:40]
    node wrap_wrap = eq(cycle, UInt<4>("h9")) @[Counter.scala 73:24]
    node _wrap_value_T = add(cycle, UInt<1>("h1")) @[Counter.scala 77:24]
    node _wrap_value_T_1 = tail(_wrap_value_T, 1) @[Counter.scala 77:24]
    node _GEN_0 = mux(wrap_wrap, UInt<1>("h0"), _wrap_value_T_1) @[Counter.scala 77:15 87:{20,28}]
    node _T = eq(state, UInt<3>("h0")) @[TPU.scala 95:14]
    node _T_2 = eq(state, UInt<3>("h1")) @[TPU.scala 103:19]
    node _T_3 = and(io_b_valid, io_b_ready) @[TPU.scala 104:23]
    node _GEN_24 = mux(_T_3, UInt<1>("h1"), UInt<1>("h0")) @[TPU.scala 104:37 108:32 58:26]
    node _T_4 = eq(state, UInt<3>("h3")) @[TPU.scala 117:19]
    node _GEN_58 = mux(_T_4, UInt<1>("h1"), UInt<1>("h0")) @[TPU.scala 117:32 121:17 51:15]
    node _GEN_83 = mux(_T_2, _GEN_24, _GEN_58) @[TPU.scala 103:28]
    node _GEN_119 = mux(_T, UInt<1>("h0"), _GEN_83) @[TPU.scala 51:15 95:23]
    node counterFlag = _GEN_119 @[TPU.scala 37:25]
    node _GEN_1 = mux(counterFlag, _GEN_0, cycle) @[Counter.scala 118:16 61:40]
    node _GEN_2 = mux(counterFlag, wrap_wrap, UInt<1>("h0")) @[Counter.scala 118:{16,23}]
    reg myOut_0_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), myOut_0_0) @[TPU.scala 43:22]
    reg myOut_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), myOut_0_1) @[TPU.scala 43:22]
    reg myOut_0_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), myOut_0_2) @[TPU.scala 43:22]
    reg myOut_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), myOut_1_0) @[TPU.scala 43:22]
    reg myOut_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), myOut_1_1) @[TPU.scala 43:22]
    reg myOut_1_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), myOut_1_2) @[TPU.scala 43:22]
    reg myOut_2_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), myOut_2_0) @[TPU.scala 43:22]
    reg myOut_2_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), myOut_2_1) @[TPU.scala 43:22]
    reg myOut_2_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), myOut_2_2) @[TPU.scala 43:22]
    reg a_ready : UInt<1>, clock with :
      reset => (UInt<1>("h0"), a_ready) @[TPU.scala 44:24]
    reg b_ready : UInt<1>, clock with :
      reset => (UInt<1>("h0"), b_ready) @[TPU.scala 45:24]
    node _actReg_io_index_T = sub(UInt<3>("h5"), cycle) @[TPU.scala 53:35]
    node _actReg_io_index_T_1 = tail(_actReg_io_index_T, 1) @[TPU.scala 53:35]
    reg act_in_0_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), act_in_0_0) @[TPU.scala 56:23]
    reg act_in_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), act_in_0_1) @[TPU.scala 56:23]
    reg act_in_0_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), act_in_0_2) @[TPU.scala 56:23]
    reg act_in_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), act_in_1_0) @[TPU.scala 56:23]
    reg act_in_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), act_in_1_1) @[TPU.scala 56:23]
    reg act_in_1_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), act_in_1_2) @[TPU.scala 56:23]
    reg act_in_2_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), act_in_2_0) @[TPU.scala 56:23]
    reg act_in_2_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), act_in_2_1) @[TPU.scala 56:23]
    reg act_in_2_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), act_in_2_2) @[TPU.scala 56:23]
    node _T_1 = and(io_a_valid, io_a_ready) @[TPU.scala 96:23]
    node _GEN_3 = mux(_T_1, UInt<3>("h1"), state) @[TPU.scala 96:37 97:15 36:22]
    node _GEN_4 = mux(_T_1, io_a_bits_0_0, act_in_0_0) @[TPU.scala 96:37 98:16 56:23]
    node _GEN_5 = mux(_T_1, io_a_bits_0_1, act_in_0_1) @[TPU.scala 96:37 98:16 56:23]
    node _GEN_6 = mux(_T_1, io_a_bits_0_2, act_in_0_2) @[TPU.scala 96:37 98:16 56:23]
    node _GEN_7 = mux(_T_1, io_a_bits_1_0, act_in_1_0) @[TPU.scala 96:37 98:16 56:23]
    node _GEN_8 = mux(_T_1, io_a_bits_1_1, act_in_1_1) @[TPU.scala 96:37 98:16 56:23]
    node _GEN_9 = mux(_T_1, io_a_bits_1_2, act_in_1_2) @[TPU.scala 96:37 98:16 56:23]
    node _GEN_10 = mux(_T_1, io_a_bits_2_0, act_in_2_0) @[TPU.scala 96:37 98:16 56:23]
    node _GEN_11 = mux(_T_1, io_a_bits_2_1, act_in_2_1) @[TPU.scala 96:37 98:16 56:23]
    node _GEN_12 = mux(_T_1, io_a_bits_2_2, act_in_2_2) @[TPU.scala 96:37 98:16 56:23]
    node _GEN_13 = mux(_T_1, UInt<1>("h0"), a_ready) @[TPU.scala 96:37 99:17 44:24]
    node _GEN_14 = mux(_T_3, UInt<3>("h3"), state) @[TPU.scala 104:37 105:15 36:22]
    node _GEN_15 = mux(_T_3, io_b_bits_0_0, io_b_bits_0_0) @[TPU.scala 104:37 107:25 61:19]
    node _GEN_16 = mux(_T_3, io_b_bits_0_1, io_b_bits_0_1) @[TPU.scala 104:37 107:25 61:19]
    node _GEN_17 = mux(_T_3, io_b_bits_0_2, io_b_bits_0_2) @[TPU.scala 104:37 107:25 61:19]
    node _GEN_18 = mux(_T_3, io_b_bits_1_0, io_b_bits_1_0) @[TPU.scala 104:37 107:25 61:19]
    node _GEN_19 = mux(_T_3, io_b_bits_1_1, io_b_bits_1_1) @[TPU.scala 104:37 107:25 61:19]
    node _GEN_20 = mux(_T_3, io_b_bits_1_2, io_b_bits_1_2) @[TPU.scala 104:37 107:25 61:19]
    node _GEN_21 = mux(_T_3, io_b_bits_2_0, io_b_bits_2_0) @[TPU.scala 104:37 107:25 61:19]
    node _GEN_22 = mux(_T_3, io_b_bits_2_1, io_b_bits_2_1) @[TPU.scala 104:37 107:25 61:19]
    node _GEN_23 = mux(_T_3, io_b_bits_2_2, io_b_bits_2_2) @[TPU.scala 104:37 107:25 61:19]
    node _GEN_25 = mux(_T_3, UInt<1>("h0"), b_ready) @[TPU.scala 104:37 109:17 45:24]
    node _GEN_26 = mux(_T_3, UInt<1>("h0"), _GEN_1) @[TPU.scala 104:37 111:15]
    node _T_5 = eq(cycle, UInt<4>("h9")) @[TPU.scala 122:16]
    node _GEN_27 = mux(_T_5, UInt<3>("h4"), state) @[TPU.scala 122:36 123:13 36:22]
    node _T_6 = geq(cycle, UInt<3>("h4")) @[TPU.scala 125:16]
    node _cycleIdx_T = sub(cycle, UInt<3>("h4")) @[TPU.scala 126:24]
    node _cycleIdx_T_1 = tail(_cycleIdx_T, 1) @[TPU.scala 126:24]
    node _GEN_28 = mux(_T_6, _cycleIdx_T_1, cycle) @[TPU.scala 125:31 126:16 129:16]
    node _T_7 = geq(cycle, UInt<3>("h4")) @[TPU.scala 131:16]
    node _GEN_60 = mux(_T_4, _GEN_28, UInt<1>("h0")) @[TPU.scala 117:32 70:12]
    node _GEN_85 = mux(_T_2, UInt<1>("h0"), _GEN_60) @[TPU.scala 103:28 70:12]
    node _GEN_120 = mux(_T, UInt<1>("h0"), _GEN_85) @[TPU.scala 70:12 95:23]
    node cycleIdx = pad(_GEN_120, 32) @[TPU.scala 64:22]
    node _T_8 = geq(cycleIdx, UInt<1>("h0")) @[TPU.scala 133:22]
    node _T_9 = lt(cycleIdx, UInt<2>("h3")) @[TPU.scala 133:39]
    node _T_10 = and(_T_8, _T_9) @[TPU.scala 133:28]
    node _GEN_29 = mux(_T_10, myOut_1_0, myOut_0_0) @[TPU.scala 133:50 138:31 43:22]
    node _GEN_30 = mux(_T_10, myOut_2_0, myOut_1_0) @[TPU.scala 133:50 138:31 43:22]
    node _GEN_31 = mux(_T_10, systArr.io_out_0, myOut_2_0) @[TPU.scala 133:50 136:31 43:22]
    node _T_11 = geq(cycleIdx, UInt<1>("h1")) @[TPU.scala 133:22]
    node _T_12 = lt(cycleIdx, UInt<3>("h4")) @[TPU.scala 133:39]
    node _T_13 = and(_T_11, _T_12) @[TPU.scala 133:28]
    node _GEN_32 = mux(_T_13, myOut_1_1, myOut_0_1) @[TPU.scala 133:50 138:31 43:22]
    node _GEN_33 = mux(_T_13, myOut_2_1, myOut_1_1) @[TPU.scala 133:50 138:31 43:22]
    node _GEN_34 = mux(_T_13, systArr.io_out_1, myOut_2_1) @[TPU.scala 133:50 136:31 43:22]
    node _T_14 = geq(cycleIdx, UInt<2>("h2")) @[TPU.scala 133:22]
    node _T_15 = lt(cycleIdx, UInt<3>("h5")) @[TPU.scala 133:39]
    node _T_16 = and(_T_14, _T_15) @[TPU.scala 133:28]
    node _GEN_35 = mux(_T_16, myOut_1_2, myOut_0_2) @[TPU.scala 133:50 138:31 43:22]
    node _GEN_36 = mux(_T_16, myOut_2_2, myOut_1_2) @[TPU.scala 133:50 138:31 43:22]
    node _GEN_37 = mux(_T_16, systArr.io_out_2, myOut_2_2) @[TPU.scala 133:50 136:31 43:22]
    node _GEN_38 = mux(_T_7, _GEN_29, myOut_0_0) @[TPU.scala 131:31 43:22]
    node _GEN_39 = mux(_T_7, _GEN_30, myOut_1_0) @[TPU.scala 131:31 43:22]
    node _GEN_40 = mux(_T_7, _GEN_31, myOut_2_0) @[TPU.scala 131:31 43:22]
    node _GEN_41 = mux(_T_7, _GEN_32, myOut_0_1) @[TPU.scala 131:31 43:22]
    node _GEN_42 = mux(_T_7, _GEN_33, myOut_1_1) @[TPU.scala 131:31 43:22]
    node _GEN_43 = mux(_T_7, _GEN_34, myOut_2_1) @[TPU.scala 131:31 43:22]
    node _GEN_44 = mux(_T_7, _GEN_35, myOut_0_2) @[TPU.scala 131:31 43:22]
    node _GEN_45 = mux(_T_7, _GEN_36, myOut_1_2) @[TPU.scala 131:31 43:22]
    node _GEN_46 = mux(_T_7, _GEN_37, myOut_2_2) @[TPU.scala 131:31 43:22]
    node _T_17 = bits(reset, 0, 0) @[TPU.scala 199:11]
    node _T_18 = eq(_T_17, UInt<1>("h0")) @[TPU.scala 199:11]
    node _T_19 = bits(reset, 0, 0) @[TPU.scala 200:11]
    node _T_20 = eq(_T_19, UInt<1>("h0")) @[TPU.scala 200:11]
    node _T_21 = bits(reset, 0, 0) @[TPU.scala 202:13]
    node _T_22 = eq(_T_21, UInt<1>("h0")) @[TPU.scala 202:13]
    node _T_23 = bits(reset, 0, 0) @[TPU.scala 202:13]
    node _T_24 = eq(_T_23, UInt<1>("h0")) @[TPU.scala 202:13]
    node _T_25 = bits(reset, 0, 0) @[TPU.scala 202:13]
    node _T_26 = eq(_T_25, UInt<1>("h0")) @[TPU.scala 202:13]
    node _T_27 = bits(reset, 0, 0) @[TPU.scala 204:11]
    node _T_28 = eq(_T_27, UInt<1>("h0")) @[TPU.scala 204:11]
    node _T_29 = bits(reset, 0, 0) @[TPU.scala 206:13]
    node _T_30 = eq(_T_29, UInt<1>("h0")) @[TPU.scala 206:13]
    node _T_31 = bits(reset, 0, 0) @[TPU.scala 206:13]
    node _T_32 = eq(_T_31, UInt<1>("h0")) @[TPU.scala 206:13]
    node _T_33 = bits(reset, 0, 0) @[TPU.scala 206:13]
    node _T_34 = eq(_T_33, UInt<1>("h0")) @[TPU.scala 206:13]
    node _T_35 = eq(state, UInt<3>("h4")) @[TPU.scala 218:19]
    node _T_36 = bits(reset, 0, 0) @[TPU.scala 219:11]
    node _T_37 = eq(_T_36, UInt<1>("h0")) @[TPU.scala 219:11]
    node _GEN_47 = mux(_T_35, UInt<3>("h1"), state) @[TPU.scala 218:29 220:11 36:22]
    node _GEN_48 = mux(_T_35, UInt<1>("h1"), b_ready) @[TPU.scala 218:29 222:16 48:14]
    node _WIRE_0_0 = asSInt(UInt<32>("h11")) @[TPU.scala 223:{36,36}]
    node _GEN_49 = mux(_T_35, _WIRE_0_0, myOut_0_0) @[TPU.scala 218:29 223:11 43:22]
    node _WIRE_0_1 = asSInt(UInt<32>("h11")) @[TPU.scala 223:{36,36}]
    node _GEN_50 = mux(_T_35, _WIRE_0_1, myOut_0_1) @[TPU.scala 218:29 223:11 43:22]
    node _WIRE_0_2 = asSInt(UInt<32>("h11")) @[TPU.scala 223:{36,36}]
    node _GEN_51 = mux(_T_35, _WIRE_0_2, myOut_0_2) @[TPU.scala 218:29 223:11 43:22]
    node _WIRE_1_0 = asSInt(UInt<32>("h11")) @[TPU.scala 223:{36,36}]
    node _GEN_52 = mux(_T_35, _WIRE_1_0, myOut_1_0) @[TPU.scala 218:29 223:11 43:22]
    node _WIRE_1_1 = asSInt(UInt<32>("h11")) @[TPU.scala 223:{36,36}]
    node _GEN_53 = mux(_T_35, _WIRE_1_1, myOut_1_1) @[TPU.scala 218:29 223:11 43:22]
    node _WIRE_1_2 = asSInt(UInt<32>("h11")) @[TPU.scala 223:{36,36}]
    node _GEN_54 = mux(_T_35, _WIRE_1_2, myOut_1_2) @[TPU.scala 218:29 223:11 43:22]
    node _WIRE_2_0 = asSInt(UInt<32>("h11")) @[TPU.scala 223:{36,36}]
    node _GEN_55 = mux(_T_35, _WIRE_2_0, myOut_2_0) @[TPU.scala 218:29 223:11 43:22]
    node _WIRE_2_1 = asSInt(UInt<32>("h11")) @[TPU.scala 223:{36,36}]
    node _GEN_56 = mux(_T_35, _WIRE_2_1, myOut_2_1) @[TPU.scala 218:29 223:11 43:22]
    node _WIRE_2_2 = asSInt(UInt<32>("h11")) @[TPU.scala 223:{36,36}]
    node _GEN_57 = mux(_T_35, _WIRE_2_2, myOut_2_2) @[TPU.scala 218:29 223:11 43:22]
    node _GEN_59 = mux(_T_4, _GEN_27, _GEN_47) @[TPU.scala 117:32]
    node _GEN_61 = mux(_T_4, _GEN_38, _GEN_49) @[TPU.scala 117:32]
    node _GEN_62 = mux(_T_4, _GEN_39, _GEN_52) @[TPU.scala 117:32]
    node _GEN_63 = mux(_T_4, _GEN_40, _GEN_55) @[TPU.scala 117:32]
    node _GEN_64 = mux(_T_4, _GEN_41, _GEN_50) @[TPU.scala 117:32]
    node _GEN_65 = mux(_T_4, _GEN_42, _GEN_53) @[TPU.scala 117:32]
    node _GEN_66 = mux(_T_4, _GEN_43, _GEN_56) @[TPU.scala 117:32]
    node _GEN_67 = mux(_T_4, _GEN_44, _GEN_51) @[TPU.scala 117:32]
    node _GEN_68 = mux(_T_4, _GEN_45, _GEN_54) @[TPU.scala 117:32]
    node _GEN_69 = mux(_T_4, _GEN_46, _GEN_57) @[TPU.scala 117:32]
    node _GEN_70 = mux(_T_4, b_ready, _GEN_48) @[TPU.scala 117:32 48:14]
    node _GEN_71 = mux(_T_2, _GEN_14, _GEN_59) @[TPU.scala 103:28]
    node _GEN_72 = mux(_T_2, _GEN_15, io_b_bits_0_0) @[TPU.scala 103:28 61:19]
    node _GEN_73 = mux(_T_2, _GEN_16, io_b_bits_0_1) @[TPU.scala 103:28 61:19]
    node _GEN_74 = mux(_T_2, _GEN_17, io_b_bits_0_2) @[TPU.scala 103:28 61:19]
    node _GEN_75 = mux(_T_2, _GEN_18, io_b_bits_1_0) @[TPU.scala 103:28 61:19]
    node _GEN_76 = mux(_T_2, _GEN_19, io_b_bits_1_1) @[TPU.scala 103:28 61:19]
    node _GEN_77 = mux(_T_2, _GEN_20, io_b_bits_1_2) @[TPU.scala 103:28 61:19]
    node _GEN_78 = mux(_T_2, _GEN_21, io_b_bits_2_0) @[TPU.scala 103:28 61:19]
    node _GEN_79 = mux(_T_2, _GEN_22, io_b_bits_2_1) @[TPU.scala 103:28 61:19]
    node _GEN_80 = mux(_T_2, _GEN_23, io_b_bits_2_2) @[TPU.scala 103:28 61:19]
    node _GEN_81 = mux(_T_2, _GEN_24, UInt<1>("h0")) @[TPU.scala 103:28 58:26]
    node _GEN_82 = mux(_T_2, _GEN_25, b_ready) @[TPU.scala 103:28 45:24]
    node _GEN_84 = mux(_T_2, _GEN_26, _GEN_1) @[TPU.scala 103:28]
    node _GEN_86 = mux(_T_2, myOut_0_0, _GEN_61) @[TPU.scala 103:28 43:22]
    node _GEN_87 = mux(_T_2, myOut_1_0, _GEN_62) @[TPU.scala 103:28 43:22]
    node _GEN_88 = mux(_T_2, myOut_2_0, _GEN_63) @[TPU.scala 103:28 43:22]
    node _GEN_89 = mux(_T_2, myOut_0_1, _GEN_64) @[TPU.scala 103:28 43:22]
    node _GEN_90 = mux(_T_2, myOut_1_1, _GEN_65) @[TPU.scala 103:28 43:22]
    node _GEN_91 = mux(_T_2, myOut_2_1, _GEN_66) @[TPU.scala 103:28 43:22]
    node _GEN_92 = mux(_T_2, myOut_0_2, _GEN_67) @[TPU.scala 103:28 43:22]
    node _GEN_93 = mux(_T_2, myOut_1_2, _GEN_68) @[TPU.scala 103:28 43:22]
    node _GEN_94 = mux(_T_2, myOut_2_2, _GEN_69) @[TPU.scala 103:28 43:22]
    node _GEN_95 = mux(_T_2, b_ready, _GEN_70) @[TPU.scala 103:28 48:14]
    node _GEN_96 = mux(_T, _GEN_3, _GEN_71) @[TPU.scala 95:23]
    node _GEN_97 = mux(_T, _GEN_4, act_in_0_0) @[TPU.scala 56:23 95:23]
    node _GEN_98 = mux(_T, _GEN_5, act_in_0_1) @[TPU.scala 56:23 95:23]
    node _GEN_99 = mux(_T, _GEN_6, act_in_0_2) @[TPU.scala 56:23 95:23]
    node _GEN_100 = mux(_T, _GEN_7, act_in_1_0) @[TPU.scala 56:23 95:23]
    node _GEN_101 = mux(_T, _GEN_8, act_in_1_1) @[TPU.scala 56:23 95:23]
    node _GEN_102 = mux(_T, _GEN_9, act_in_1_2) @[TPU.scala 56:23 95:23]
    node _GEN_103 = mux(_T, _GEN_10, act_in_2_0) @[TPU.scala 56:23 95:23]
    node _GEN_104 = mux(_T, _GEN_11, act_in_2_1) @[TPU.scala 56:23 95:23]
    node _GEN_105 = mux(_T, _GEN_12, act_in_2_2) @[TPU.scala 56:23 95:23]
    node _GEN_106 = mux(_T, _GEN_13, a_ready) @[TPU.scala 95:23 44:24]
    node _GEN_107 = mux(_T, UInt<1>("h0"), _GEN_84) @[TPU.scala 101:13 95:23]
    node _GEN_108 = mux(_T, io_b_bits_0_0, _GEN_72) @[TPU.scala 61:19 95:23]
    node _GEN_109 = mux(_T, io_b_bits_0_1, _GEN_73) @[TPU.scala 61:19 95:23]
    node _GEN_110 = mux(_T, io_b_bits_0_2, _GEN_74) @[TPU.scala 61:19 95:23]
    node _GEN_111 = mux(_T, io_b_bits_1_0, _GEN_75) @[TPU.scala 61:19 95:23]
    node _GEN_112 = mux(_T, io_b_bits_1_1, _GEN_76) @[TPU.scala 61:19 95:23]
    node _GEN_113 = mux(_T, io_b_bits_1_2, _GEN_77) @[TPU.scala 61:19 95:23]
    node _GEN_114 = mux(_T, io_b_bits_2_0, _GEN_78) @[TPU.scala 61:19 95:23]
    node _GEN_115 = mux(_T, io_b_bits_2_1, _GEN_79) @[TPU.scala 61:19 95:23]
    node _GEN_116 = mux(_T, io_b_bits_2_2, _GEN_80) @[TPU.scala 61:19 95:23]
    node _GEN_117 = mux(_T, UInt<1>("h0"), _GEN_81) @[TPU.scala 95:23 58:26]
    node _GEN_118 = mux(_T, b_ready, _GEN_82) @[TPU.scala 95:23 45:24]
    node _GEN_121 = mux(_T, myOut_0_0, _GEN_86) @[TPU.scala 43:22 95:23]
    node _GEN_122 = mux(_T, myOut_1_0, _GEN_87) @[TPU.scala 43:22 95:23]
    node _GEN_123 = mux(_T, myOut_2_0, _GEN_88) @[TPU.scala 43:22 95:23]
    node _GEN_124 = mux(_T, myOut_0_1, _GEN_89) @[TPU.scala 43:22 95:23]
    node _GEN_125 = mux(_T, myOut_1_1, _GEN_90) @[TPU.scala 43:22 95:23]
    node _GEN_126 = mux(_T, myOut_2_1, _GEN_91) @[TPU.scala 43:22 95:23]
    node _GEN_127 = mux(_T, myOut_0_2, _GEN_92) @[TPU.scala 43:22 95:23]
    node _GEN_128 = mux(_T, myOut_1_2, _GEN_93) @[TPU.scala 43:22 95:23]
    node _GEN_129 = mux(_T, myOut_2_2, _GEN_94) @[TPU.scala 43:22 95:23]
    node _GEN_130 = mux(_T, b_ready, _GEN_95) @[TPU.scala 48:14 95:23]
    node wrap = _GEN_2
    node _myOut_WIRE_0_0 = asSInt(UInt<32>("h0")) @[TPU.scala 43:{45,45}]
    node _myOut_WIRE_0_1 = asSInt(UInt<32>("h0")) @[TPU.scala 43:{45,45}]
    node _myOut_WIRE_0_2 = asSInt(UInt<32>("h0")) @[TPU.scala 43:{45,45}]
    node _myOut_WIRE_1_0 = asSInt(UInt<32>("h0")) @[TPU.scala 43:{45,45}]
    node _myOut_WIRE_1_1 = asSInt(UInt<32>("h0")) @[TPU.scala 43:{45,45}]
    node _myOut_WIRE_1_2 = asSInt(UInt<32>("h0")) @[TPU.scala 43:{45,45}]
    node _myOut_WIRE_2_0 = asSInt(UInt<32>("h0")) @[TPU.scala 43:{45,45}]
    node _myOut_WIRE_2_1 = asSInt(UInt<32>("h0")) @[TPU.scala 43:{45,45}]
    node _myOut_WIRE_2_2 = asSInt(UInt<32>("h0")) @[TPU.scala 43:{45,45}]
    node _act_in_WIRE_0_0 = asSInt(UInt<32>("h0")) @[TPU.scala 56:{46,46}]
    node _act_in_WIRE_0_1 = asSInt(UInt<32>("h0")) @[TPU.scala 56:{46,46}]
    node _act_in_WIRE_0_2 = asSInt(UInt<32>("h0")) @[TPU.scala 56:{46,46}]
    node _act_in_WIRE_1_0 = asSInt(UInt<32>("h0")) @[TPU.scala 56:{46,46}]
    node _act_in_WIRE_1_1 = asSInt(UInt<32>("h0")) @[TPU.scala 56:{46,46}]
    node _act_in_WIRE_1_2 = asSInt(UInt<32>("h0")) @[TPU.scala 56:{46,46}]
    node _act_in_WIRE_2_0 = asSInt(UInt<32>("h0")) @[TPU.scala 56:{46,46}]
    node _act_in_WIRE_2_1 = asSInt(UInt<32>("h0")) @[TPU.scala 56:{46,46}]
    node _act_in_WIRE_2_2 = asSInt(UInt<32>("h0")) @[TPU.scala 56:{46,46}]
    node cycleIdxCols_0 = UInt<32>("h0") @[TPU.scala 65:26 72:21]
    node cycleIdxCols_1 = UInt<32>("h0") @[TPU.scala 65:26 72:21]
    node cycleIdxCols_2 = UInt<32>("h0") @[TPU.scala 65:26 72:21]
    node cycleIdxRows_0 = UInt<32>("h0") @[TPU.scala 66:26 73:21]
    node cycleIdxRows_1 = UInt<32>("h0") @[TPU.scala 66:26 73:21]
    node cycleIdxRows_2 = UInt<32>("h0") @[TPU.scala 66:26 73:21]
    node systArrOutOffset = UInt<32>("h0") @[TPU.scala 67:30 89:20]
    io_a_ready <= a_ready @[TPU.scala 47:14]
    io_b_ready <= _GEN_130
    io_out_0_0 <= myOut_0_0 @[TPU.scala 49:10]
    io_out_0_1 <= myOut_0_1 @[TPU.scala 49:10]
    io_out_0_2 <= myOut_0_2 @[TPU.scala 49:10]
    io_out_1_0 <= myOut_1_0 @[TPU.scala 49:10]
    io_out_1_1 <= myOut_1_1 @[TPU.scala 49:10]
    io_out_1_2 <= myOut_1_2 @[TPU.scala 49:10]
    io_out_2_0 <= myOut_2_0 @[TPU.scala 49:10]
    io_out_2_1 <= myOut_2_1 @[TPU.scala 49:10]
    io_out_2_2 <= myOut_2_2 @[TPU.scala 49:10]
    io_debug_1_0_0 <= systArr.io_cmp_debug_0_0 @[TPU.scala 77:14]
    io_debug_1_0_1 <= systArr.io_cmp_debug_0_1 @[TPU.scala 77:14]
    io_debug_1_0_2 <= systArr.io_cmp_debug_0_2 @[TPU.scala 77:14]
    io_debug_1_1_0 <= systArr.io_cmp_debug_1_0 @[TPU.scala 77:14]
    io_debug_1_1_1 <= systArr.io_cmp_debug_1_1 @[TPU.scala 77:14]
    io_debug_1_1_2 <= systArr.io_cmp_debug_1_2 @[TPU.scala 77:14]
    io_debug_1_2_0 <= systArr.io_cmp_debug_2_0 @[TPU.scala 77:14]
    io_debug_1_2_1 <= systArr.io_cmp_debug_2_1 @[TPU.scala 77:14]
    io_debug_1_2_2 <= systArr.io_cmp_debug_2_2 @[TPU.scala 77:14]
    io_debug_a_out_0 <= actReg.io_a_out_0 @[TPU.scala 78:18]
    io_debug_a_out_1 <= actReg.io_a_out_1 @[TPU.scala 78:18]
    io_debug_a_out_2 <= actReg.io_a_out_2 @[TPU.scala 78:18]
    io_debug_systreg_out_0 <= systArr.io_out_0 @[TPU.scala 83:24]
    io_debug_systreg_out_1 <= systArr.io_out_1 @[TPU.scala 83:24]
    io_debug_systreg_out_2 <= systArr.io_out_2 @[TPU.scala 83:24]
    io_debug_a_regs_0_0 <= systArr.io_debug_a_regs_0_0 @[TPU.scala 79:19]
    io_debug_a_regs_0_1 <= systArr.io_debug_a_regs_0_1 @[TPU.scala 79:19]
    io_debug_a_regs_0_2 <= systArr.io_debug_a_regs_0_2 @[TPU.scala 79:19]
    io_debug_a_regs_1_0 <= systArr.io_debug_a_regs_1_0 @[TPU.scala 79:19]
    io_debug_a_regs_1_1 <= systArr.io_debug_a_regs_1_1 @[TPU.scala 79:19]
    io_debug_a_regs_1_2 <= systArr.io_debug_a_regs_1_2 @[TPU.scala 79:19]
    io_debug_a_regs_2_0 <= systArr.io_debug_a_regs_2_0 @[TPU.scala 79:19]
    io_debug_a_regs_2_1 <= systArr.io_debug_a_regs_2_1 @[TPU.scala 79:19]
    io_debug_a_regs_2_2 <= systArr.io_debug_a_regs_2_2 @[TPU.scala 79:19]
    io_debug_b_regs_0_0 <= systArr.io_debug_b_regs_0_0 @[TPU.scala 80:19]
    io_debug_b_regs_0_1 <= systArr.io_debug_b_regs_0_1 @[TPU.scala 80:19]
    io_debug_b_regs_0_2 <= systArr.io_debug_b_regs_0_2 @[TPU.scala 80:19]
    io_debug_b_regs_1_0 <= systArr.io_debug_b_regs_1_0 @[TPU.scala 80:19]
    io_debug_b_regs_1_1 <= systArr.io_debug_b_regs_1_1 @[TPU.scala 80:19]
    io_debug_b_regs_1_2 <= systArr.io_debug_b_regs_1_2 @[TPU.scala 80:19]
    io_debug_b_regs_2_0 <= systArr.io_debug_b_regs_2_0 @[TPU.scala 80:19]
    io_debug_b_regs_2_1 <= systArr.io_debug_b_regs_2_1 @[TPU.scala 80:19]
    io_debug_b_regs_2_2 <= systArr.io_debug_b_regs_2_2 @[TPU.scala 80:19]
    io_debug_cycleOut <= pad(cycle, 32) @[TPU.scala 81:21]
    io_debug_00 <= systArr.io_debug_00 @[TPU.scala 82:15]
    io_debug_cycleIdxCols_0 <= cycleIdxCols_0 @[TPU.scala 84:25]
    io_debug_cycleIdxCols_1 <= cycleIdxCols_1 @[TPU.scala 84:25]
    io_debug_cycleIdxCols_2 <= cycleIdxCols_2 @[TPU.scala 84:25]
    io_debug_cycleIdxRows_0 <= cycleIdxRows_0 @[TPU.scala 85:25]
    io_debug_cycleIdxRows_1 <= cycleIdxRows_1 @[TPU.scala 85:25]
    io_debug_cycleIdxRows_2 <= cycleIdxRows_2 @[TPU.scala 85:25]
    io_debug_cycleIdx <= cycleIdx @[TPU.scala 86:21]
    io_debug_systout_upperLim <= SInt<32>("h0") @[TPU.scala 87:29]
    state <= mux(reset, UInt<3>("h0"), _GEN_96) @[TPU.scala 36:{22,22}]
    cycle <= mux(reset, UInt<4>("h0"), _GEN_107) @[Counter.scala 61:{40,40}]
    actReg.clock <= clock
    actReg.reset <= reset
    actReg.io_index <= _actReg_io_index_T_1 @[TPU.scala 53:19]
    actReg.io_a_0_0 <= act_in_0_0 @[TPU.scala 59:15]
    actReg.io_a_0_1 <= act_in_0_1 @[TPU.scala 59:15]
    actReg.io_a_0_2 <= act_in_0_2 @[TPU.scala 59:15]
    actReg.io_a_1_0 <= act_in_1_0 @[TPU.scala 59:15]
    actReg.io_a_1_1 <= act_in_1_1 @[TPU.scala 59:15]
    actReg.io_a_1_2 <= act_in_1_2 @[TPU.scala 59:15]
    actReg.io_a_2_0 <= act_in_2_0 @[TPU.scala 59:15]
    actReg.io_a_2_1 <= act_in_2_1 @[TPU.scala 59:15]
    actReg.io_a_2_2 <= act_in_2_2 @[TPU.scala 59:15]
    systArr.clock <= clock
    systArr.reset <= reset
    systArr.io_a_in_0 <= actReg.io_a_out_0 @[TPU.scala 55:19]
    systArr.io_a_in_1 <= actReg.io_a_out_1 @[TPU.scala 55:19]
    systArr.io_a_in_2 <= actReg.io_a_out_2 @[TPU.scala 55:19]
    systArr.io_b_in_0_0 <= _GEN_108
    systArr.io_b_in_0_1 <= _GEN_109
    systArr.io_b_in_0_2 <= _GEN_110
    systArr.io_b_in_1_0 <= _GEN_111
    systArr.io_b_in_1_1 <= _GEN_112
    systArr.io_b_in_1_2 <= _GEN_113
    systArr.io_b_in_2_0 <= _GEN_114
    systArr.io_b_in_2_1 <= _GEN_115
    systArr.io_b_in_2_2 <= _GEN_116
    systArr.io_b_readingin <= _GEN_117
    myOut_0_0 <= mux(reset, _myOut_WIRE_0_0, _GEN_121) @[TPU.scala 43:{22,22}]
    myOut_0_1 <= mux(reset, _myOut_WIRE_0_1, _GEN_124) @[TPU.scala 43:{22,22}]
    myOut_0_2 <= mux(reset, _myOut_WIRE_0_2, _GEN_127) @[TPU.scala 43:{22,22}]
    myOut_1_0 <= mux(reset, _myOut_WIRE_1_0, _GEN_122) @[TPU.scala 43:{22,22}]
    myOut_1_1 <= mux(reset, _myOut_WIRE_1_1, _GEN_125) @[TPU.scala 43:{22,22}]
    myOut_1_2 <= mux(reset, _myOut_WIRE_1_2, _GEN_128) @[TPU.scala 43:{22,22}]
    myOut_2_0 <= mux(reset, _myOut_WIRE_2_0, _GEN_123) @[TPU.scala 43:{22,22}]
    myOut_2_1 <= mux(reset, _myOut_WIRE_2_1, _GEN_126) @[TPU.scala 43:{22,22}]
    myOut_2_2 <= mux(reset, _myOut_WIRE_2_2, _GEN_129) @[TPU.scala 43:{22,22}]
    a_ready <= mux(reset, UInt<1>("h1"), _GEN_106) @[TPU.scala 44:{24,24}]
    b_ready <= mux(reset, UInt<1>("h1"), _GEN_118) @[TPU.scala 45:{24,24}]
    act_in_0_0 <= mux(reset, _act_in_WIRE_0_0, _GEN_97) @[TPU.scala 56:{23,23}]
    act_in_0_1 <= mux(reset, _act_in_WIRE_0_1, _GEN_98) @[TPU.scala 56:{23,23}]
    act_in_0_2 <= mux(reset, _act_in_WIRE_0_2, _GEN_99) @[TPU.scala 56:{23,23}]
    act_in_1_0 <= mux(reset, _act_in_WIRE_1_0, _GEN_100) @[TPU.scala 56:{23,23}]
    act_in_1_1 <= mux(reset, _act_in_WIRE_1_1, _GEN_101) @[TPU.scala 56:{23,23}]
    act_in_1_2 <= mux(reset, _act_in_WIRE_1_2, _GEN_102) @[TPU.scala 56:{23,23}]
    act_in_2_0 <= mux(reset, _act_in_WIRE_2_0, _GEN_103) @[TPU.scala 56:{23,23}]
    act_in_2_1 <= mux(reset, _act_in_WIRE_2_1, _GEN_104) @[TPU.scala 56:{23,23}]
    act_in_2_2 <= mux(reset, _act_in_WIRE_2_2, _GEN_105) @[TPU.scala 56:{23,23}]
    printf(clock, and(and(and(and(and(UInt<1>("h1"), eq(_T, UInt<1>("h0"))), eq(_T_2, UInt<1>("h0"))), _T_4), _T_18), UInt<1>("h1")), "-------------------------\n") : printf @[TPU.scala 199:11]
    printf(clock, and(and(and(and(and(UInt<1>("h1"), eq(_T, UInt<1>("h0"))), eq(_T_2, UInt<1>("h0"))), _T_4), _T_20), UInt<1>("h1")), "MY TPU OUT: \n") : printf_1 @[TPU.scala 200:11]
    printf(clock, and(and(and(and(and(UInt<1>("h1"), eq(_T, UInt<1>("h0"))), eq(_T_2, UInt<1>("h0"))), _T_4), _T_22), UInt<1>("h1")), "Vec(%d, %d, %d)\n", myOut_0_0, myOut_0_1, myOut_0_2) : printf_2 @[TPU.scala 202:13]
    printf(clock, and(and(and(and(and(UInt<1>("h1"), eq(_T, UInt<1>("h0"))), eq(_T_2, UInt<1>("h0"))), _T_4), _T_24), UInt<1>("h1")), "Vec(%d, %d, %d)\n", myOut_1_0, myOut_1_1, myOut_1_2) : printf_3 @[TPU.scala 202:13]
    printf(clock, and(and(and(and(and(UInt<1>("h1"), eq(_T, UInt<1>("h0"))), eq(_T_2, UInt<1>("h0"))), _T_4), _T_26), UInt<1>("h1")), "Vec(%d, %d, %d)\n", myOut_2_0, myOut_2_1, myOut_2_2) : printf_4 @[TPU.scala 202:13]
    printf(clock, and(and(and(and(and(UInt<1>("h1"), eq(_T, UInt<1>("h0"))), eq(_T_2, UInt<1>("h0"))), _T_4), _T_28), UInt<1>("h1")), "actreg out:\n") : printf_5 @[TPU.scala 204:11]
    printf(clock, and(and(and(and(and(UInt<1>("h1"), eq(_T, UInt<1>("h0"))), eq(_T_2, UInt<1>("h0"))), _T_4), _T_30), UInt<1>("h1")), "%d  ", actReg.io_a_out_0) : printf_6 @[TPU.scala 206:13]
    printf(clock, and(and(and(and(and(UInt<1>("h1"), eq(_T, UInt<1>("h0"))), eq(_T_2, UInt<1>("h0"))), _T_4), _T_32), UInt<1>("h1")), "%d  ", actReg.io_a_out_1) : printf_7 @[TPU.scala 206:13]
    printf(clock, and(and(and(and(and(UInt<1>("h1"), eq(_T, UInt<1>("h0"))), eq(_T_2, UInt<1>("h0"))), _T_4), _T_34), UInt<1>("h1")), "%d  ", actReg.io_a_out_2) : printf_8 @[TPU.scala 206:13]
    printf(clock, and(and(and(and(and(and(UInt<1>("h1"), eq(_T, UInt<1>("h0"))), eq(_T_2, UInt<1>("h0"))), eq(_T_4, UInt<1>("h0"))), _T_35), _T_37), UInt<1>("h1")), "FINISH\n") : printf_9 @[TPU.scala 219:11]
