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
      reset => (UInt<1>("h0"), a_reg_0_0) @[TPU.scala 304:20]
    reg a_reg_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_0_1) @[TPU.scala 304:20]
    reg a_reg_0_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_0_2) @[TPU.scala 304:20]
    reg a_reg_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_1_0) @[TPU.scala 304:20]
    reg a_reg_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_1_1) @[TPU.scala 304:20]
    reg a_reg_1_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_1_2) @[TPU.scala 304:20]
    reg a_reg_2_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_2_0) @[TPU.scala 304:20]
    reg a_reg_2_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_2_1) @[TPU.scala 304:20]
    reg a_reg_2_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_2_2) @[TPU.scala 304:20]
    reg b_reg_0_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_0_0) @[TPU.scala 305:20]
    reg b_reg_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_0_1) @[TPU.scala 305:20]
    reg b_reg_0_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_0_2) @[TPU.scala 305:20]
    reg b_reg_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_1_0) @[TPU.scala 305:20]
    reg b_reg_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_1_1) @[TPU.scala 305:20]
    reg b_reg_1_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_1_2) @[TPU.scala 305:20]
    reg b_reg_2_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_2_0) @[TPU.scala 305:20]
    reg b_reg_2_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_2_1) @[TPU.scala 305:20]
    reg b_reg_2_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_2_2) @[TPU.scala 305:20]
    reg cms_reg_0_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_0_0) @[TPU.scala 306:22]
    reg cms_reg_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_0_1) @[TPU.scala 306:22]
    reg cms_reg_0_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_0_2) @[TPU.scala 306:22]
    reg cms_reg_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_1_0) @[TPU.scala 306:22]
    reg cms_reg_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_1_1) @[TPU.scala 306:22]
    reg cms_reg_1_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_1_2) @[TPU.scala 306:22]
    reg cms_reg_2_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_2_0) @[TPU.scala 306:22]
    reg cms_reg_2_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_2_1) @[TPU.scala 306:22]
    reg cms_reg_2_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_2_2) @[TPU.scala 306:22]
    node _io_debug_00_T = mul(io_a_in_0, b_reg_0_0) @[TPU.scala 310:31]
    node _GEN_0 = mux(io_b_readingin, io_b_in_0_0, b_reg_0_0) @[TPU.scala 311:25 312:13 314:13]
    node _GEN_1 = mux(io_b_readingin, io_b_in_0_1, b_reg_0_1) @[TPU.scala 311:25 312:13 314:13]
    node _GEN_2 = mux(io_b_readingin, io_b_in_0_2, b_reg_0_2) @[TPU.scala 311:25 312:13 314:13]
    node _GEN_3 = mux(io_b_readingin, io_b_in_1_0, b_reg_1_0) @[TPU.scala 311:25 312:13 314:13]
    node _GEN_4 = mux(io_b_readingin, io_b_in_1_1, b_reg_1_1) @[TPU.scala 311:25 312:13 314:13]
    node _GEN_5 = mux(io_b_readingin, io_b_in_1_2, b_reg_1_2) @[TPU.scala 311:25 312:13 314:13]
    node _GEN_6 = mux(io_b_readingin, io_b_in_2_0, b_reg_2_0) @[TPU.scala 311:25 312:13 314:13]
    node _GEN_7 = mux(io_b_readingin, io_b_in_2_1, b_reg_2_1) @[TPU.scala 311:25 312:13 314:13]
    node _GEN_8 = mux(io_b_readingin, io_b_in_2_2, b_reg_2_2) @[TPU.scala 311:25 312:13 314:13]
    node _cmp_input_0_0_T = mul(io_a_in_0, b_reg_0_0) @[TPU.scala 324:60]
    node _cmp_input_0_1_T = mul(a_reg_0_0, b_reg_0_1) @[TPU.scala 327:66]
    node _cmp_input_0_2_T = mul(a_reg_0_1, b_reg_0_2) @[TPU.scala 327:66]
    node _cmp_input_1_0_T = mul(io_a_in_1, b_reg_1_0) @[TPU.scala 332:61]
    node _cmp_input_1_0_T_1 = add(_cmp_input_1_0_T, cms_reg_0_0) @[TPU.scala 332:82]
    node _cmp_input_1_0_T_2 = tail(_cmp_input_1_0_T_1, 1) @[TPU.scala 332:82]
    node _cmp_input_1_0_T_3 = asSInt(_cmp_input_1_0_T_2) @[TPU.scala 332:82]
    node _cmp_input_1_1_T = mul(a_reg_1_0, b_reg_1_1) @[TPU.scala 335:67]
    node _cmp_input_1_1_T_1 = add(_cmp_input_1_1_T, cms_reg_0_1) @[TPU.scala 335:88]
    node _cmp_input_1_1_T_2 = tail(_cmp_input_1_1_T_1, 1) @[TPU.scala 335:88]
    node _cmp_input_1_1_T_3 = asSInt(_cmp_input_1_1_T_2) @[TPU.scala 335:88]
    node _cmp_input_1_2_T = mul(a_reg_1_1, b_reg_1_2) @[TPU.scala 335:67]
    node _cmp_input_1_2_T_1 = add(_cmp_input_1_2_T, cms_reg_0_2) @[TPU.scala 335:88]
    node _cmp_input_1_2_T_2 = tail(_cmp_input_1_2_T_1, 1) @[TPU.scala 335:88]
    node _cmp_input_1_2_T_3 = asSInt(_cmp_input_1_2_T_2) @[TPU.scala 335:88]
    node _cmp_input_2_0_T = mul(io_a_in_2, b_reg_2_0) @[TPU.scala 332:61]
    node _cmp_input_2_0_T_1 = add(_cmp_input_2_0_T, cms_reg_1_0) @[TPU.scala 332:82]
    node _cmp_input_2_0_T_2 = tail(_cmp_input_2_0_T_1, 1) @[TPU.scala 332:82]
    node _cmp_input_2_0_T_3 = asSInt(_cmp_input_2_0_T_2) @[TPU.scala 332:82]
    node _cmp_input_2_1_T = mul(a_reg_2_0, b_reg_2_1) @[TPU.scala 335:67]
    node _cmp_input_2_1_T_1 = add(_cmp_input_2_1_T, cms_reg_1_1) @[TPU.scala 335:88]
    node _cmp_input_2_1_T_2 = tail(_cmp_input_2_1_T_1, 1) @[TPU.scala 335:88]
    node _cmp_input_2_1_T_3 = asSInt(_cmp_input_2_1_T_2) @[TPU.scala 335:88]
    node _cmp_input_2_2_T = mul(a_reg_2_1, b_reg_2_2) @[TPU.scala 335:67]
    node _cmp_input_2_2_T_1 = add(_cmp_input_2_2_T, cms_reg_1_2) @[TPU.scala 335:88]
    node _cmp_input_2_2_T_2 = tail(_cmp_input_2_2_T_1, 1) @[TPU.scala 335:88]
    node _cmp_input_2_2_T_3 = asSInt(_cmp_input_2_2_T_2) @[TPU.scala 335:88]
    node _T = bits(reset, 0, 0) @[TPU.scala 345:11]
    node _T_1 = eq(_T, UInt<1>("h0")) @[TPU.scala 345:11]
    node _T_2 = bits(reset, 0, 0) @[TPU.scala 347:13]
    node _T_3 = eq(_T_2, UInt<1>("h0")) @[TPU.scala 347:13]
    node _T_4 = bits(reset, 0, 0) @[TPU.scala 347:13]
    node _T_5 = eq(_T_4, UInt<1>("h0")) @[TPU.scala 347:13]
    node _T_6 = bits(reset, 0, 0) @[TPU.scala 347:13]
    node _T_7 = eq(_T_6, UInt<1>("h0")) @[TPU.scala 347:13]
    node _T_8 = bits(reset, 0, 0) @[TPU.scala 349:11]
    node _T_9 = eq(_T_8, UInt<1>("h0")) @[TPU.scala 349:11]
    node _T_10 = bits(reset, 0, 0) @[TPU.scala 350:11]
    node _T_11 = eq(_T_10, UInt<1>("h0")) @[TPU.scala 350:11]
    node _T_12 = bits(reset, 0, 0) @[TPU.scala 352:13]
    node _T_13 = eq(_T_12, UInt<1>("h0")) @[TPU.scala 352:13]
    node _T_14 = bits(reset, 0, 0) @[TPU.scala 352:13]
    node _T_15 = eq(_T_14, UInt<1>("h0")) @[TPU.scala 352:13]
    node _T_16 = bits(reset, 0, 0) @[TPU.scala 352:13]
    node _T_17 = eq(_T_16, UInt<1>("h0")) @[TPU.scala 352:13]
    node cmp_input_0_0 = asSInt(bits(_cmp_input_0_0_T, 31, 0)) @[TPU.scala 319:25 324:43]
    node cmp_input_0_1 = asSInt(bits(_cmp_input_0_1_T, 31, 0)) @[TPU.scala 319:25 327:43]
    node cmp_input_0_2 = asSInt(bits(_cmp_input_0_2_T, 31, 0)) @[TPU.scala 319:25 327:43]
    node cmp_input_1_0 = asSInt(bits(_cmp_input_1_0_T_3, 31, 0)) @[TPU.scala 319:25 332:43]
    node cmp_input_1_1 = asSInt(bits(_cmp_input_1_1_T_3, 31, 0)) @[TPU.scala 319:25 335:43]
    node cmp_input_1_2 = asSInt(bits(_cmp_input_1_2_T_3, 31, 0)) @[TPU.scala 319:25 335:43]
    node cmp_input_2_0 = asSInt(bits(_cmp_input_2_0_T_3, 31, 0)) @[TPU.scala 319:25 332:43]
    node cmp_input_2_1 = asSInt(bits(_cmp_input_2_1_T_3, 31, 0)) @[TPU.scala 319:25 335:43]
    node cmp_input_2_2 = asSInt(bits(_cmp_input_2_2_T_3, 31, 0)) @[TPU.scala 319:25 335:43]
    io_out_0 <= cms_reg_2_0 @[TPU.scala 317:19]
    io_out_1 <= cms_reg_2_1 @[TPU.scala 317:19]
    io_out_2 <= cms_reg_2_2 @[TPU.scala 317:19]
    io_cmp_debug_0_0 <= cms_reg_0_0 @[TPU.scala 307:18]
    io_cmp_debug_0_1 <= cms_reg_0_1 @[TPU.scala 307:18]
    io_cmp_debug_0_2 <= cms_reg_0_2 @[TPU.scala 307:18]
    io_cmp_debug_1_0 <= cms_reg_1_0 @[TPU.scala 307:18]
    io_cmp_debug_1_1 <= cms_reg_1_1 @[TPU.scala 307:18]
    io_cmp_debug_1_2 <= cms_reg_1_2 @[TPU.scala 307:18]
    io_cmp_debug_2_0 <= cms_reg_2_0 @[TPU.scala 307:18]
    io_cmp_debug_2_1 <= cms_reg_2_1 @[TPU.scala 307:18]
    io_cmp_debug_2_2 <= cms_reg_2_2 @[TPU.scala 307:18]
    io_debug_b_regs_0_0 <= b_reg_0_0 @[TPU.scala 308:21]
    io_debug_b_regs_0_1 <= b_reg_0_1 @[TPU.scala 308:21]
    io_debug_b_regs_0_2 <= b_reg_0_2 @[TPU.scala 308:21]
    io_debug_b_regs_1_0 <= b_reg_1_0 @[TPU.scala 308:21]
    io_debug_b_regs_1_1 <= b_reg_1_1 @[TPU.scala 308:21]
    io_debug_b_regs_1_2 <= b_reg_1_2 @[TPU.scala 308:21]
    io_debug_b_regs_2_0 <= b_reg_2_0 @[TPU.scala 308:21]
    io_debug_b_regs_2_1 <= b_reg_2_1 @[TPU.scala 308:21]
    io_debug_b_regs_2_2 <= b_reg_2_2 @[TPU.scala 308:21]
    io_debug_a_regs_0_0 <= a_reg_0_0 @[TPU.scala 309:21]
    io_debug_a_regs_0_1 <= a_reg_0_1 @[TPU.scala 309:21]
    io_debug_a_regs_0_2 <= a_reg_0_2 @[TPU.scala 309:21]
    io_debug_a_regs_1_0 <= a_reg_1_0 @[TPU.scala 309:21]
    io_debug_a_regs_1_1 <= a_reg_1_1 @[TPU.scala 309:21]
    io_debug_a_regs_1_2 <= a_reg_1_2 @[TPU.scala 309:21]
    io_debug_a_regs_2_0 <= a_reg_2_0 @[TPU.scala 309:21]
    io_debug_a_regs_2_1 <= a_reg_2_1 @[TPU.scala 309:21]
    io_debug_a_regs_2_2 <= a_reg_2_2 @[TPU.scala 309:21]
    io_debug_00 <= asSInt(bits(_io_debug_00_T, 31, 0)) @[TPU.scala 310:17]
    a_reg_0_0 <= io_a_in_0 @[TPU.scala 325:39]
    a_reg_0_1 <= a_reg_0_0 @[TPU.scala 328:39]
    a_reg_0_2 <= a_reg_0_1 @[TPU.scala 328:39]
    a_reg_1_0 <= io_a_in_1 @[TPU.scala 333:39]
    a_reg_1_1 <= a_reg_1_0 @[TPU.scala 336:39]
    a_reg_1_2 <= a_reg_1_1 @[TPU.scala 336:39]
    a_reg_2_0 <= io_a_in_2 @[TPU.scala 333:39]
    a_reg_2_1 <= a_reg_2_0 @[TPU.scala 336:39]
    a_reg_2_2 <= a_reg_2_1 @[TPU.scala 336:39]
    b_reg_0_0 <= _GEN_0
    b_reg_0_1 <= _GEN_1
    b_reg_0_2 <= _GEN_2
    b_reg_1_0 <= _GEN_3
    b_reg_1_1 <= _GEN_4
    b_reg_1_2 <= _GEN_5
    b_reg_2_0 <= _GEN_6
    b_reg_2_1 <= _GEN_7
    b_reg_2_2 <= _GEN_8
    cms_reg_0_0 <= cmp_input_0_0 @[TPU.scala 339:21]
    cms_reg_0_1 <= cmp_input_0_1 @[TPU.scala 339:21]
    cms_reg_0_2 <= cmp_input_0_2 @[TPU.scala 339:21]
    cms_reg_1_0 <= cmp_input_1_0 @[TPU.scala 339:21]
    cms_reg_1_1 <= cmp_input_1_1 @[TPU.scala 339:21]
    cms_reg_1_2 <= cmp_input_1_2 @[TPU.scala 339:21]
    cms_reg_2_0 <= cmp_input_2_0 @[TPU.scala 339:21]
    cms_reg_2_1 <= cmp_input_2_1 @[TPU.scala 339:21]
    cms_reg_2_2 <= cmp_input_2_2 @[TPU.scala 339:21]
    printf(clock, and(and(UInt<1>("h1"), _T_1), UInt<1>("h1")), "MY A_IN:\n") : printf @[TPU.scala 345:11]
    printf(clock, and(and(UInt<1>("h1"), _T_3), UInt<1>("h1")), "%d ", io_a_in_0) : printf_1 @[TPU.scala 347:13]
    printf(clock, and(and(UInt<1>("h1"), _T_5), UInt<1>("h1")), "%d ", io_a_in_1) : printf_2 @[TPU.scala 347:13]
    printf(clock, and(and(UInt<1>("h1"), _T_7), UInt<1>("h1")), "%d ", io_a_in_2) : printf_3 @[TPU.scala 347:13]
    printf(clock, and(and(UInt<1>("h1"), _T_9), UInt<1>("h1")), "\n\n") : printf_4 @[TPU.scala 349:11]
    printf(clock, and(and(UInt<1>("h1"), _T_11), UInt<1>("h1")), "MY B_IN:\n") : printf_5 @[TPU.scala 350:11]
    printf(clock, and(and(UInt<1>("h1"), _T_13), UInt<1>("h1")), "Vec(%d, %d, %d)\n", io_b_in_0_0, io_b_in_0_1, io_b_in_0_2) : printf_6 @[TPU.scala 352:13]
    printf(clock, and(and(UInt<1>("h1"), _T_15), UInt<1>("h1")), "Vec(%d, %d, %d)\n", io_b_in_1_0, io_b_in_1_1, io_b_in_1_2) : printf_7 @[TPU.scala 352:13]
    printf(clock, and(and(UInt<1>("h1"), _T_17), UInt<1>("h1")), "Vec(%d, %d, %d)\n", io_b_in_2_0, io_b_in_2_1, io_b_in_2_2) : printf_8 @[TPU.scala 352:13]
