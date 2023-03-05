circuit ChiselTPU :
  module ActReg :
    input clock : Clock
    input reset : UInt<1>
    input io_index : UInt<4>
    input io_a_0_0 : SInt<8>
    input io_a_0_1 : SInt<8>
    input io_a_0_2 : SInt<8>
    input io_a_1_0 : SInt<8>
    input io_a_1_1 : SInt<8>
    input io_a_1_2 : SInt<8>
    input io_a_2_0 : SInt<8>
    input io_a_2_1 : SInt<8>
    input io_a_2_2 : SInt<8>
    output io_a_out_0 : SInt<8>
    output io_a_out_1 : SInt<8>
    output io_a_out_2 : SInt<8>

    node _T = lt(io_index, UInt<3>("h5")) @[TPU.scala 81:19]
    node _T_1 = asSInt(io_index) @[TPU.scala 83:35]
    node _T_2 = sub(asSInt(UInt<3>("h2")), _T_1) @[TPU.scala 83:25]
    node _T_3 = tail(_T_2, 1) @[TPU.scala 83:25]
    node _T_4 = asSInt(_T_3) @[TPU.scala 83:25]
    node _T_5 = leq(_T_4, asSInt(UInt<1>("h0"))) @[TPU.scala 83:42]
    node _T_6 = asSInt(io_index) @[TPU.scala 83:77]
    node _T_7 = sub(asSInt(UInt<4>("h5")), _T_6) @[TPU.scala 83:67]
    node _T_8 = tail(_T_7, 1) @[TPU.scala 83:67]
    node _T_9 = asSInt(_T_8) @[TPU.scala 83:67]
    node _T_10 = gt(_T_9, asSInt(UInt<1>("h0"))) @[TPU.scala 83:84]
    node _T_11 = and(_T_5, _T_10) @[TPU.scala 83:49]
    node _io_a_out_0_T = sub(UInt<3>("h4"), io_index) @[TPU.scala 84:55]
    node _io_a_out_0_T_1 = tail(_io_a_out_0_T, 1) @[TPU.scala 84:55]
    node _io_a_out_0_T_2 = bits(_io_a_out_0_T_1, 1, 0)
    node _GEN_0 = validif(eq(UInt<1>("h0"), _io_a_out_0_T_2), io_a_0_0) @[TPU.scala 84:{25,25}]
    node _GEN_1 = mux(eq(UInt<1>("h1"), _io_a_out_0_T_2), io_a_1_0, _GEN_0) @[TPU.scala 84:{25,25}]
    node _GEN_2 = mux(eq(UInt<2>("h2"), _io_a_out_0_T_2), io_a_2_0, _GEN_1) @[TPU.scala 84:{25,25}]
    node _io_a_io_a_out_0_T_2_0 = _GEN_2 @[TPU.scala 84:25]
    node _GEN_3 = mux(_T_11, _io_a_io_a_out_0_T_2_0, asSInt(UInt<1>("h0"))) @[TPU.scala 83:90 84:25 87:25]
    node _T_12 = asSInt(io_index) @[TPU.scala 83:35]
    node _T_13 = sub(asSInt(UInt<2>("h1")), _T_12) @[TPU.scala 83:25]
    node _T_14 = tail(_T_13, 1) @[TPU.scala 83:25]
    node _T_15 = asSInt(_T_14) @[TPU.scala 83:25]
    node _T_16 = leq(_T_15, asSInt(UInt<1>("h0"))) @[TPU.scala 83:42]
    node _T_17 = asSInt(io_index) @[TPU.scala 83:77]
    node _T_18 = sub(asSInt(UInt<4>("h4")), _T_17) @[TPU.scala 83:67]
    node _T_19 = tail(_T_18, 1) @[TPU.scala 83:67]
    node _T_20 = asSInt(_T_19) @[TPU.scala 83:67]
    node _T_21 = gt(_T_20, asSInt(UInt<1>("h0"))) @[TPU.scala 83:84]
    node _T_22 = and(_T_16, _T_21) @[TPU.scala 83:49]
    node _io_a_out_1_T = sub(UInt<2>("h3"), io_index) @[TPU.scala 84:55]
    node _io_a_out_1_T_1 = tail(_io_a_out_1_T, 1) @[TPU.scala 84:55]
    node _io_a_out_1_T_2 = bits(_io_a_out_1_T_1, 1, 0)
    node _GEN_4 = validif(eq(UInt<1>("h0"), _io_a_out_1_T_2), io_a_0_1) @[TPU.scala 84:{25,25}]
    node _GEN_5 = mux(eq(UInt<1>("h1"), _io_a_out_1_T_2), io_a_1_1, _GEN_4) @[TPU.scala 84:{25,25}]
    node _GEN_6 = mux(eq(UInt<2>("h2"), _io_a_out_1_T_2), io_a_2_1, _GEN_5) @[TPU.scala 84:{25,25}]
    node _io_a_io_a_out_1_T_2_1 = _GEN_6 @[TPU.scala 84:25]
    node _GEN_7 = mux(_T_22, _io_a_io_a_out_1_T_2_1, asSInt(UInt<1>("h0"))) @[TPU.scala 83:90 84:25 87:25]
    node _T_23 = asSInt(io_index) @[TPU.scala 83:35]
    node _T_24 = sub(asSInt(UInt<1>("h0")), _T_23) @[TPU.scala 83:25]
    node _T_25 = tail(_T_24, 1) @[TPU.scala 83:25]
    node _T_26 = asSInt(_T_25) @[TPU.scala 83:25]
    node _T_27 = leq(_T_26, asSInt(UInt<1>("h0"))) @[TPU.scala 83:42]
    node _T_28 = asSInt(io_index) @[TPU.scala 83:77]
    node _T_29 = sub(asSInt(UInt<3>("h3")), _T_28) @[TPU.scala 83:67]
    node _T_30 = tail(_T_29, 1) @[TPU.scala 83:67]
    node _T_31 = asSInt(_T_30) @[TPU.scala 83:67]
    node _T_32 = gt(_T_31, asSInt(UInt<1>("h0"))) @[TPU.scala 83:84]
    node _T_33 = and(_T_27, _T_32) @[TPU.scala 83:49]
    node _io_a_out_2_T = sub(UInt<2>("h2"), io_index) @[TPU.scala 84:55]
    node _io_a_out_2_T_1 = tail(_io_a_out_2_T, 1) @[TPU.scala 84:55]
    node _io_a_out_2_T_2 = bits(_io_a_out_2_T_1, 1, 0)
    node _GEN_8 = validif(eq(UInt<1>("h0"), _io_a_out_2_T_2), io_a_0_2) @[TPU.scala 84:{25,25}]
    node _GEN_9 = mux(eq(UInt<1>("h1"), _io_a_out_2_T_2), io_a_1_2, _GEN_8) @[TPU.scala 84:{25,25}]
    node _GEN_10 = mux(eq(UInt<2>("h2"), _io_a_out_2_T_2), io_a_2_2, _GEN_9) @[TPU.scala 84:{25,25}]
    node _io_a_io_a_out_2_T_2_2 = _GEN_10 @[TPU.scala 84:25]
    node _GEN_11 = mux(_T_33, _io_a_io_a_out_2_T_2_2, asSInt(UInt<1>("h0"))) @[TPU.scala 83:90 84:25 87:25]
    node _GEN_12 = mux(_T, _GEN_3, asSInt(UInt<1>("h0"))) @[TPU.scala 81:35 93:23]
    node _GEN_13 = mux(_T, _GEN_7, asSInt(UInt<1>("h0"))) @[TPU.scala 81:35 93:23]
    node _GEN_14 = mux(_T, _GEN_11, asSInt(UInt<1>("h0"))) @[TPU.scala 81:35 93:23]
    io_a_out_0 <= _GEN_12
    io_a_out_1 <= _GEN_13
    io_a_out_2 <= _GEN_14

  module SystArr :
    input clock : Clock
    input reset : UInt<1>
    input io_a_in_0 : SInt<8>
    input io_a_in_1 : SInt<8>
    input io_a_in_2 : SInt<8>
    input io_b_in_0_0 : SInt<8>
    input io_b_in_0_1 : SInt<8>
    input io_b_in_0_2 : SInt<8>
    input io_b_in_1_0 : SInt<8>
    input io_b_in_1_1 : SInt<8>
    input io_b_in_1_2 : SInt<8>
    input io_b_in_2_0 : SInt<8>
    input io_b_in_2_1 : SInt<8>
    input io_b_in_2_2 : SInt<8>
    output io_out_0 : SInt<8>
    output io_out_1 : SInt<8>
    output io_out_2 : SInt<8>
    output io_cmp_debug_0_0 : SInt<8>
    output io_cmp_debug_0_1 : SInt<8>
    output io_cmp_debug_0_2 : SInt<8>
    output io_cmp_debug_1_0 : SInt<8>
    output io_cmp_debug_1_1 : SInt<8>
    output io_cmp_debug_1_2 : SInt<8>
    output io_cmp_debug_2_0 : SInt<8>
    output io_cmp_debug_2_1 : SInt<8>
    output io_cmp_debug_2_2 : SInt<8>
    output io_b_reg_debug_0_0 : SInt<8>
    output io_b_reg_debug_0_1 : SInt<8>
    output io_b_reg_debug_0_2 : SInt<8>
    output io_b_reg_debug_1_0 : SInt<8>
    output io_b_reg_debug_1_1 : SInt<8>
    output io_b_reg_debug_1_2 : SInt<8>
    output io_b_reg_debug_2_0 : SInt<8>
    output io_b_reg_debug_2_1 : SInt<8>
    output io_b_reg_debug_2_2 : SInt<8>

    reg a_reg_0_0 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), a_reg_0_0) @[TPU.scala 107:20]
    reg a_reg_0_1 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), a_reg_0_1) @[TPU.scala 107:20]
    reg a_reg_0_2 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), a_reg_0_2) @[TPU.scala 107:20]
    reg a_reg_1_0 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), a_reg_1_0) @[TPU.scala 107:20]
    reg a_reg_1_1 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), a_reg_1_1) @[TPU.scala 107:20]
    reg a_reg_1_2 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), a_reg_1_2) @[TPU.scala 107:20]
    reg a_reg_2_0 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), a_reg_2_0) @[TPU.scala 107:20]
    reg a_reg_2_1 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), a_reg_2_1) @[TPU.scala 107:20]
    reg a_reg_2_2 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), a_reg_2_2) @[TPU.scala 107:20]
    reg b_reg_0_0 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), b_reg_0_0) @[TPU.scala 108:20]
    reg b_reg_0_1 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), b_reg_0_1) @[TPU.scala 108:20]
    reg b_reg_0_2 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), b_reg_0_2) @[TPU.scala 108:20]
    reg b_reg_1_0 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), b_reg_1_0) @[TPU.scala 108:20]
    reg b_reg_1_1 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), b_reg_1_1) @[TPU.scala 108:20]
    reg b_reg_1_2 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), b_reg_1_2) @[TPU.scala 108:20]
    reg b_reg_2_0 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), b_reg_2_0) @[TPU.scala 108:20]
    reg b_reg_2_1 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), b_reg_2_1) @[TPU.scala 108:20]
    reg b_reg_2_2 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), b_reg_2_2) @[TPU.scala 108:20]
    reg cms_reg_0_0 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), cms_reg_0_0) @[TPU.scala 109:22]
    reg cms_reg_0_1 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), cms_reg_0_1) @[TPU.scala 109:22]
    reg cms_reg_0_2 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), cms_reg_0_2) @[TPU.scala 109:22]
    reg cms_reg_1_0 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), cms_reg_1_0) @[TPU.scala 109:22]
    reg cms_reg_1_1 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), cms_reg_1_1) @[TPU.scala 109:22]
    reg cms_reg_1_2 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), cms_reg_1_2) @[TPU.scala 109:22]
    reg cms_reg_2_0 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), cms_reg_2_0) @[TPU.scala 109:22]
    reg cms_reg_2_1 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), cms_reg_2_1) @[TPU.scala 109:22]
    reg cms_reg_2_2 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), cms_reg_2_2) @[TPU.scala 109:22]
    node _cmp_input_T = mul(io_a_in_0, b_reg_0_0) @[TPU.scala 121:48]
    node _cmp_input_T_1 = mul(a_reg_0_0, b_reg_0_1) @[TPU.scala 124:54]
    node _cmp_input_T_2 = mul(a_reg_0_1, b_reg_0_2) @[TPU.scala 124:54]
    node _cmp_input_T_3 = mul(io_a_in_1, b_reg_1_0) @[TPU.scala 129:49]
    node _cmp_input_T_4 = add(_cmp_input_T_3, cms_reg_0_0) @[TPU.scala 129:70]
    node _cmp_input_T_5 = tail(_cmp_input_T_4, 1) @[TPU.scala 129:70]
    node _cmp_input_T_6 = asSInt(_cmp_input_T_5) @[TPU.scala 129:70]
    node _cmp_input_T_7 = mul(a_reg_1_0, b_reg_1_1) @[TPU.scala 132:55]
    node _cmp_input_T_8 = add(_cmp_input_T_7, cms_reg_0_1) @[TPU.scala 132:76]
    node _cmp_input_T_9 = tail(_cmp_input_T_8, 1) @[TPU.scala 132:76]
    node _cmp_input_T_10 = asSInt(_cmp_input_T_9) @[TPU.scala 132:76]
    node _cmp_input_T_11 = mul(a_reg_1_1, b_reg_1_2) @[TPU.scala 132:55]
    node _cmp_input_T_12 = add(_cmp_input_T_11, cms_reg_0_2) @[TPU.scala 132:76]
    node _cmp_input_T_13 = tail(_cmp_input_T_12, 1) @[TPU.scala 132:76]
    node _cmp_input_T_14 = asSInt(_cmp_input_T_13) @[TPU.scala 132:76]
    node _cmp_input_T_15 = mul(io_a_in_2, b_reg_2_0) @[TPU.scala 129:49]
    node _cmp_input_T_16 = add(_cmp_input_T_15, cms_reg_1_0) @[TPU.scala 129:70]
    node _cmp_input_T_17 = tail(_cmp_input_T_16, 1) @[TPU.scala 129:70]
    node _cmp_input_T_18 = asSInt(_cmp_input_T_17) @[TPU.scala 129:70]
    node _cmp_input_T_19 = mul(a_reg_2_0, b_reg_2_1) @[TPU.scala 132:55]
    node _cmp_input_T_20 = add(_cmp_input_T_19, cms_reg_1_1) @[TPU.scala 132:76]
    node _cmp_input_T_21 = tail(_cmp_input_T_20, 1) @[TPU.scala 132:76]
    node _cmp_input_T_22 = asSInt(_cmp_input_T_21) @[TPU.scala 132:76]
    node _cmp_input_T_23 = mul(a_reg_2_1, b_reg_2_2) @[TPU.scala 132:55]
    node _cmp_input_T_24 = add(_cmp_input_T_23, cms_reg_1_2) @[TPU.scala 132:76]
    node _cmp_input_T_25 = tail(_cmp_input_T_24, 1) @[TPU.scala 132:76]
    node _cmp_input_T_26 = asSInt(_cmp_input_T_25) @[TPU.scala 132:76]
    node cmp_input = asSInt(bits(_cmp_input_T, 7, 0)) @[TPU.scala 118:33 121:31]
    node cmp_input_1 = asSInt(bits(_cmp_input_T_1, 7, 0)) @[TPU.scala 118:33 124:31]
    node cmp_input_2 = asSInt(bits(_cmp_input_T_2, 7, 0)) @[TPU.scala 118:33 124:31]
    node cmp_input_3 = asSInt(bits(_cmp_input_T_6, 7, 0)) @[TPU.scala 118:33 129:31]
    node cmp_input_4 = asSInt(bits(_cmp_input_T_10, 7, 0)) @[TPU.scala 118:33 132:31]
    node cmp_input_5 = asSInt(bits(_cmp_input_T_14, 7, 0)) @[TPU.scala 118:33 132:31]
    node cmp_input_6 = asSInt(bits(_cmp_input_T_18, 7, 0)) @[TPU.scala 118:33 129:31]
    node cmp_input_7 = asSInt(bits(_cmp_input_T_22, 7, 0)) @[TPU.scala 118:33 132:31]
    node cmp_input_8 = asSInt(bits(_cmp_input_T_26, 7, 0)) @[TPU.scala 118:33 132:31]
    io_out_0 <= cms_reg_2_0 @[TPU.scala 114:19]
    io_out_1 <= cms_reg_2_1 @[TPU.scala 114:19]
    io_out_2 <= cms_reg_2_2 @[TPU.scala 114:19]
    io_cmp_debug_0_0 <= cms_reg_0_0 @[TPU.scala 110:18]
    io_cmp_debug_0_1 <= cms_reg_0_1 @[TPU.scala 110:18]
    io_cmp_debug_0_2 <= cms_reg_0_2 @[TPU.scala 110:18]
    io_cmp_debug_1_0 <= cms_reg_1_0 @[TPU.scala 110:18]
    io_cmp_debug_1_1 <= cms_reg_1_1 @[TPU.scala 110:18]
    io_cmp_debug_1_2 <= cms_reg_1_2 @[TPU.scala 110:18]
    io_cmp_debug_2_0 <= cms_reg_2_0 @[TPU.scala 110:18]
    io_cmp_debug_2_1 <= cms_reg_2_1 @[TPU.scala 110:18]
    io_cmp_debug_2_2 <= cms_reg_2_2 @[TPU.scala 110:18]
    io_b_reg_debug_0_0 <= b_reg_0_0 @[TPU.scala 111:20]
    io_b_reg_debug_0_1 <= b_reg_0_1 @[TPU.scala 111:20]
    io_b_reg_debug_0_2 <= b_reg_0_2 @[TPU.scala 111:20]
    io_b_reg_debug_1_0 <= b_reg_1_0 @[TPU.scala 111:20]
    io_b_reg_debug_1_1 <= b_reg_1_1 @[TPU.scala 111:20]
    io_b_reg_debug_1_2 <= b_reg_1_2 @[TPU.scala 111:20]
    io_b_reg_debug_2_0 <= b_reg_2_0 @[TPU.scala 111:20]
    io_b_reg_debug_2_1 <= b_reg_2_1 @[TPU.scala 111:20]
    io_b_reg_debug_2_2 <= b_reg_2_2 @[TPU.scala 111:20]
    a_reg_0_0 <= io_a_in_0 @[TPU.scala 122:39]
    a_reg_0_1 <= a_reg_0_0 @[TPU.scala 125:39]
    a_reg_0_2 <= a_reg_0_1 @[TPU.scala 125:39]
    a_reg_1_0 <= io_a_in_1 @[TPU.scala 130:39]
    a_reg_1_1 <= a_reg_1_0 @[TPU.scala 133:39]
    a_reg_1_2 <= a_reg_1_1 @[TPU.scala 133:39]
    a_reg_2_0 <= io_a_in_2 @[TPU.scala 130:39]
    a_reg_2_1 <= a_reg_2_0 @[TPU.scala 133:39]
    a_reg_2_2 <= a_reg_2_1 @[TPU.scala 133:39]
    b_reg_0_0 <= io_b_in_0_0 @[TPU.scala 112:11]
    b_reg_0_1 <= io_b_in_0_1 @[TPU.scala 112:11]
    b_reg_0_2 <= io_b_in_0_2 @[TPU.scala 112:11]
    b_reg_1_0 <= io_b_in_1_0 @[TPU.scala 112:11]
    b_reg_1_1 <= io_b_in_1_1 @[TPU.scala 112:11]
    b_reg_1_2 <= io_b_in_1_2 @[TPU.scala 112:11]
    b_reg_2_0 <= io_b_in_2_0 @[TPU.scala 112:11]
    b_reg_2_1 <= io_b_in_2_1 @[TPU.scala 112:11]
    b_reg_2_2 <= io_b_in_2_2 @[TPU.scala 112:11]
    cms_reg_0_0 <= cmp_input @[TPU.scala 136:33]
    cms_reg_0_1 <= cmp_input_1 @[TPU.scala 136:33]
    cms_reg_0_2 <= cmp_input_2 @[TPU.scala 136:33]
    cms_reg_1_0 <= cmp_input_3 @[TPU.scala 136:33]
    cms_reg_1_1 <= cmp_input_4 @[TPU.scala 136:33]
    cms_reg_1_2 <= cmp_input_5 @[TPU.scala 136:33]
    cms_reg_2_0 <= cmp_input_6 @[TPU.scala 136:33]
    cms_reg_2_1 <= cmp_input_7 @[TPU.scala 136:33]
    cms_reg_2_2 <= cmp_input_8 @[TPU.scala 136:33]

  module ChiselTPU :
    input clock : Clock
    input reset : UInt<1>
    output io_a_ready : UInt<1>
    input io_a_valid : UInt<1>
    input io_a_bits_0_0 : SInt<8>
    input io_a_bits_0_1 : SInt<8>
    input io_a_bits_0_2 : SInt<8>
    input io_a_bits_1_0 : SInt<8>
    input io_a_bits_1_1 : SInt<8>
    input io_a_bits_1_2 : SInt<8>
    input io_a_bits_2_0 : SInt<8>
    input io_a_bits_2_1 : SInt<8>
    input io_a_bits_2_2 : SInt<8>
    output io_b_ready : UInt<1>
    input io_b_valid : UInt<1>
    input io_b_bits_0_0 : SInt<8>
    input io_b_bits_0_1 : SInt<8>
    input io_b_bits_0_2 : SInt<8>
    input io_b_bits_1_0 : SInt<8>
    input io_b_bits_1_1 : SInt<8>
    input io_b_bits_1_2 : SInt<8>
    input io_b_bits_2_0 : SInt<8>
    input io_b_bits_2_1 : SInt<8>
    input io_b_bits_2_2 : SInt<8>
    output io_out_0_0 : SInt<8>
    output io_out_0_1 : SInt<8>
    output io_out_0_2 : SInt<8>
    output io_out_1_0 : SInt<8>
    output io_out_1_1 : SInt<8>
    output io_out_1_2 : SInt<8>
    output io_out_2_0 : SInt<8>
    output io_out_2_1 : SInt<8>
    output io_out_2_2 : SInt<8>

    inst actReg of ActReg @[TPU.scala 18:22]
    inst systArr of SystArr @[TPU.scala 20:23]
    reg state : UInt<2>, clock with :
      reset => (UInt<1>("h0"), state) @[TPU.scala 15:22]
    node _T = eq(state, UInt<2>("h2")) @[TPU.scala 16:37]
    reg cycle : UInt<4>, clock with :
      reset => (UInt<1>("h0"), cycle) @[Counter.scala 61:40]
    node wrap_wrap = eq(cycle, UInt<4>("h9")) @[Counter.scala 73:24]
    node _wrap_value_T = add(cycle, UInt<1>("h1")) @[Counter.scala 77:24]
    node _wrap_value_T_1 = tail(_wrap_value_T, 1) @[Counter.scala 77:24]
    node _GEN_0 = mux(wrap_wrap, UInt<1>("h0"), _wrap_value_T_1) @[Counter.scala 77:15 87:{20,28}]
    node _GEN_1 = mux(_T, _GEN_0, cycle) @[Counter.scala 118:16 61:40]
    node _GEN_2 = mux(_T, wrap_wrap, UInt<1>("h0")) @[Counter.scala 118:{16,23}]
    reg myOut_0_0 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), myOut_0_0) @[TPU.scala 21:22]
    reg myOut_0_1 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), myOut_0_1) @[TPU.scala 21:22]
    reg myOut_0_2 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), myOut_0_2) @[TPU.scala 21:22]
    reg myOut_1_0 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), myOut_1_0) @[TPU.scala 21:22]
    reg myOut_1_1 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), myOut_1_1) @[TPU.scala 21:22]
    reg myOut_1_2 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), myOut_1_2) @[TPU.scala 21:22]
    reg myOut_2_0 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), myOut_2_0) @[TPU.scala 21:22]
    reg myOut_2_1 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), myOut_2_1) @[TPU.scala 21:22]
    reg myOut_2_2 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), myOut_2_2) @[TPU.scala 21:22]
    reg a_ready : UInt<1>, clock with :
      reset => (UInt<1>("h0"), a_ready) @[TPU.scala 22:24]
    reg b_ready : UInt<1>, clock with :
      reset => (UInt<1>("h0"), b_ready) @[TPU.scala 23:24]
    node _actReg_io_index_T = sub(UInt<3>("h4"), cycle) @[TPU.scala 30:35]
    node _actReg_io_index_T_1 = tail(_actReg_io_index_T, 1) @[TPU.scala 30:35]
    reg REG_0 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), REG_0) @[TPU.scala 31:29]
    reg REG_1 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), REG_1) @[TPU.scala 31:29]
    reg REG_2 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), REG_2) @[TPU.scala 31:29]
    reg act_in_0_0 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), act_in_0_0) @[TPU.scala 32:23]
    reg act_in_0_1 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), act_in_0_1) @[TPU.scala 32:23]
    reg act_in_0_2 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), act_in_0_2) @[TPU.scala 32:23]
    reg act_in_1_0 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), act_in_1_0) @[TPU.scala 32:23]
    reg act_in_1_1 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), act_in_1_1) @[TPU.scala 32:23]
    reg act_in_1_2 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), act_in_1_2) @[TPU.scala 32:23]
    reg act_in_2_0 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), act_in_2_0) @[TPU.scala 32:23]
    reg act_in_2_1 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), act_in_2_1) @[TPU.scala 32:23]
    reg act_in_2_2 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), act_in_2_2) @[TPU.scala 32:23]
    reg syst_in_0_0 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), syst_in_0_0) @[TPU.scala 33:24]
    reg syst_in_0_1 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), syst_in_0_1) @[TPU.scala 33:24]
    reg syst_in_0_2 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), syst_in_0_2) @[TPU.scala 33:24]
    reg syst_in_1_0 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), syst_in_1_0) @[TPU.scala 33:24]
    reg syst_in_1_1 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), syst_in_1_1) @[TPU.scala 33:24]
    reg syst_in_1_2 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), syst_in_1_2) @[TPU.scala 33:24]
    reg syst_in_2_0 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), syst_in_2_0) @[TPU.scala 33:24]
    reg syst_in_2_1 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), syst_in_2_1) @[TPU.scala 33:24]
    reg syst_in_2_2 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), syst_in_2_2) @[TPU.scala 33:24]
    node _T_1 = eq(state, UInt<2>("h0")) @[TPU.scala 37:14]
    node _T_2 = and(io_a_valid, io_a_ready) @[TPU.scala 38:23]
    node _GEN_3 = mux(_T_2, UInt<2>("h1"), state) @[TPU.scala 38:37 39:15 15:22]
    node _GEN_4 = mux(_T_2, io_a_bits_0_0, act_in_0_0) @[TPU.scala 38:37 40:16 32:23]
    node _GEN_5 = mux(_T_2, io_a_bits_0_1, act_in_0_1) @[TPU.scala 38:37 40:16 32:23]
    node _GEN_6 = mux(_T_2, io_a_bits_0_2, act_in_0_2) @[TPU.scala 38:37 40:16 32:23]
    node _GEN_7 = mux(_T_2, io_a_bits_1_0, act_in_1_0) @[TPU.scala 38:37 40:16 32:23]
    node _GEN_8 = mux(_T_2, io_a_bits_1_1, act_in_1_1) @[TPU.scala 38:37 40:16 32:23]
    node _GEN_9 = mux(_T_2, io_a_bits_1_2, act_in_1_2) @[TPU.scala 38:37 40:16 32:23]
    node _GEN_10 = mux(_T_2, io_a_bits_2_0, act_in_2_0) @[TPU.scala 38:37 40:16 32:23]
    node _GEN_11 = mux(_T_2, io_a_bits_2_1, act_in_2_1) @[TPU.scala 38:37 40:16 32:23]
    node _GEN_12 = mux(_T_2, io_a_bits_2_2, act_in_2_2) @[TPU.scala 38:37 40:16 32:23]
    node _GEN_13 = mux(_T_2, UInt<1>("h0"), a_ready) @[TPU.scala 38:37 41:17 22:24]
    node _T_3 = eq(state, UInt<2>("h1")) @[TPU.scala 44:19]
    node _T_4 = and(io_b_valid, io_b_ready) @[TPU.scala 45:23]
    node _GEN_14 = mux(_T_4, UInt<2>("h2"), state) @[TPU.scala 45:37 46:15 15:22]
    node _GEN_15 = mux(_T_4, io_b_bits_0_0, syst_in_0_0) @[TPU.scala 45:37 47:17 33:24]
    node _GEN_16 = mux(_T_4, io_b_bits_0_1, syst_in_0_1) @[TPU.scala 45:37 47:17 33:24]
    node _GEN_17 = mux(_T_4, io_b_bits_0_2, syst_in_0_2) @[TPU.scala 45:37 47:17 33:24]
    node _GEN_18 = mux(_T_4, io_b_bits_1_0, syst_in_1_0) @[TPU.scala 45:37 47:17 33:24]
    node _GEN_19 = mux(_T_4, io_b_bits_1_1, syst_in_1_1) @[TPU.scala 45:37 47:17 33:24]
    node _GEN_20 = mux(_T_4, io_b_bits_1_2, syst_in_1_2) @[TPU.scala 45:37 47:17 33:24]
    node _GEN_21 = mux(_T_4, io_b_bits_2_0, syst_in_2_0) @[TPU.scala 45:37 47:17 33:24]
    node _GEN_22 = mux(_T_4, io_b_bits_2_1, syst_in_2_1) @[TPU.scala 45:37 47:17 33:24]
    node _GEN_23 = mux(_T_4, io_b_bits_2_2, syst_in_2_2) @[TPU.scala 45:37 47:17 33:24]
    node _GEN_24 = mux(_T_4, UInt<1>("h0"), b_ready) @[TPU.scala 45:37 48:17 23:24]
    node _T_5 = eq(state, UInt<2>("h2")) @[TPU.scala 52:19]
    node _T_6 = eq(cycle, UInt<4>("h9")) @[TPU.scala 53:16]
    node _GEN_25 = mux(_T_6, UInt<2>("h3"), state) @[TPU.scala 53:36 54:13 15:22]
    node _T_7 = gt(cycle, UInt<2>("h3")) @[TPU.scala 57:18]
    node _T_8 = lt(cycle, UInt<3>("h7")) @[TPU.scala 57:39]
    node _T_9 = and(_T_7, _T_8) @[TPU.scala 57:30]
    node _T_10 = sub(cycle, UInt<3>("h4")) @[TPU.scala 58:21]
    node _T_11 = tail(_T_10, 1) @[TPU.scala 58:21]
    node _T_12 = bits(_T_11, 1, 0)
    node _myOut_T_12_0 = systArr.io_out_0 @[TPU.scala 58:{41,41}]
    node _GEN_26 = mux(eq(UInt<1>("h0"), _T_12), _myOut_T_12_0, myOut_0_0) @[TPU.scala 21:22 58:{41,41}]
    node _GEN_27 = mux(eq(UInt<1>("h1"), _T_12), _myOut_T_12_0, myOut_1_0) @[TPU.scala 21:22 58:{41,41}]
    node _GEN_28 = mux(eq(UInt<2>("h2"), _T_12), _myOut_T_12_0, myOut_2_0) @[TPU.scala 21:22 58:{41,41}]
    node _GEN_29 = mux(_T_9, _GEN_26, myOut_0_0) @[TPU.scala 21:22 57:57]
    node _GEN_30 = mux(_T_9, _GEN_27, myOut_1_0) @[TPU.scala 21:22 57:57]
    node _GEN_31 = mux(_T_9, _GEN_28, myOut_2_0) @[TPU.scala 21:22 57:57]
    node _T_13 = gt(cycle, UInt<3>("h4")) @[TPU.scala 57:18]
    node _T_14 = lt(cycle, UInt<4>("h8")) @[TPU.scala 57:39]
    node _T_15 = and(_T_13, _T_14) @[TPU.scala 57:30]
    node _T_16 = sub(cycle, UInt<3>("h5")) @[TPU.scala 58:21]
    node _T_17 = tail(_T_16, 1) @[TPU.scala 58:21]
    node _T_18 = bits(_T_17, 1, 0)
    node _myOut_T_18_1 = systArr.io_out_1 @[TPU.scala 58:{41,41}]
    node _GEN_32 = mux(eq(UInt<1>("h0"), _T_18), _myOut_T_18_1, myOut_0_1) @[TPU.scala 21:22 58:{41,41}]
    node _GEN_33 = mux(eq(UInt<1>("h1"), _T_18), _myOut_T_18_1, myOut_1_1) @[TPU.scala 21:22 58:{41,41}]
    node _GEN_34 = mux(eq(UInt<2>("h2"), _T_18), _myOut_T_18_1, myOut_2_1) @[TPU.scala 21:22 58:{41,41}]
    node _GEN_35 = mux(_T_15, _GEN_32, myOut_0_1) @[TPU.scala 21:22 57:57]
    node _GEN_36 = mux(_T_15, _GEN_33, myOut_1_1) @[TPU.scala 21:22 57:57]
    node _GEN_37 = mux(_T_15, _GEN_34, myOut_2_1) @[TPU.scala 21:22 57:57]
    node _T_19 = gt(cycle, UInt<3>("h5")) @[TPU.scala 57:18]
    node _T_20 = lt(cycle, UInt<4>("h9")) @[TPU.scala 57:39]
    node _T_21 = and(_T_19, _T_20) @[TPU.scala 57:30]
    node _T_22 = sub(cycle, UInt<3>("h6")) @[TPU.scala 58:21]
    node _T_23 = tail(_T_22, 1) @[TPU.scala 58:21]
    node _T_24 = bits(_T_23, 1, 0)
    node _myOut_T_24_2 = systArr.io_out_2 @[TPU.scala 58:{41,41}]
    node _GEN_38 = mux(eq(UInt<1>("h0"), _T_24), _myOut_T_24_2, myOut_0_2) @[TPU.scala 21:22 58:{41,41}]
    node _GEN_39 = mux(eq(UInt<1>("h1"), _T_24), _myOut_T_24_2, myOut_1_2) @[TPU.scala 21:22 58:{41,41}]
    node _GEN_40 = mux(eq(UInt<2>("h2"), _T_24), _myOut_T_24_2, myOut_2_2) @[TPU.scala 21:22 58:{41,41}]
    node _GEN_41 = mux(_T_21, _GEN_38, myOut_0_2) @[TPU.scala 21:22 57:57]
    node _GEN_42 = mux(_T_21, _GEN_39, myOut_1_2) @[TPU.scala 21:22 57:57]
    node _GEN_43 = mux(_T_21, _GEN_40, myOut_2_2) @[TPU.scala 21:22 57:57]
    node _T_25 = bits(reset, 0, 0) @[TPU.scala 61:11]
    node _T_26 = eq(_T_25, UInt<1>("h0")) @[TPU.scala 61:11]
    node _T_27 = bits(reset, 0, 0) @[TPU.scala 63:13]
    node _T_28 = eq(_T_27, UInt<1>("h0")) @[TPU.scala 63:13]
    node _T_29 = bits(reset, 0, 0) @[TPU.scala 63:13]
    node _T_30 = eq(_T_29, UInt<1>("h0")) @[TPU.scala 63:13]
    node _T_31 = bits(reset, 0, 0) @[TPU.scala 63:13]
    node _T_32 = eq(_T_31, UInt<1>("h0")) @[TPU.scala 63:13]
    node _T_33 = eq(state, UInt<2>("h3")) @[TPU.scala 66:19]
    node _T_34 = bits(reset, 0, 0) @[TPU.scala 67:11]
    node _T_35 = eq(_T_34, UInt<1>("h0")) @[TPU.scala 67:11]
    node _GEN_44 = mux(_T_33, UInt<2>("h1"), state) @[TPU.scala 66:29 68:11 15:22]
    node _GEN_45 = mux(_T_33, UInt<1>("h1"), b_ready) @[TPU.scala 66:29 69:13 23:24]
    node _WIRE_0_0 = asSInt(UInt<8>("h0")) @[TPU.scala 70:{36,36}]
    node _GEN_46 = mux(_T_33, _WIRE_0_0, myOut_0_0) @[TPU.scala 66:29 70:11 21:22]
    node _WIRE_0_1 = asSInt(UInt<8>("h0")) @[TPU.scala 70:{36,36}]
    node _GEN_47 = mux(_T_33, _WIRE_0_1, myOut_0_1) @[TPU.scala 66:29 70:11 21:22]
    node _WIRE_0_2 = asSInt(UInt<8>("h0")) @[TPU.scala 70:{36,36}]
    node _GEN_48 = mux(_T_33, _WIRE_0_2, myOut_0_2) @[TPU.scala 66:29 70:11 21:22]
    node _WIRE_1_0 = asSInt(UInt<8>("h0")) @[TPU.scala 70:{36,36}]
    node _GEN_49 = mux(_T_33, _WIRE_1_0, myOut_1_0) @[TPU.scala 66:29 70:11 21:22]
    node _WIRE_1_1 = asSInt(UInt<8>("h0")) @[TPU.scala 70:{36,36}]
    node _GEN_50 = mux(_T_33, _WIRE_1_1, myOut_1_1) @[TPU.scala 66:29 70:11 21:22]
    node _WIRE_1_2 = asSInt(UInt<8>("h0")) @[TPU.scala 70:{36,36}]
    node _GEN_51 = mux(_T_33, _WIRE_1_2, myOut_1_2) @[TPU.scala 66:29 70:11 21:22]
    node _WIRE_2_0 = asSInt(UInt<8>("h0")) @[TPU.scala 70:{36,36}]
    node _GEN_52 = mux(_T_33, _WIRE_2_0, myOut_2_0) @[TPU.scala 66:29 70:11 21:22]
    node _WIRE_2_1 = asSInt(UInt<8>("h0")) @[TPU.scala 70:{36,36}]
    node _GEN_53 = mux(_T_33, _WIRE_2_1, myOut_2_1) @[TPU.scala 66:29 70:11 21:22]
    node _WIRE_2_2 = asSInt(UInt<8>("h0")) @[TPU.scala 70:{36,36}]
    node _GEN_54 = mux(_T_33, _WIRE_2_2, myOut_2_2) @[TPU.scala 66:29 70:11 21:22]
    node _GEN_55 = mux(_T_5, _GEN_25, _GEN_44) @[TPU.scala 52:32]
    node _GEN_56 = mux(_T_5, _GEN_29, _GEN_46) @[TPU.scala 52:32]
    node _GEN_57 = mux(_T_5, _GEN_30, _GEN_49) @[TPU.scala 52:32]
    node _GEN_58 = mux(_T_5, _GEN_31, _GEN_52) @[TPU.scala 52:32]
    node _GEN_59 = mux(_T_5, _GEN_35, _GEN_47) @[TPU.scala 52:32]
    node _GEN_60 = mux(_T_5, _GEN_36, _GEN_50) @[TPU.scala 52:32]
    node _GEN_61 = mux(_T_5, _GEN_37, _GEN_53) @[TPU.scala 52:32]
    node _GEN_62 = mux(_T_5, _GEN_41, _GEN_48) @[TPU.scala 52:32]
    node _GEN_63 = mux(_T_5, _GEN_42, _GEN_51) @[TPU.scala 52:32]
    node _GEN_64 = mux(_T_5, _GEN_43, _GEN_54) @[TPU.scala 52:32]
    node _GEN_65 = mux(_T_5, b_ready, _GEN_45) @[TPU.scala 23:24 52:32]
    node _GEN_66 = mux(_T_3, _GEN_14, _GEN_55) @[TPU.scala 44:28]
    node _GEN_67 = mux(_T_3, _GEN_15, syst_in_0_0) @[TPU.scala 33:24 44:28]
    node _GEN_68 = mux(_T_3, _GEN_16, syst_in_0_1) @[TPU.scala 33:24 44:28]
    node _GEN_69 = mux(_T_3, _GEN_17, syst_in_0_2) @[TPU.scala 33:24 44:28]
    node _GEN_70 = mux(_T_3, _GEN_18, syst_in_1_0) @[TPU.scala 33:24 44:28]
    node _GEN_71 = mux(_T_3, _GEN_19, syst_in_1_1) @[TPU.scala 33:24 44:28]
    node _GEN_72 = mux(_T_3, _GEN_20, syst_in_1_2) @[TPU.scala 33:24 44:28]
    node _GEN_73 = mux(_T_3, _GEN_21, syst_in_2_0) @[TPU.scala 33:24 44:28]
    node _GEN_74 = mux(_T_3, _GEN_22, syst_in_2_1) @[TPU.scala 33:24 44:28]
    node _GEN_75 = mux(_T_3, _GEN_23, syst_in_2_2) @[TPU.scala 33:24 44:28]
    node _GEN_76 = mux(_T_3, _GEN_24, _GEN_65) @[TPU.scala 44:28]
    node _GEN_77 = mux(_T_3, UInt<1>("h0"), _GEN_1) @[TPU.scala 44:28 50:13]
    node _GEN_78 = mux(_T_3, myOut_0_0, _GEN_56) @[TPU.scala 21:22 44:28]
    node _GEN_79 = mux(_T_3, myOut_1_0, _GEN_57) @[TPU.scala 21:22 44:28]
    node _GEN_80 = mux(_T_3, myOut_2_0, _GEN_58) @[TPU.scala 21:22 44:28]
    node _GEN_81 = mux(_T_3, myOut_0_1, _GEN_59) @[TPU.scala 21:22 44:28]
    node _GEN_82 = mux(_T_3, myOut_1_1, _GEN_60) @[TPU.scala 21:22 44:28]
    node _GEN_83 = mux(_T_3, myOut_2_1, _GEN_61) @[TPU.scala 21:22 44:28]
    node _GEN_84 = mux(_T_3, myOut_0_2, _GEN_62) @[TPU.scala 21:22 44:28]
    node _GEN_85 = mux(_T_3, myOut_1_2, _GEN_63) @[TPU.scala 21:22 44:28]
    node _GEN_86 = mux(_T_3, myOut_2_2, _GEN_64) @[TPU.scala 21:22 44:28]
    node _GEN_87 = mux(_T_1, _GEN_3, _GEN_66) @[TPU.scala 37:23]
    node _GEN_88 = mux(_T_1, _GEN_4, act_in_0_0) @[TPU.scala 32:23 37:23]
    node _GEN_89 = mux(_T_1, _GEN_5, act_in_0_1) @[TPU.scala 32:23 37:23]
    node _GEN_90 = mux(_T_1, _GEN_6, act_in_0_2) @[TPU.scala 32:23 37:23]
    node _GEN_91 = mux(_T_1, _GEN_7, act_in_1_0) @[TPU.scala 32:23 37:23]
    node _GEN_92 = mux(_T_1, _GEN_8, act_in_1_1) @[TPU.scala 32:23 37:23]
    node _GEN_93 = mux(_T_1, _GEN_9, act_in_1_2) @[TPU.scala 32:23 37:23]
    node _GEN_94 = mux(_T_1, _GEN_10, act_in_2_0) @[TPU.scala 32:23 37:23]
    node _GEN_95 = mux(_T_1, _GEN_11, act_in_2_1) @[TPU.scala 32:23 37:23]
    node _GEN_96 = mux(_T_1, _GEN_12, act_in_2_2) @[TPU.scala 32:23 37:23]
    node _GEN_97 = mux(_T_1, _GEN_13, a_ready) @[TPU.scala 37:23 22:24]
    node _GEN_98 = mux(_T_1, syst_in_0_0, _GEN_67) @[TPU.scala 37:23 33:24]
    node _GEN_99 = mux(_T_1, syst_in_0_1, _GEN_68) @[TPU.scala 37:23 33:24]
    node _GEN_100 = mux(_T_1, syst_in_0_2, _GEN_69) @[TPU.scala 37:23 33:24]
    node _GEN_101 = mux(_T_1, syst_in_1_0, _GEN_70) @[TPU.scala 37:23 33:24]
    node _GEN_102 = mux(_T_1, syst_in_1_1, _GEN_71) @[TPU.scala 37:23 33:24]
    node _GEN_103 = mux(_T_1, syst_in_1_2, _GEN_72) @[TPU.scala 37:23 33:24]
    node _GEN_104 = mux(_T_1, syst_in_2_0, _GEN_73) @[TPU.scala 37:23 33:24]
    node _GEN_105 = mux(_T_1, syst_in_2_1, _GEN_74) @[TPU.scala 37:23 33:24]
    node _GEN_106 = mux(_T_1, syst_in_2_2, _GEN_75) @[TPU.scala 37:23 33:24]
    node _GEN_107 = mux(_T_1, b_ready, _GEN_76) @[TPU.scala 37:23 23:24]
    node _GEN_108 = mux(_T_1, _GEN_1, _GEN_77) @[TPU.scala 37:23]
    node _GEN_109 = mux(_T_1, myOut_0_0, _GEN_78) @[TPU.scala 21:22 37:23]
    node _GEN_110 = mux(_T_1, myOut_1_0, _GEN_79) @[TPU.scala 21:22 37:23]
    node _GEN_111 = mux(_T_1, myOut_2_0, _GEN_80) @[TPU.scala 21:22 37:23]
    node _GEN_112 = mux(_T_1, myOut_0_1, _GEN_81) @[TPU.scala 21:22 37:23]
    node _GEN_113 = mux(_T_1, myOut_1_1, _GEN_82) @[TPU.scala 21:22 37:23]
    node _GEN_114 = mux(_T_1, myOut_2_1, _GEN_83) @[TPU.scala 21:22 37:23]
    node _GEN_115 = mux(_T_1, myOut_0_2, _GEN_84) @[TPU.scala 21:22 37:23]
    node _GEN_116 = mux(_T_1, myOut_1_2, _GEN_85) @[TPU.scala 21:22 37:23]
    node _GEN_117 = mux(_T_1, myOut_2_2, _GEN_86) @[TPU.scala 21:22 37:23]
    node wrap = _GEN_2
    node _myOut_WIRE_0_0 = asSInt(UInt<8>("h0")) @[TPU.scala 21:{45,45}]
    node _myOut_WIRE_0_1 = asSInt(UInt<8>("h0")) @[TPU.scala 21:{45,45}]
    node _myOut_WIRE_0_2 = asSInt(UInt<8>("h0")) @[TPU.scala 21:{45,45}]
    node _myOut_WIRE_1_0 = asSInt(UInt<8>("h0")) @[TPU.scala 21:{45,45}]
    node _myOut_WIRE_1_1 = asSInt(UInt<8>("h0")) @[TPU.scala 21:{45,45}]
    node _myOut_WIRE_1_2 = asSInt(UInt<8>("h0")) @[TPU.scala 21:{45,45}]
    node _myOut_WIRE_2_0 = asSInt(UInt<8>("h0")) @[TPU.scala 21:{45,45}]
    node _myOut_WIRE_2_1 = asSInt(UInt<8>("h0")) @[TPU.scala 21:{45,45}]
    node _myOut_WIRE_2_2 = asSInt(UInt<8>("h0")) @[TPU.scala 21:{45,45}]
    node _act_in_WIRE_0_0 = asSInt(UInt<8>("h0")) @[TPU.scala 32:{46,46}]
    node _act_in_WIRE_0_1 = asSInt(UInt<8>("h0")) @[TPU.scala 32:{46,46}]
    node _act_in_WIRE_0_2 = asSInt(UInt<8>("h0")) @[TPU.scala 32:{46,46}]
    node _act_in_WIRE_1_0 = asSInt(UInt<8>("h0")) @[TPU.scala 32:{46,46}]
    node _act_in_WIRE_1_1 = asSInt(UInt<8>("h0")) @[TPU.scala 32:{46,46}]
    node _act_in_WIRE_1_2 = asSInt(UInt<8>("h0")) @[TPU.scala 32:{46,46}]
    node _act_in_WIRE_2_0 = asSInt(UInt<8>("h0")) @[TPU.scala 32:{46,46}]
    node _act_in_WIRE_2_1 = asSInt(UInt<8>("h0")) @[TPU.scala 32:{46,46}]
    node _act_in_WIRE_2_2 = asSInt(UInt<8>("h0")) @[TPU.scala 32:{46,46}]
    node _syst_in_WIRE_0_0 = asSInt(UInt<8>("h0")) @[TPU.scala 33:{47,47}]
    node _syst_in_WIRE_0_1 = asSInt(UInt<8>("h0")) @[TPU.scala 33:{47,47}]
    node _syst_in_WIRE_0_2 = asSInt(UInt<8>("h0")) @[TPU.scala 33:{47,47}]
    node _syst_in_WIRE_1_0 = asSInt(UInt<8>("h0")) @[TPU.scala 33:{47,47}]
    node _syst_in_WIRE_1_1 = asSInt(UInt<8>("h0")) @[TPU.scala 33:{47,47}]
    node _syst_in_WIRE_1_2 = asSInt(UInt<8>("h0")) @[TPU.scala 33:{47,47}]
    node _syst_in_WIRE_2_0 = asSInt(UInt<8>("h0")) @[TPU.scala 33:{47,47}]
    node _syst_in_WIRE_2_1 = asSInt(UInt<8>("h0")) @[TPU.scala 33:{47,47}]
    node _syst_in_WIRE_2_2 = asSInt(UInt<8>("h0")) @[TPU.scala 33:{47,47}]
    io_a_ready <= a_ready @[TPU.scala 25:14]
    io_b_ready <= b_ready @[TPU.scala 26:14]
    io_out_0_0 <= myOut_0_0 @[TPU.scala 27:10]
    io_out_0_1 <= myOut_0_1 @[TPU.scala 27:10]
    io_out_0_2 <= myOut_0_2 @[TPU.scala 27:10]
    io_out_1_0 <= myOut_1_0 @[TPU.scala 27:10]
    io_out_1_1 <= myOut_1_1 @[TPU.scala 27:10]
    io_out_1_2 <= myOut_1_2 @[TPU.scala 27:10]
    io_out_2_0 <= myOut_2_0 @[TPU.scala 27:10]
    io_out_2_1 <= myOut_2_1 @[TPU.scala 27:10]
    io_out_2_2 <= myOut_2_2 @[TPU.scala 27:10]
    state <= mux(reset, UInt<2>("h0"), _GEN_87) @[TPU.scala 15:{22,22}]
    cycle <= mux(reset, UInt<4>("h0"), _GEN_108) @[Counter.scala 61:{40,40}]
    actReg.clock <= clock
    actReg.reset <= reset
    actReg.io_index <= _actReg_io_index_T_1 @[TPU.scala 30:19]
    actReg.io_a_0_0 <= act_in_0_0 @[TPU.scala 34:15]
    actReg.io_a_0_1 <= act_in_0_1 @[TPU.scala 34:15]
    actReg.io_a_0_2 <= act_in_0_2 @[TPU.scala 34:15]
    actReg.io_a_1_0 <= act_in_1_0 @[TPU.scala 34:15]
    actReg.io_a_1_1 <= act_in_1_1 @[TPU.scala 34:15]
    actReg.io_a_1_2 <= act_in_1_2 @[TPU.scala 34:15]
    actReg.io_a_2_0 <= act_in_2_0 @[TPU.scala 34:15]
    actReg.io_a_2_1 <= act_in_2_1 @[TPU.scala 34:15]
    actReg.io_a_2_2 <= act_in_2_2 @[TPU.scala 34:15]
    systArr.clock <= clock
    systArr.reset <= reset
    systArr.io_a_in_0 <= REG_0 @[TPU.scala 31:19]
    systArr.io_a_in_1 <= REG_1 @[TPU.scala 31:19]
    systArr.io_a_in_2 <= REG_2 @[TPU.scala 31:19]
    systArr.io_b_in_0_0 <= syst_in_0_0 @[TPU.scala 35:19]
    systArr.io_b_in_0_1 <= syst_in_0_1 @[TPU.scala 35:19]
    systArr.io_b_in_0_2 <= syst_in_0_2 @[TPU.scala 35:19]
    systArr.io_b_in_1_0 <= syst_in_1_0 @[TPU.scala 35:19]
    systArr.io_b_in_1_1 <= syst_in_1_1 @[TPU.scala 35:19]
    systArr.io_b_in_1_2 <= syst_in_1_2 @[TPU.scala 35:19]
    systArr.io_b_in_2_0 <= syst_in_2_0 @[TPU.scala 35:19]
    systArr.io_b_in_2_1 <= syst_in_2_1 @[TPU.scala 35:19]
    systArr.io_b_in_2_2 <= syst_in_2_2 @[TPU.scala 35:19]
    myOut_0_0 <= mux(reset, _myOut_WIRE_0_0, _GEN_109) @[TPU.scala 21:{22,22}]
    myOut_0_1 <= mux(reset, _myOut_WIRE_0_1, _GEN_112) @[TPU.scala 21:{22,22}]
    myOut_0_2 <= mux(reset, _myOut_WIRE_0_2, _GEN_115) @[TPU.scala 21:{22,22}]
    myOut_1_0 <= mux(reset, _myOut_WIRE_1_0, _GEN_110) @[TPU.scala 21:{22,22}]
    myOut_1_1 <= mux(reset, _myOut_WIRE_1_1, _GEN_113) @[TPU.scala 21:{22,22}]
    myOut_1_2 <= mux(reset, _myOut_WIRE_1_2, _GEN_116) @[TPU.scala 21:{22,22}]
    myOut_2_0 <= mux(reset, _myOut_WIRE_2_0, _GEN_111) @[TPU.scala 21:{22,22}]
    myOut_2_1 <= mux(reset, _myOut_WIRE_2_1, _GEN_114) @[TPU.scala 21:{22,22}]
    myOut_2_2 <= mux(reset, _myOut_WIRE_2_2, _GEN_117) @[TPU.scala 21:{22,22}]
    a_ready <= mux(reset, UInt<1>("h1"), _GEN_97) @[TPU.scala 22:{24,24}]
    b_ready <= mux(reset, UInt<1>("h1"), _GEN_107) @[TPU.scala 23:{24,24}]
    REG_0 <= actReg.io_a_out_0 @[TPU.scala 31:29]
    REG_1 <= actReg.io_a_out_1 @[TPU.scala 31:29]
    REG_2 <= actReg.io_a_out_2 @[TPU.scala 31:29]
    act_in_0_0 <= mux(reset, _act_in_WIRE_0_0, _GEN_88) @[TPU.scala 32:{23,23}]
    act_in_0_1 <= mux(reset, _act_in_WIRE_0_1, _GEN_89) @[TPU.scala 32:{23,23}]
    act_in_0_2 <= mux(reset, _act_in_WIRE_0_2, _GEN_90) @[TPU.scala 32:{23,23}]
    act_in_1_0 <= mux(reset, _act_in_WIRE_1_0, _GEN_91) @[TPU.scala 32:{23,23}]
    act_in_1_1 <= mux(reset, _act_in_WIRE_1_1, _GEN_92) @[TPU.scala 32:{23,23}]
    act_in_1_2 <= mux(reset, _act_in_WIRE_1_2, _GEN_93) @[TPU.scala 32:{23,23}]
    act_in_2_0 <= mux(reset, _act_in_WIRE_2_0, _GEN_94) @[TPU.scala 32:{23,23}]
    act_in_2_1 <= mux(reset, _act_in_WIRE_2_1, _GEN_95) @[TPU.scala 32:{23,23}]
    act_in_2_2 <= mux(reset, _act_in_WIRE_2_2, _GEN_96) @[TPU.scala 32:{23,23}]
    syst_in_0_0 <= mux(reset, _syst_in_WIRE_0_0, _GEN_98) @[TPU.scala 33:{24,24}]
    syst_in_0_1 <= mux(reset, _syst_in_WIRE_0_1, _GEN_99) @[TPU.scala 33:{24,24}]
    syst_in_0_2 <= mux(reset, _syst_in_WIRE_0_2, _GEN_100) @[TPU.scala 33:{24,24}]
    syst_in_1_0 <= mux(reset, _syst_in_WIRE_1_0, _GEN_101) @[TPU.scala 33:{24,24}]
    syst_in_1_1 <= mux(reset, _syst_in_WIRE_1_1, _GEN_102) @[TPU.scala 33:{24,24}]
    syst_in_1_2 <= mux(reset, _syst_in_WIRE_1_2, _GEN_103) @[TPU.scala 33:{24,24}]
    syst_in_2_0 <= mux(reset, _syst_in_WIRE_2_0, _GEN_104) @[TPU.scala 33:{24,24}]
    syst_in_2_1 <= mux(reset, _syst_in_WIRE_2_1, _GEN_105) @[TPU.scala 33:{24,24}]
    syst_in_2_2 <= mux(reset, _syst_in_WIRE_2_2, _GEN_106) @[TPU.scala 33:{24,24}]
    printf(clock, and(and(and(and(and(UInt<1>("h1"), eq(_T_1, UInt<1>("h0"))), eq(_T_3, UInt<1>("h0"))), _T_5), _T_26), UInt<1>("h1")), "MY TPU OUT: \n") : printf @[TPU.scala 61:11]
    printf(clock, and(and(and(and(and(UInt<1>("h1"), eq(_T_1, UInt<1>("h0"))), eq(_T_3, UInt<1>("h0"))), _T_5), _T_28), UInt<1>("h1")), "Vec(%d, %d, %d)\n", myOut_0_0, myOut_0_1, myOut_0_2) : printf_1 @[TPU.scala 63:13]
    printf(clock, and(and(and(and(and(UInt<1>("h1"), eq(_T_1, UInt<1>("h0"))), eq(_T_3, UInt<1>("h0"))), _T_5), _T_30), UInt<1>("h1")), "Vec(%d, %d, %d)\n", myOut_1_0, myOut_1_1, myOut_1_2) : printf_2 @[TPU.scala 63:13]
    printf(clock, and(and(and(and(and(UInt<1>("h1"), eq(_T_1, UInt<1>("h0"))), eq(_T_3, UInt<1>("h0"))), _T_5), _T_32), UInt<1>("h1")), "Vec(%d, %d, %d)\n", myOut_2_0, myOut_2_1, myOut_2_2) : printf_3 @[TPU.scala 63:13]
    printf(clock, and(and(and(and(and(and(UInt<1>("h1"), eq(_T_1, UInt<1>("h0"))), eq(_T_3, UInt<1>("h0"))), eq(_T_5, UInt<1>("h0"))), _T_33), _T_35), UInt<1>("h1")), "FINISH\n") : printf_4 @[TPU.scala 67:11]
