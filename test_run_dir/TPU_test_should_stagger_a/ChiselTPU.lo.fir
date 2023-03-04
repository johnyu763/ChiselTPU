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

    node _T = lt(io_index, UInt<3>("h5")) @[TPU.scala 74:19]
    node _T_1 = asSInt(io_index) @[TPU.scala 76:35]
    node _T_2 = sub(asSInt(UInt<3>("h2")), _T_1) @[TPU.scala 76:25]
    node _T_3 = tail(_T_2, 1) @[TPU.scala 76:25]
    node _T_4 = asSInt(_T_3) @[TPU.scala 76:25]
    node _T_5 = leq(_T_4, asSInt(UInt<1>("h0"))) @[TPU.scala 76:42]
    node _T_6 = asSInt(io_index) @[TPU.scala 76:77]
    node _T_7 = sub(asSInt(UInt<4>("h5")), _T_6) @[TPU.scala 76:67]
    node _T_8 = tail(_T_7, 1) @[TPU.scala 76:67]
    node _T_9 = asSInt(_T_8) @[TPU.scala 76:67]
    node _T_10 = gt(_T_9, asSInt(UInt<1>("h0"))) @[TPU.scala 76:84]
    node _T_11 = and(_T_5, _T_10) @[TPU.scala 76:49]
    node _io_a_out_0_T = sub(UInt<3>("h4"), io_index) @[TPU.scala 77:55]
    node _io_a_out_0_T_1 = tail(_io_a_out_0_T, 1) @[TPU.scala 77:55]
    node _io_a_out_0_T_2 = bits(_io_a_out_0_T_1, 1, 0)
    node _GEN_0 = validif(eq(UInt<1>("h0"), _io_a_out_0_T_2), io_a_0_0) @[TPU.scala 77:{25,25}]
    node _GEN_1 = mux(eq(UInt<1>("h1"), _io_a_out_0_T_2), io_a_1_0, _GEN_0) @[TPU.scala 77:{25,25}]
    node _GEN_2 = mux(eq(UInt<2>("h2"), _io_a_out_0_T_2), io_a_2_0, _GEN_1) @[TPU.scala 77:{25,25}]
    node _io_a_io_a_out_0_T_2_0 = _GEN_2 @[TPU.scala 77:25]
    node _GEN_3 = mux(_T_11, _io_a_io_a_out_0_T_2_0, asSInt(UInt<1>("h0"))) @[TPU.scala 76:90 77:25 80:25]
    node _T_12 = asSInt(io_index) @[TPU.scala 76:35]
    node _T_13 = sub(asSInt(UInt<2>("h1")), _T_12) @[TPU.scala 76:25]
    node _T_14 = tail(_T_13, 1) @[TPU.scala 76:25]
    node _T_15 = asSInt(_T_14) @[TPU.scala 76:25]
    node _T_16 = leq(_T_15, asSInt(UInt<1>("h0"))) @[TPU.scala 76:42]
    node _T_17 = asSInt(io_index) @[TPU.scala 76:77]
    node _T_18 = sub(asSInt(UInt<4>("h4")), _T_17) @[TPU.scala 76:67]
    node _T_19 = tail(_T_18, 1) @[TPU.scala 76:67]
    node _T_20 = asSInt(_T_19) @[TPU.scala 76:67]
    node _T_21 = gt(_T_20, asSInt(UInt<1>("h0"))) @[TPU.scala 76:84]
    node _T_22 = and(_T_16, _T_21) @[TPU.scala 76:49]
    node _io_a_out_1_T = sub(UInt<2>("h3"), io_index) @[TPU.scala 77:55]
    node _io_a_out_1_T_1 = tail(_io_a_out_1_T, 1) @[TPU.scala 77:55]
    node _io_a_out_1_T_2 = bits(_io_a_out_1_T_1, 1, 0)
    node _GEN_4 = validif(eq(UInt<1>("h0"), _io_a_out_1_T_2), io_a_0_1) @[TPU.scala 77:{25,25}]
    node _GEN_5 = mux(eq(UInt<1>("h1"), _io_a_out_1_T_2), io_a_1_1, _GEN_4) @[TPU.scala 77:{25,25}]
    node _GEN_6 = mux(eq(UInt<2>("h2"), _io_a_out_1_T_2), io_a_2_1, _GEN_5) @[TPU.scala 77:{25,25}]
    node _io_a_io_a_out_1_T_2_1 = _GEN_6 @[TPU.scala 77:25]
    node _GEN_7 = mux(_T_22, _io_a_io_a_out_1_T_2_1, asSInt(UInt<1>("h0"))) @[TPU.scala 76:90 77:25 80:25]
    node _T_23 = asSInt(io_index) @[TPU.scala 76:35]
    node _T_24 = sub(asSInt(UInt<1>("h0")), _T_23) @[TPU.scala 76:25]
    node _T_25 = tail(_T_24, 1) @[TPU.scala 76:25]
    node _T_26 = asSInt(_T_25) @[TPU.scala 76:25]
    node _T_27 = leq(_T_26, asSInt(UInt<1>("h0"))) @[TPU.scala 76:42]
    node _T_28 = asSInt(io_index) @[TPU.scala 76:77]
    node _T_29 = sub(asSInt(UInt<3>("h3")), _T_28) @[TPU.scala 76:67]
    node _T_30 = tail(_T_29, 1) @[TPU.scala 76:67]
    node _T_31 = asSInt(_T_30) @[TPU.scala 76:67]
    node _T_32 = gt(_T_31, asSInt(UInt<1>("h0"))) @[TPU.scala 76:84]
    node _T_33 = and(_T_27, _T_32) @[TPU.scala 76:49]
    node _io_a_out_2_T = sub(UInt<2>("h2"), io_index) @[TPU.scala 77:55]
    node _io_a_out_2_T_1 = tail(_io_a_out_2_T, 1) @[TPU.scala 77:55]
    node _io_a_out_2_T_2 = bits(_io_a_out_2_T_1, 1, 0)
    node _GEN_8 = validif(eq(UInt<1>("h0"), _io_a_out_2_T_2), io_a_0_2) @[TPU.scala 77:{25,25}]
    node _GEN_9 = mux(eq(UInt<1>("h1"), _io_a_out_2_T_2), io_a_1_2, _GEN_8) @[TPU.scala 77:{25,25}]
    node _GEN_10 = mux(eq(UInt<2>("h2"), _io_a_out_2_T_2), io_a_2_2, _GEN_9) @[TPU.scala 77:{25,25}]
    node _io_a_io_a_out_2_T_2_2 = _GEN_10 @[TPU.scala 77:25]
    node _GEN_11 = mux(_T_33, _io_a_io_a_out_2_T_2_2, asSInt(UInt<1>("h0"))) @[TPU.scala 76:90 77:25 80:25]
    node _GEN_12 = mux(_T, _GEN_3, asSInt(UInt<1>("h0"))) @[TPU.scala 74:35 86:23]
    node _GEN_13 = mux(_T, _GEN_7, asSInt(UInt<1>("h0"))) @[TPU.scala 74:35 86:23]
    node _GEN_14 = mux(_T, _GEN_11, asSInt(UInt<1>("h0"))) @[TPU.scala 74:35 86:23]
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
      reset => (UInt<1>("h0"), a_reg_0_0) @[TPU.scala 100:20]
    reg a_reg_0_1 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), a_reg_0_1) @[TPU.scala 100:20]
    reg a_reg_0_2 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), a_reg_0_2) @[TPU.scala 100:20]
    reg a_reg_1_0 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), a_reg_1_0) @[TPU.scala 100:20]
    reg a_reg_1_1 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), a_reg_1_1) @[TPU.scala 100:20]
    reg a_reg_1_2 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), a_reg_1_2) @[TPU.scala 100:20]
    reg a_reg_2_0 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), a_reg_2_0) @[TPU.scala 100:20]
    reg a_reg_2_1 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), a_reg_2_1) @[TPU.scala 100:20]
    reg a_reg_2_2 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), a_reg_2_2) @[TPU.scala 100:20]
    reg b_reg_0_0 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), b_reg_0_0) @[TPU.scala 101:20]
    reg b_reg_0_1 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), b_reg_0_1) @[TPU.scala 101:20]
    reg b_reg_0_2 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), b_reg_0_2) @[TPU.scala 101:20]
    reg b_reg_1_0 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), b_reg_1_0) @[TPU.scala 101:20]
    reg b_reg_1_1 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), b_reg_1_1) @[TPU.scala 101:20]
    reg b_reg_1_2 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), b_reg_1_2) @[TPU.scala 101:20]
    reg b_reg_2_0 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), b_reg_2_0) @[TPU.scala 101:20]
    reg b_reg_2_1 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), b_reg_2_1) @[TPU.scala 101:20]
    reg b_reg_2_2 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), b_reg_2_2) @[TPU.scala 101:20]
    reg cms_reg_0_0 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), cms_reg_0_0) @[TPU.scala 102:22]
    reg cms_reg_0_1 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), cms_reg_0_1) @[TPU.scala 102:22]
    reg cms_reg_0_2 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), cms_reg_0_2) @[TPU.scala 102:22]
    reg cms_reg_1_0 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), cms_reg_1_0) @[TPU.scala 102:22]
    reg cms_reg_1_1 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), cms_reg_1_1) @[TPU.scala 102:22]
    reg cms_reg_1_2 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), cms_reg_1_2) @[TPU.scala 102:22]
    reg cms_reg_2_0 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), cms_reg_2_0) @[TPU.scala 102:22]
    reg cms_reg_2_1 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), cms_reg_2_1) @[TPU.scala 102:22]
    reg cms_reg_2_2 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), cms_reg_2_2) @[TPU.scala 102:22]
    node _cmp_input_T = mul(io_a_in_0, b_reg_0_0) @[TPU.scala 114:48]
    node _cmp_input_T_1 = mul(a_reg_0_0, b_reg_0_1) @[TPU.scala 117:54]
    node _cmp_input_T_2 = mul(a_reg_0_1, b_reg_0_2) @[TPU.scala 117:54]
    node _cmp_input_T_3 = mul(io_a_in_1, b_reg_1_0) @[TPU.scala 122:49]
    node _cmp_input_T_4 = add(_cmp_input_T_3, cms_reg_0_0) @[TPU.scala 122:70]
    node _cmp_input_T_5 = tail(_cmp_input_T_4, 1) @[TPU.scala 122:70]
    node _cmp_input_T_6 = asSInt(_cmp_input_T_5) @[TPU.scala 122:70]
    node _cmp_input_T_7 = mul(a_reg_1_0, b_reg_1_1) @[TPU.scala 125:55]
    node _cmp_input_T_8 = add(_cmp_input_T_7, cms_reg_0_1) @[TPU.scala 125:76]
    node _cmp_input_T_9 = tail(_cmp_input_T_8, 1) @[TPU.scala 125:76]
    node _cmp_input_T_10 = asSInt(_cmp_input_T_9) @[TPU.scala 125:76]
    node _cmp_input_T_11 = mul(a_reg_1_1, b_reg_1_2) @[TPU.scala 125:55]
    node _cmp_input_T_12 = add(_cmp_input_T_11, cms_reg_0_2) @[TPU.scala 125:76]
    node _cmp_input_T_13 = tail(_cmp_input_T_12, 1) @[TPU.scala 125:76]
    node _cmp_input_T_14 = asSInt(_cmp_input_T_13) @[TPU.scala 125:76]
    node _cmp_input_T_15 = mul(io_a_in_2, b_reg_2_0) @[TPU.scala 122:49]
    node _cmp_input_T_16 = add(_cmp_input_T_15, cms_reg_1_0) @[TPU.scala 122:70]
    node _cmp_input_T_17 = tail(_cmp_input_T_16, 1) @[TPU.scala 122:70]
    node _cmp_input_T_18 = asSInt(_cmp_input_T_17) @[TPU.scala 122:70]
    node _cmp_input_T_19 = mul(a_reg_2_0, b_reg_2_1) @[TPU.scala 125:55]
    node _cmp_input_T_20 = add(_cmp_input_T_19, cms_reg_1_1) @[TPU.scala 125:76]
    node _cmp_input_T_21 = tail(_cmp_input_T_20, 1) @[TPU.scala 125:76]
    node _cmp_input_T_22 = asSInt(_cmp_input_T_21) @[TPU.scala 125:76]
    node _cmp_input_T_23 = mul(a_reg_2_1, b_reg_2_2) @[TPU.scala 125:55]
    node _cmp_input_T_24 = add(_cmp_input_T_23, cms_reg_1_2) @[TPU.scala 125:76]
    node _cmp_input_T_25 = tail(_cmp_input_T_24, 1) @[TPU.scala 125:76]
    node _cmp_input_T_26 = asSInt(_cmp_input_T_25) @[TPU.scala 125:76]
    node cmp_input = asSInt(bits(_cmp_input_T, 7, 0)) @[TPU.scala 111:33 114:31]
    node cmp_input_1 = asSInt(bits(_cmp_input_T_1, 7, 0)) @[TPU.scala 111:33 117:31]
    node cmp_input_2 = asSInt(bits(_cmp_input_T_2, 7, 0)) @[TPU.scala 111:33 117:31]
    node cmp_input_3 = asSInt(bits(_cmp_input_T_6, 7, 0)) @[TPU.scala 111:33 122:31]
    node cmp_input_4 = asSInt(bits(_cmp_input_T_10, 7, 0)) @[TPU.scala 111:33 125:31]
    node cmp_input_5 = asSInt(bits(_cmp_input_T_14, 7, 0)) @[TPU.scala 111:33 125:31]
    node cmp_input_6 = asSInt(bits(_cmp_input_T_18, 7, 0)) @[TPU.scala 111:33 122:31]
    node cmp_input_7 = asSInt(bits(_cmp_input_T_22, 7, 0)) @[TPU.scala 111:33 125:31]
    node cmp_input_8 = asSInt(bits(_cmp_input_T_26, 7, 0)) @[TPU.scala 111:33 125:31]
    io_out_0 <= cms_reg_2_0 @[TPU.scala 107:19]
    io_out_1 <= cms_reg_2_1 @[TPU.scala 107:19]
    io_out_2 <= cms_reg_2_2 @[TPU.scala 107:19]
    io_cmp_debug_0_0 <= cms_reg_0_0 @[TPU.scala 103:18]
    io_cmp_debug_0_1 <= cms_reg_0_1 @[TPU.scala 103:18]
    io_cmp_debug_0_2 <= cms_reg_0_2 @[TPU.scala 103:18]
    io_cmp_debug_1_0 <= cms_reg_1_0 @[TPU.scala 103:18]
    io_cmp_debug_1_1 <= cms_reg_1_1 @[TPU.scala 103:18]
    io_cmp_debug_1_2 <= cms_reg_1_2 @[TPU.scala 103:18]
    io_cmp_debug_2_0 <= cms_reg_2_0 @[TPU.scala 103:18]
    io_cmp_debug_2_1 <= cms_reg_2_1 @[TPU.scala 103:18]
    io_cmp_debug_2_2 <= cms_reg_2_2 @[TPU.scala 103:18]
    io_b_reg_debug_0_0 <= b_reg_0_0 @[TPU.scala 104:20]
    io_b_reg_debug_0_1 <= b_reg_0_1 @[TPU.scala 104:20]
    io_b_reg_debug_0_2 <= b_reg_0_2 @[TPU.scala 104:20]
    io_b_reg_debug_1_0 <= b_reg_1_0 @[TPU.scala 104:20]
    io_b_reg_debug_1_1 <= b_reg_1_1 @[TPU.scala 104:20]
    io_b_reg_debug_1_2 <= b_reg_1_2 @[TPU.scala 104:20]
    io_b_reg_debug_2_0 <= b_reg_2_0 @[TPU.scala 104:20]
    io_b_reg_debug_2_1 <= b_reg_2_1 @[TPU.scala 104:20]
    io_b_reg_debug_2_2 <= b_reg_2_2 @[TPU.scala 104:20]
    a_reg_0_0 <= io_a_in_0 @[TPU.scala 115:39]
    a_reg_0_1 <= a_reg_0_0 @[TPU.scala 118:39]
    a_reg_0_2 <= a_reg_0_1 @[TPU.scala 118:39]
    a_reg_1_0 <= io_a_in_1 @[TPU.scala 123:39]
    a_reg_1_1 <= a_reg_1_0 @[TPU.scala 126:39]
    a_reg_1_2 <= a_reg_1_1 @[TPU.scala 126:39]
    a_reg_2_0 <= io_a_in_2 @[TPU.scala 123:39]
    a_reg_2_1 <= a_reg_2_0 @[TPU.scala 126:39]
    a_reg_2_2 <= a_reg_2_1 @[TPU.scala 126:39]
    b_reg_0_0 <= io_b_in_0_0 @[TPU.scala 105:11]
    b_reg_0_1 <= io_b_in_0_1 @[TPU.scala 105:11]
    b_reg_0_2 <= io_b_in_0_2 @[TPU.scala 105:11]
    b_reg_1_0 <= io_b_in_1_0 @[TPU.scala 105:11]
    b_reg_1_1 <= io_b_in_1_1 @[TPU.scala 105:11]
    b_reg_1_2 <= io_b_in_1_2 @[TPU.scala 105:11]
    b_reg_2_0 <= io_b_in_2_0 @[TPU.scala 105:11]
    b_reg_2_1 <= io_b_in_2_1 @[TPU.scala 105:11]
    b_reg_2_2 <= io_b_in_2_2 @[TPU.scala 105:11]
    cms_reg_0_0 <= cmp_input @[TPU.scala 129:33]
    cms_reg_0_1 <= cmp_input_1 @[TPU.scala 129:33]
    cms_reg_0_2 <= cmp_input_2 @[TPU.scala 129:33]
    cms_reg_1_0 <= cmp_input_3 @[TPU.scala 129:33]
    cms_reg_1_1 <= cmp_input_4 @[TPU.scala 129:33]
    cms_reg_1_2 <= cmp_input_5 @[TPU.scala 129:33]
    cms_reg_2_0 <= cmp_input_6 @[TPU.scala 129:33]
    cms_reg_2_1 <= cmp_input_7 @[TPU.scala 129:33]
    cms_reg_2_2 <= cmp_input_8 @[TPU.scala 129:33]

  module ChiselTPU :
    input clock : Clock
    input reset : UInt<1>
    input io_a_0_0 : SInt<8>
    input io_a_0_1 : SInt<8>
    input io_a_0_2 : SInt<8>
    input io_a_1_0 : SInt<8>
    input io_a_1_1 : SInt<8>
    input io_a_1_2 : SInt<8>
    input io_a_2_0 : SInt<8>
    input io_a_2_1 : SInt<8>
    input io_a_2_2 : SInt<8>
    input io_b_0_0 : SInt<8>
    input io_b_0_1 : SInt<8>
    input io_b_0_2 : SInt<8>
    input io_b_1_0 : SInt<8>
    input io_b_1_1 : SInt<8>
    input io_b_1_2 : SInt<8>
    input io_b_2_0 : SInt<8>
    input io_b_2_1 : SInt<8>
    input io_b_2_2 : SInt<8>
    output io_out_0_0 : SInt<8>
    output io_out_0_1 : SInt<8>
    output io_out_0_2 : SInt<8>
    output io_out_1_0 : SInt<8>
    output io_out_1_1 : SInt<8>
    output io_out_1_2 : SInt<8>
    output io_out_2_0 : SInt<8>
    output io_out_2_1 : SInt<8>
    output io_out_2_2 : SInt<8>

    inst actReg of ActReg @[TPU.scala 16:22]
    inst systArr of SystArr @[TPU.scala 18:23]
    reg cycle : UInt<4>, clock with :
      reset => (UInt<1>("h0"), cycle) @[Counter.scala 61:40]
    node wrap_wrap = eq(cycle, UInt<4>("h8")) @[Counter.scala 73:24]
    node _wrap_value_T = add(cycle, UInt<1>("h1")) @[Counter.scala 77:24]
    node _wrap_value_T_1 = tail(_wrap_value_T, 1) @[Counter.scala 77:24]
    node _GEN_0 = mux(wrap_wrap, UInt<1>("h0"), _wrap_value_T_1) @[Counter.scala 77:15 87:{20,28}]
    node _GEN_1 = mux(UInt<1>("h1"), _GEN_0, cycle) @[Counter.scala 118:16 61:40]
    node _GEN_2 = mux(UInt<1>("h1"), wrap_wrap, UInt<1>("h0")) @[Counter.scala 118:{16,23}]
    reg myOut_0_0 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), myOut_0_0) @[TPU.scala 25:22]
    reg myOut_0_1 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), myOut_0_1) @[TPU.scala 25:22]
    reg myOut_0_2 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), myOut_0_2) @[TPU.scala 25:22]
    reg myOut_1_0 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), myOut_1_0) @[TPU.scala 25:22]
    reg myOut_1_1 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), myOut_1_1) @[TPU.scala 25:22]
    reg myOut_1_2 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), myOut_1_2) @[TPU.scala 25:22]
    reg myOut_2_0 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), myOut_2_0) @[TPU.scala 25:22]
    reg myOut_2_1 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), myOut_2_1) @[TPU.scala 25:22]
    reg myOut_2_2 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), myOut_2_2) @[TPU.scala 25:22]
    node _actReg_io_index_T = sub(UInt<3>("h4"), cycle) @[TPU.scala 29:35]
    node _actReg_io_index_T_1 = tail(_actReg_io_index_T, 1) @[TPU.scala 29:35]
    reg REG_0 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), REG_0) @[TPU.scala 31:29]
    reg REG_1 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), REG_1) @[TPU.scala 31:29]
    reg REG_2 : SInt<8>, clock with :
      reset => (UInt<1>("h0"), REG_2) @[TPU.scala 31:29]
    node _T = gt(cycle, UInt<2>("h3")) @[TPU.scala 57:16]
    node _T_1 = lt(cycle, UInt<3>("h7")) @[TPU.scala 57:37]
    node _T_2 = and(_T, _T_1) @[TPU.scala 57:28]
    node _T_3 = sub(cycle, UInt<3>("h4")) @[TPU.scala 58:19]
    node _T_4 = tail(_T_3, 1) @[TPU.scala 58:19]
    node _T_5 = bits(_T_4, 1, 0)
    node _myOut_T_5_0 = systArr.io_out_0 @[TPU.scala 58:{39,39}]
    node _GEN_3 = mux(eq(UInt<1>("h0"), _T_5), _myOut_T_5_0, myOut_0_0) @[TPU.scala 25:22 58:{39,39}]
    node _GEN_4 = mux(eq(UInt<1>("h1"), _T_5), _myOut_T_5_0, myOut_1_0) @[TPU.scala 25:22 58:{39,39}]
    node _GEN_5 = mux(eq(UInt<2>("h2"), _T_5), _myOut_T_5_0, myOut_2_0) @[TPU.scala 25:22 58:{39,39}]
    node _GEN_6 = mux(_T_2, _GEN_3, myOut_0_0) @[TPU.scala 25:22 57:55]
    node _GEN_7 = mux(_T_2, _GEN_4, myOut_1_0) @[TPU.scala 25:22 57:55]
    node _GEN_8 = mux(_T_2, _GEN_5, myOut_2_0) @[TPU.scala 25:22 57:55]
    node _T_6 = gt(cycle, UInt<3>("h4")) @[TPU.scala 57:16]
    node _T_7 = lt(cycle, UInt<4>("h8")) @[TPU.scala 57:37]
    node _T_8 = and(_T_6, _T_7) @[TPU.scala 57:28]
    node _T_9 = sub(cycle, UInt<3>("h5")) @[TPU.scala 58:19]
    node _T_10 = tail(_T_9, 1) @[TPU.scala 58:19]
    node _T_11 = bits(_T_10, 1, 0)
    node _myOut_T_11_1 = systArr.io_out_1 @[TPU.scala 58:{39,39}]
    node _GEN_9 = mux(eq(UInt<1>("h0"), _T_11), _myOut_T_11_1, myOut_0_1) @[TPU.scala 25:22 58:{39,39}]
    node _GEN_10 = mux(eq(UInt<1>("h1"), _T_11), _myOut_T_11_1, myOut_1_1) @[TPU.scala 25:22 58:{39,39}]
    node _GEN_11 = mux(eq(UInt<2>("h2"), _T_11), _myOut_T_11_1, myOut_2_1) @[TPU.scala 25:22 58:{39,39}]
    node _GEN_12 = mux(_T_8, _GEN_9, myOut_0_1) @[TPU.scala 25:22 57:55]
    node _GEN_13 = mux(_T_8, _GEN_10, myOut_1_1) @[TPU.scala 25:22 57:55]
    node _GEN_14 = mux(_T_8, _GEN_11, myOut_2_1) @[TPU.scala 25:22 57:55]
    node _T_12 = gt(cycle, UInt<3>("h5")) @[TPU.scala 57:16]
    node _T_13 = lt(cycle, UInt<4>("h9")) @[TPU.scala 57:37]
    node _T_14 = and(_T_12, _T_13) @[TPU.scala 57:28]
    node _T_15 = sub(cycle, UInt<3>("h6")) @[TPU.scala 58:19]
    node _T_16 = tail(_T_15, 1) @[TPU.scala 58:19]
    node _T_17 = bits(_T_16, 1, 0)
    node _myOut_T_17_2 = systArr.io_out_2 @[TPU.scala 58:{39,39}]
    node _GEN_15 = mux(eq(UInt<1>("h0"), _T_17), _myOut_T_17_2, myOut_0_2) @[TPU.scala 25:22 58:{39,39}]
    node _GEN_16 = mux(eq(UInt<1>("h1"), _T_17), _myOut_T_17_2, myOut_1_2) @[TPU.scala 25:22 58:{39,39}]
    node _GEN_17 = mux(eq(UInt<2>("h2"), _T_17), _myOut_T_17_2, myOut_2_2) @[TPU.scala 25:22 58:{39,39}]
    node _GEN_18 = mux(_T_14, _GEN_15, myOut_0_2) @[TPU.scala 25:22 57:55]
    node _GEN_19 = mux(_T_14, _GEN_16, myOut_1_2) @[TPU.scala 25:22 57:55]
    node _GEN_20 = mux(_T_14, _GEN_17, myOut_2_2) @[TPU.scala 25:22 57:55]
    node _T_18 = bits(reset, 0, 0) @[TPU.scala 62:9]
    node _T_19 = eq(_T_18, UInt<1>("h0")) @[TPU.scala 62:9]
    node _T_20 = bits(reset, 0, 0) @[TPU.scala 64:11]
    node _T_21 = eq(_T_20, UInt<1>("h0")) @[TPU.scala 64:11]
    node _T_22 = bits(reset, 0, 0) @[TPU.scala 64:11]
    node _T_23 = eq(_T_22, UInt<1>("h0")) @[TPU.scala 64:11]
    node _T_24 = bits(reset, 0, 0) @[TPU.scala 64:11]
    node _T_25 = eq(_T_24, UInt<1>("h0")) @[TPU.scala 64:11]
    node wrap = _GEN_2
    node _myOut_WIRE_0_0 = asSInt(UInt<8>("h0")) @[TPU.scala 25:{45,45}]
    node _myOut_WIRE_0_1 = asSInt(UInt<8>("h0")) @[TPU.scala 25:{45,45}]
    node _myOut_WIRE_0_2 = asSInt(UInt<8>("h0")) @[TPU.scala 25:{45,45}]
    node _myOut_WIRE_1_0 = asSInt(UInt<8>("h0")) @[TPU.scala 25:{45,45}]
    node _myOut_WIRE_1_1 = asSInt(UInt<8>("h0")) @[TPU.scala 25:{45,45}]
    node _myOut_WIRE_1_2 = asSInt(UInt<8>("h0")) @[TPU.scala 25:{45,45}]
    node _myOut_WIRE_2_0 = asSInt(UInt<8>("h0")) @[TPU.scala 25:{45,45}]
    node _myOut_WIRE_2_1 = asSInt(UInt<8>("h0")) @[TPU.scala 25:{45,45}]
    node _myOut_WIRE_2_2 = asSInt(UInt<8>("h0")) @[TPU.scala 25:{45,45}]
    io_out_0_0 <= myOut_0_0 @[TPU.scala 26:10]
    io_out_0_1 <= myOut_0_1 @[TPU.scala 26:10]
    io_out_0_2 <= myOut_0_2 @[TPU.scala 26:10]
    io_out_1_0 <= myOut_1_0 @[TPU.scala 26:10]
    io_out_1_1 <= myOut_1_1 @[TPU.scala 26:10]
    io_out_1_2 <= myOut_1_2 @[TPU.scala 26:10]
    io_out_2_0 <= myOut_2_0 @[TPU.scala 26:10]
    io_out_2_1 <= myOut_2_1 @[TPU.scala 26:10]
    io_out_2_2 <= myOut_2_2 @[TPU.scala 26:10]
    cycle <= mux(reset, UInt<4>("h0"), _GEN_1) @[Counter.scala 61:{40,40}]
    actReg.clock <= clock
    actReg.reset <= reset
    actReg.io_index <= _actReg_io_index_T_1 @[TPU.scala 29:19]
    actReg.io_a_0_0 <= io_a_0_0 @[TPU.scala 30:15]
    actReg.io_a_0_1 <= io_a_0_1 @[TPU.scala 30:15]
    actReg.io_a_0_2 <= io_a_0_2 @[TPU.scala 30:15]
    actReg.io_a_1_0 <= io_a_1_0 @[TPU.scala 30:15]
    actReg.io_a_1_1 <= io_a_1_1 @[TPU.scala 30:15]
    actReg.io_a_1_2 <= io_a_1_2 @[TPU.scala 30:15]
    actReg.io_a_2_0 <= io_a_2_0 @[TPU.scala 30:15]
    actReg.io_a_2_1 <= io_a_2_1 @[TPU.scala 30:15]
    actReg.io_a_2_2 <= io_a_2_2 @[TPU.scala 30:15]
    systArr.clock <= clock
    systArr.reset <= reset
    systArr.io_a_in_0 <= REG_0 @[TPU.scala 31:19]
    systArr.io_a_in_1 <= REG_1 @[TPU.scala 31:19]
    systArr.io_a_in_2 <= REG_2 @[TPU.scala 31:19]
    systArr.io_b_in_0_0 <= io_b_0_0 @[TPU.scala 32:19]
    systArr.io_b_in_0_1 <= io_b_0_1 @[TPU.scala 32:19]
    systArr.io_b_in_0_2 <= io_b_0_2 @[TPU.scala 32:19]
    systArr.io_b_in_1_0 <= io_b_1_0 @[TPU.scala 32:19]
    systArr.io_b_in_1_1 <= io_b_1_1 @[TPU.scala 32:19]
    systArr.io_b_in_1_2 <= io_b_1_2 @[TPU.scala 32:19]
    systArr.io_b_in_2_0 <= io_b_2_0 @[TPU.scala 32:19]
    systArr.io_b_in_2_1 <= io_b_2_1 @[TPU.scala 32:19]
    systArr.io_b_in_2_2 <= io_b_2_2 @[TPU.scala 32:19]
    myOut_0_0 <= mux(reset, _myOut_WIRE_0_0, _GEN_6) @[TPU.scala 25:{22,22}]
    myOut_0_1 <= mux(reset, _myOut_WIRE_0_1, _GEN_12) @[TPU.scala 25:{22,22}]
    myOut_0_2 <= mux(reset, _myOut_WIRE_0_2, _GEN_18) @[TPU.scala 25:{22,22}]
    myOut_1_0 <= mux(reset, _myOut_WIRE_1_0, _GEN_7) @[TPU.scala 25:{22,22}]
    myOut_1_1 <= mux(reset, _myOut_WIRE_1_1, _GEN_13) @[TPU.scala 25:{22,22}]
    myOut_1_2 <= mux(reset, _myOut_WIRE_1_2, _GEN_19) @[TPU.scala 25:{22,22}]
    myOut_2_0 <= mux(reset, _myOut_WIRE_2_0, _GEN_8) @[TPU.scala 25:{22,22}]
    myOut_2_1 <= mux(reset, _myOut_WIRE_2_1, _GEN_14) @[TPU.scala 25:{22,22}]
    myOut_2_2 <= mux(reset, _myOut_WIRE_2_2, _GEN_20) @[TPU.scala 25:{22,22}]
    REG_0 <= actReg.io_a_out_0 @[TPU.scala 31:29]
    REG_1 <= actReg.io_a_out_1 @[TPU.scala 31:29]
    REG_2 <= actReg.io_a_out_2 @[TPU.scala 31:29]
    printf(clock, and(and(UInt<1>("h1"), _T_19), UInt<1>("h1")), "MY TPU OUT: \n") : printf @[TPU.scala 62:9]
    printf(clock, and(and(UInt<1>("h1"), _T_21), UInt<1>("h1")), "Vec(%d, %d, %d)\n", myOut_0_0, myOut_0_1, myOut_0_2) : printf_1 @[TPU.scala 64:11]
    printf(clock, and(and(UInt<1>("h1"), _T_23), UInt<1>("h1")), "Vec(%d, %d, %d)\n", myOut_1_0, myOut_1_1, myOut_1_2) : printf_2 @[TPU.scala 64:11]
    printf(clock, and(and(UInt<1>("h1"), _T_25), UInt<1>("h1")), "Vec(%d, %d, %d)\n", myOut_2_0, myOut_2_1, myOut_2_2) : printf_3 @[TPU.scala 64:11]
