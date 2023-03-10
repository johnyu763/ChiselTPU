circuit ChiselTPU :
  module ActReg :
    input clock : Clock
    input reset : UInt<1>
    input io_index : UInt<4>
    input io_a_0_0 : SInt<32>
    input io_a_0_1 : SInt<32>
    input io_a_0_2 : SInt<32>
    input io_a_0_3 : SInt<32>
    input io_a_1_0 : SInt<32>
    input io_a_1_1 : SInt<32>
    input io_a_1_2 : SInt<32>
    input io_a_1_3 : SInt<32>
    output io_a_out_0 : SInt<32>
    output io_a_out_1 : SInt<32>
    output io_a_out_2 : SInt<32>
    output io_a_out_3 : SInt<32>

    node _T = lt(io_index, UInt<3>("h5")) @[TPU.scala 101:19]
    node _T_1 = asSInt(io_index) @[TPU.scala 103:35]
    node _T_2 = sub(asSInt(UInt<3>("h3")), _T_1) @[TPU.scala 103:25]
    node _T_3 = tail(_T_2, 1) @[TPU.scala 103:25]
    node _T_4 = asSInt(_T_3) @[TPU.scala 103:25]
    node _T_5 = leq(_T_4, asSInt(UInt<1>("h0"))) @[TPU.scala 103:42]
    node _T_6 = asSInt(io_index) @[TPU.scala 103:77]
    node _T_7 = sub(asSInt(UInt<4>("h5")), _T_6) @[TPU.scala 103:67]
    node _T_8 = tail(_T_7, 1) @[TPU.scala 103:67]
    node _T_9 = asSInt(_T_8) @[TPU.scala 103:67]
    node _T_10 = gt(_T_9, asSInt(UInt<1>("h0"))) @[TPU.scala 103:84]
    node _T_11 = and(_T_5, _T_10) @[TPU.scala 103:49]
    node _io_a_out_0_T = sub(UInt<3>("h4"), io_index) @[TPU.scala 104:55]
    node _io_a_out_0_T_1 = tail(_io_a_out_0_T, 1) @[TPU.scala 104:55]
    node _io_a_out_0_T_2 = bits(_io_a_out_0_T_1, 0, 0)
    node _GEN_0 = validif(eq(UInt<1>("h0"), _io_a_out_0_T_2), io_a_0_0) @[TPU.scala 104:{25,25}]
    node _GEN_1 = mux(eq(UInt<1>("h1"), _io_a_out_0_T_2), io_a_1_0, _GEN_0) @[TPU.scala 104:{25,25}]
    node _io_a_io_a_out_0_T_2_0 = _GEN_1 @[TPU.scala 104:25]
    node _GEN_2 = mux(_T_11, _io_a_io_a_out_0_T_2_0, asSInt(UInt<1>("h0"))) @[TPU.scala 103:90 104:25 107:25]
    node _T_12 = asSInt(io_index) @[TPU.scala 103:35]
    node _T_13 = sub(asSInt(UInt<3>("h2")), _T_12) @[TPU.scala 103:25]
    node _T_14 = tail(_T_13, 1) @[TPU.scala 103:25]
    node _T_15 = asSInt(_T_14) @[TPU.scala 103:25]
    node _T_16 = leq(_T_15, asSInt(UInt<1>("h0"))) @[TPU.scala 103:42]
    node _T_17 = asSInt(io_index) @[TPU.scala 103:77]
    node _T_18 = sub(asSInt(UInt<4>("h4")), _T_17) @[TPU.scala 103:67]
    node _T_19 = tail(_T_18, 1) @[TPU.scala 103:67]
    node _T_20 = asSInt(_T_19) @[TPU.scala 103:67]
    node _T_21 = gt(_T_20, asSInt(UInt<1>("h0"))) @[TPU.scala 103:84]
    node _T_22 = and(_T_16, _T_21) @[TPU.scala 103:49]
    node _io_a_out_1_T = sub(UInt<2>("h3"), io_index) @[TPU.scala 104:55]
    node _io_a_out_1_T_1 = tail(_io_a_out_1_T, 1) @[TPU.scala 104:55]
    node _io_a_out_1_T_2 = bits(_io_a_out_1_T_1, 0, 0)
    node _GEN_3 = validif(eq(UInt<1>("h0"), _io_a_out_1_T_2), io_a_0_1) @[TPU.scala 104:{25,25}]
    node _GEN_4 = mux(eq(UInt<1>("h1"), _io_a_out_1_T_2), io_a_1_1, _GEN_3) @[TPU.scala 104:{25,25}]
    node _io_a_io_a_out_1_T_2_1 = _GEN_4 @[TPU.scala 104:25]
    node _GEN_5 = mux(_T_22, _io_a_io_a_out_1_T_2_1, asSInt(UInt<1>("h0"))) @[TPU.scala 103:90 104:25 107:25]
    node _T_23 = asSInt(io_index) @[TPU.scala 103:35]
    node _T_24 = sub(asSInt(UInt<2>("h1")), _T_23) @[TPU.scala 103:25]
    node _T_25 = tail(_T_24, 1) @[TPU.scala 103:25]
    node _T_26 = asSInt(_T_25) @[TPU.scala 103:25]
    node _T_27 = leq(_T_26, asSInt(UInt<1>("h0"))) @[TPU.scala 103:42]
    node _T_28 = asSInt(io_index) @[TPU.scala 103:77]
    node _T_29 = sub(asSInt(UInt<3>("h3")), _T_28) @[TPU.scala 103:67]
    node _T_30 = tail(_T_29, 1) @[TPU.scala 103:67]
    node _T_31 = asSInt(_T_30) @[TPU.scala 103:67]
    node _T_32 = gt(_T_31, asSInt(UInt<1>("h0"))) @[TPU.scala 103:84]
    node _T_33 = and(_T_27, _T_32) @[TPU.scala 103:49]
    node _io_a_out_2_T = sub(UInt<2>("h2"), io_index) @[TPU.scala 104:55]
    node _io_a_out_2_T_1 = tail(_io_a_out_2_T, 1) @[TPU.scala 104:55]
    node _io_a_out_2_T_2 = bits(_io_a_out_2_T_1, 0, 0)
    node _GEN_6 = validif(eq(UInt<1>("h0"), _io_a_out_2_T_2), io_a_0_2) @[TPU.scala 104:{25,25}]
    node _GEN_7 = mux(eq(UInt<1>("h1"), _io_a_out_2_T_2), io_a_1_2, _GEN_6) @[TPU.scala 104:{25,25}]
    node _io_a_io_a_out_2_T_2_2 = _GEN_7 @[TPU.scala 104:25]
    node _GEN_8 = mux(_T_33, _io_a_io_a_out_2_T_2_2, asSInt(UInt<1>("h0"))) @[TPU.scala 103:90 104:25 107:25]
    node _T_34 = asSInt(io_index) @[TPU.scala 103:35]
    node _T_35 = sub(asSInt(UInt<1>("h0")), _T_34) @[TPU.scala 103:25]
    node _T_36 = tail(_T_35, 1) @[TPU.scala 103:25]
    node _T_37 = asSInt(_T_36) @[TPU.scala 103:25]
    node _T_38 = leq(_T_37, asSInt(UInt<1>("h0"))) @[TPU.scala 103:42]
    node _T_39 = asSInt(io_index) @[TPU.scala 103:77]
    node _T_40 = sub(asSInt(UInt<3>("h2")), _T_39) @[TPU.scala 103:67]
    node _T_41 = tail(_T_40, 1) @[TPU.scala 103:67]
    node _T_42 = asSInt(_T_41) @[TPU.scala 103:67]
    node _T_43 = gt(_T_42, asSInt(UInt<1>("h0"))) @[TPU.scala 103:84]
    node _T_44 = and(_T_38, _T_43) @[TPU.scala 103:49]
    node _io_a_out_3_T = sub(UInt<1>("h1"), io_index) @[TPU.scala 104:55]
    node _io_a_out_3_T_1 = tail(_io_a_out_3_T, 1) @[TPU.scala 104:55]
    node _io_a_out_3_T_2 = bits(_io_a_out_3_T_1, 0, 0)
    node _GEN_9 = validif(eq(UInt<1>("h0"), _io_a_out_3_T_2), io_a_0_3) @[TPU.scala 104:{25,25}]
    node _GEN_10 = mux(eq(UInt<1>("h1"), _io_a_out_3_T_2), io_a_1_3, _GEN_9) @[TPU.scala 104:{25,25}]
    node _io_a_io_a_out_3_T_2_3 = _GEN_10 @[TPU.scala 104:25]
    node _GEN_11 = mux(_T_44, _io_a_io_a_out_3_T_2_3, asSInt(UInt<1>("h0"))) @[TPU.scala 103:90 104:25 107:25]
    node _GEN_12 = mux(_T, _GEN_2, asSInt(UInt<1>("h0"))) @[TPU.scala 101:35 113:23]
    node _GEN_13 = mux(_T, _GEN_5, asSInt(UInt<1>("h0"))) @[TPU.scala 101:35 113:23]
    node _GEN_14 = mux(_T, _GEN_8, asSInt(UInt<1>("h0"))) @[TPU.scala 101:35 113:23]
    node _GEN_15 = mux(_T, _GEN_11, asSInt(UInt<1>("h0"))) @[TPU.scala 101:35 113:23]
    io_a_out_0 <= _GEN_12
    io_a_out_1 <= _GEN_13
    io_a_out_2 <= _GEN_14
    io_a_out_3 <= _GEN_15

  module SystArr :
    input clock : Clock
    input reset : UInt<1>
    input io_a_in_0 : SInt<32>
    input io_a_in_1 : SInt<32>
    input io_a_in_2 : SInt<32>
    input io_a_in_3 : SInt<32>
    input io_b_in_0_0 : SInt<32>
    input io_b_in_0_1 : SInt<32>
    input io_b_in_1_0 : SInt<32>
    input io_b_in_1_1 : SInt<32>
    input io_b_in_2_0 : SInt<32>
    input io_b_in_2_1 : SInt<32>
    input io_b_in_3_0 : SInt<32>
    input io_b_in_3_1 : SInt<32>
    output io_out_0 : SInt<32>
    output io_out_1 : SInt<32>
    output io_cmp_debug_0_0 : SInt<32>
    output io_cmp_debug_0_1 : SInt<32>
    output io_cmp_debug_1_0 : SInt<32>
    output io_cmp_debug_1_1 : SInt<32>
    output io_cmp_debug_2_0 : SInt<32>
    output io_cmp_debug_2_1 : SInt<32>
    output io_cmp_debug_3_0 : SInt<32>
    output io_cmp_debug_3_1 : SInt<32>
    output io_b_reg_debug_0_0 : SInt<32>
    output io_b_reg_debug_0_1 : SInt<32>
    output io_b_reg_debug_1_0 : SInt<32>
    output io_b_reg_debug_1_1 : SInt<32>
    output io_b_reg_debug_2_0 : SInt<32>
    output io_b_reg_debug_2_1 : SInt<32>
    output io_b_reg_debug_3_0 : SInt<32>
    output io_b_reg_debug_3_1 : SInt<32>

    reg a_reg_0_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_0_0) @[TPU.scala 127:20]
    reg a_reg_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_0_1) @[TPU.scala 127:20]
    reg a_reg_0_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_0_2) @[TPU.scala 127:20]
    reg a_reg_0_3 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_0_3) @[TPU.scala 127:20]
    reg a_reg_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_1_0) @[TPU.scala 127:20]
    reg a_reg_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_1_1) @[TPU.scala 127:20]
    reg a_reg_1_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_1_2) @[TPU.scala 127:20]
    reg a_reg_1_3 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_1_3) @[TPU.scala 127:20]
    reg a_reg_2_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_2_0) @[TPU.scala 127:20]
    reg a_reg_2_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_2_1) @[TPU.scala 127:20]
    reg a_reg_2_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_2_2) @[TPU.scala 127:20]
    reg a_reg_2_3 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_2_3) @[TPU.scala 127:20]
    reg a_reg_3_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_3_0) @[TPU.scala 127:20]
    reg a_reg_3_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_3_1) @[TPU.scala 127:20]
    reg a_reg_3_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_3_2) @[TPU.scala 127:20]
    reg a_reg_3_3 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_3_3) @[TPU.scala 127:20]
    reg b_reg_0_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_0_0) @[TPU.scala 128:20]
    reg b_reg_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_0_1) @[TPU.scala 128:20]
    reg b_reg_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_1_0) @[TPU.scala 128:20]
    reg b_reg_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_1_1) @[TPU.scala 128:20]
    reg b_reg_2_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_2_0) @[TPU.scala 128:20]
    reg b_reg_2_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_2_1) @[TPU.scala 128:20]
    reg b_reg_3_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_3_0) @[TPU.scala 128:20]
    reg b_reg_3_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_3_1) @[TPU.scala 128:20]
    reg cms_reg_0_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_0_0) @[TPU.scala 129:22]
    reg cms_reg_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_0_1) @[TPU.scala 129:22]
    reg cms_reg_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_1_0) @[TPU.scala 129:22]
    reg cms_reg_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_1_1) @[TPU.scala 129:22]
    reg cms_reg_2_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_2_0) @[TPU.scala 129:22]
    reg cms_reg_2_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_2_1) @[TPU.scala 129:22]
    reg cms_reg_3_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_3_0) @[TPU.scala 129:22]
    reg cms_reg_3_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_3_1) @[TPU.scala 129:22]
    node _cmp_input_T = mul(io_a_in_0, b_reg_0_0) @[TPU.scala 141:48]
    node _cmp_input_T_1 = mul(a_reg_0_0, b_reg_0_1) @[TPU.scala 144:54]
    node _cmp_input_T_2 = mul(io_a_in_1, b_reg_1_0) @[TPU.scala 149:49]
    node _cmp_input_T_3 = add(_cmp_input_T_2, cms_reg_0_0) @[TPU.scala 149:70]
    node _cmp_input_T_4 = tail(_cmp_input_T_3, 1) @[TPU.scala 149:70]
    node _cmp_input_T_5 = asSInt(_cmp_input_T_4) @[TPU.scala 149:70]
    node _cmp_input_T_6 = mul(a_reg_1_0, b_reg_1_1) @[TPU.scala 152:55]
    node _cmp_input_T_7 = add(_cmp_input_T_6, cms_reg_0_1) @[TPU.scala 152:76]
    node _cmp_input_T_8 = tail(_cmp_input_T_7, 1) @[TPU.scala 152:76]
    node _cmp_input_T_9 = asSInt(_cmp_input_T_8) @[TPU.scala 152:76]
    node _cmp_input_T_10 = mul(io_a_in_2, b_reg_2_0) @[TPU.scala 149:49]
    node _cmp_input_T_11 = add(_cmp_input_T_10, cms_reg_1_0) @[TPU.scala 149:70]
    node _cmp_input_T_12 = tail(_cmp_input_T_11, 1) @[TPU.scala 149:70]
    node _cmp_input_T_13 = asSInt(_cmp_input_T_12) @[TPU.scala 149:70]
    node _cmp_input_T_14 = mul(a_reg_2_0, b_reg_2_1) @[TPU.scala 152:55]
    node _cmp_input_T_15 = add(_cmp_input_T_14, cms_reg_1_1) @[TPU.scala 152:76]
    node _cmp_input_T_16 = tail(_cmp_input_T_15, 1) @[TPU.scala 152:76]
    node _cmp_input_T_17 = asSInt(_cmp_input_T_16) @[TPU.scala 152:76]
    node _cmp_input_T_18 = mul(io_a_in_3, b_reg_3_0) @[TPU.scala 149:49]
    node _cmp_input_T_19 = add(_cmp_input_T_18, cms_reg_2_0) @[TPU.scala 149:70]
    node _cmp_input_T_20 = tail(_cmp_input_T_19, 1) @[TPU.scala 149:70]
    node _cmp_input_T_21 = asSInt(_cmp_input_T_20) @[TPU.scala 149:70]
    node _cmp_input_T_22 = mul(a_reg_3_0, b_reg_3_1) @[TPU.scala 152:55]
    node _cmp_input_T_23 = add(_cmp_input_T_22, cms_reg_2_1) @[TPU.scala 152:76]
    node _cmp_input_T_24 = tail(_cmp_input_T_23, 1) @[TPU.scala 152:76]
    node _cmp_input_T_25 = asSInt(_cmp_input_T_24) @[TPU.scala 152:76]
    node _T = asUInt(reset) @[TPU.scala 162:11]
    node _T_1 = eq(_T, UInt<1>("h0")) @[TPU.scala 162:11]
    node _T_2 = asUInt(reset) @[TPU.scala 164:13]
    node _T_3 = eq(_T_2, UInt<1>("h0")) @[TPU.scala 164:13]
    node _T_4 = asUInt(reset) @[TPU.scala 164:13]
    node _T_5 = eq(_T_4, UInt<1>("h0")) @[TPU.scala 164:13]
    node _T_6 = asUInt(reset) @[TPU.scala 164:13]
    node _T_7 = eq(_T_6, UInt<1>("h0")) @[TPU.scala 164:13]
    node _T_8 = asUInt(reset) @[TPU.scala 164:13]
    node _T_9 = eq(_T_8, UInt<1>("h0")) @[TPU.scala 164:13]
    node _T_10 = asUInt(reset) @[TPU.scala 166:11]
    node _T_11 = eq(_T_10, UInt<1>("h0")) @[TPU.scala 166:11]
    node _T_12 = asUInt(reset) @[TPU.scala 168:13]
    node _T_13 = eq(_T_12, UInt<1>("h0")) @[TPU.scala 168:13]
    node _T_14 = asUInt(reset) @[TPU.scala 168:13]
    node _T_15 = eq(_T_14, UInt<1>("h0")) @[TPU.scala 168:13]
    node _T_16 = asUInt(reset) @[TPU.scala 168:13]
    node _T_17 = eq(_T_16, UInt<1>("h0")) @[TPU.scala 168:13]
    node _T_18 = asUInt(reset) @[TPU.scala 168:13]
    node _T_19 = eq(_T_18, UInt<1>("h0")) @[TPU.scala 168:13]
    node _T_20 = asUInt(reset) @[TPU.scala 170:11]
    node _T_21 = eq(_T_20, UInt<1>("h0")) @[TPU.scala 170:11]
    node _T_22 = asUInt(reset) @[TPU.scala 172:13]
    node _T_23 = eq(_T_22, UInt<1>("h0")) @[TPU.scala 172:13]
    node _T_24 = asUInt(reset) @[TPU.scala 172:13]
    node _T_25 = eq(_T_24, UInt<1>("h0")) @[TPU.scala 172:13]
    node _T_26 = asUInt(reset) @[TPU.scala 172:13]
    node _T_27 = eq(_T_26, UInt<1>("h0")) @[TPU.scala 172:13]
    node _T_28 = asUInt(reset) @[TPU.scala 172:13]
    node _T_29 = eq(_T_28, UInt<1>("h0")) @[TPU.scala 172:13]
    node _T_30 = asUInt(reset) @[TPU.scala 174:11]
    node _T_31 = eq(_T_30, UInt<1>("h0")) @[TPU.scala 174:11]
    node _T_32 = asUInt(reset) @[TPU.scala 176:13]
    node _T_33 = eq(_T_32, UInt<1>("h0")) @[TPU.scala 176:13]
    node _T_34 = asUInt(reset) @[TPU.scala 176:13]
    node _T_35 = eq(_T_34, UInt<1>("h0")) @[TPU.scala 176:13]
    node _T_36 = asUInt(reset) @[TPU.scala 176:13]
    node _T_37 = eq(_T_36, UInt<1>("h0")) @[TPU.scala 176:13]
    node _T_38 = asUInt(reset) @[TPU.scala 176:13]
    node _T_39 = eq(_T_38, UInt<1>("h0")) @[TPU.scala 176:13]
    node _T_40 = asUInt(reset) @[TPU.scala 178:11]
    node _T_41 = eq(_T_40, UInt<1>("h0")) @[TPU.scala 178:11]
    node _T_42 = asUInt(reset) @[TPU.scala 180:13]
    node _T_43 = eq(_T_42, UInt<1>("h0")) @[TPU.scala 180:13]
    node _T_44 = asUInt(reset) @[TPU.scala 180:13]
    node _T_45 = eq(_T_44, UInt<1>("h0")) @[TPU.scala 180:13]
    node cmp_input = asSInt(bits(_cmp_input_T, 31, 0)) @[TPU.scala 138:33 141:31]
    node cmp_input_1 = asSInt(bits(_cmp_input_T_1, 31, 0)) @[TPU.scala 138:33 144:31]
    node cmp_input_2 = asSInt(bits(_cmp_input_T_5, 31, 0)) @[TPU.scala 138:33 149:31]
    node cmp_input_3 = asSInt(bits(_cmp_input_T_9, 31, 0)) @[TPU.scala 138:33 152:31]
    node cmp_input_4 = asSInt(bits(_cmp_input_T_13, 31, 0)) @[TPU.scala 138:33 149:31]
    node cmp_input_5 = asSInt(bits(_cmp_input_T_17, 31, 0)) @[TPU.scala 138:33 152:31]
    node cmp_input_6 = asSInt(bits(_cmp_input_T_21, 31, 0)) @[TPU.scala 138:33 149:31]
    node cmp_input_7 = asSInt(bits(_cmp_input_T_25, 31, 0)) @[TPU.scala 138:33 152:31]
    io_out_0 <= cms_reg_3_0 @[TPU.scala 134:19]
    io_out_1 <= cms_reg_3_1 @[TPU.scala 134:19]
    io_cmp_debug_0_0 <= cms_reg_0_0 @[TPU.scala 130:18]
    io_cmp_debug_0_1 <= cms_reg_0_1 @[TPU.scala 130:18]
    io_cmp_debug_1_0 <= cms_reg_1_0 @[TPU.scala 130:18]
    io_cmp_debug_1_1 <= cms_reg_1_1 @[TPU.scala 130:18]
    io_cmp_debug_2_0 <= cms_reg_2_0 @[TPU.scala 130:18]
    io_cmp_debug_2_1 <= cms_reg_2_1 @[TPU.scala 130:18]
    io_cmp_debug_3_0 <= cms_reg_3_0 @[TPU.scala 130:18]
    io_cmp_debug_3_1 <= cms_reg_3_1 @[TPU.scala 130:18]
    io_b_reg_debug_0_0 <= b_reg_0_0 @[TPU.scala 131:20]
    io_b_reg_debug_0_1 <= b_reg_0_1 @[TPU.scala 131:20]
    io_b_reg_debug_1_0 <= b_reg_1_0 @[TPU.scala 131:20]
    io_b_reg_debug_1_1 <= b_reg_1_1 @[TPU.scala 131:20]
    io_b_reg_debug_2_0 <= b_reg_2_0 @[TPU.scala 131:20]
    io_b_reg_debug_2_1 <= b_reg_2_1 @[TPU.scala 131:20]
    io_b_reg_debug_3_0 <= b_reg_3_0 @[TPU.scala 131:20]
    io_b_reg_debug_3_1 <= b_reg_3_1 @[TPU.scala 131:20]
    a_reg_0_0 <= io_a_in_0 @[TPU.scala 142:39]
    a_reg_0_1 <= a_reg_0_0 @[TPU.scala 145:39]
    a_reg_0_2 <= a_reg_0_2 @[TPU.scala 127:20]
    a_reg_0_3 <= a_reg_0_3 @[TPU.scala 127:20]
    a_reg_1_0 <= io_a_in_1 @[TPU.scala 150:39]
    a_reg_1_1 <= a_reg_1_0 @[TPU.scala 153:39]
    a_reg_1_2 <= a_reg_1_2 @[TPU.scala 127:20]
    a_reg_1_3 <= a_reg_1_3 @[TPU.scala 127:20]
    a_reg_2_0 <= io_a_in_2 @[TPU.scala 150:39]
    a_reg_2_1 <= a_reg_2_0 @[TPU.scala 153:39]
    a_reg_2_2 <= a_reg_2_2 @[TPU.scala 127:20]
    a_reg_2_3 <= a_reg_2_3 @[TPU.scala 127:20]
    a_reg_3_0 <= io_a_in_3 @[TPU.scala 150:39]
    a_reg_3_1 <= a_reg_3_0 @[TPU.scala 153:39]
    a_reg_3_2 <= a_reg_3_2 @[TPU.scala 127:20]
    a_reg_3_3 <= a_reg_3_3 @[TPU.scala 127:20]
    b_reg_0_0 <= io_b_in_0_0 @[TPU.scala 132:11]
    b_reg_0_1 <= io_b_in_0_1 @[TPU.scala 132:11]
    b_reg_1_0 <= io_b_in_1_0 @[TPU.scala 132:11]
    b_reg_1_1 <= io_b_in_1_1 @[TPU.scala 132:11]
    b_reg_2_0 <= io_b_in_2_0 @[TPU.scala 132:11]
    b_reg_2_1 <= io_b_in_2_1 @[TPU.scala 132:11]
    b_reg_3_0 <= io_b_in_3_0 @[TPU.scala 132:11]
    b_reg_3_1 <= io_b_in_3_1 @[TPU.scala 132:11]
    cms_reg_0_0 <= cmp_input @[TPU.scala 156:33]
    cms_reg_0_1 <= cmp_input_1 @[TPU.scala 156:33]
    cms_reg_1_0 <= cmp_input_2 @[TPU.scala 156:33]
    cms_reg_1_1 <= cmp_input_3 @[TPU.scala 156:33]
    cms_reg_2_0 <= cmp_input_4 @[TPU.scala 156:33]
    cms_reg_2_1 <= cmp_input_5 @[TPU.scala 156:33]
    cms_reg_3_0 <= cmp_input_6 @[TPU.scala 156:33]
    cms_reg_3_1 <= cmp_input_7 @[TPU.scala 156:33]
    printf(clock, and(and(UInt<1>("h1"), _T_1), UInt<1>("h1")), "MY A_IN:\n") : printf @[TPU.scala 162:11]
    printf(clock, and(and(UInt<1>("h1"), _T_3), UInt<1>("h1")), "%d\n", io_a_in_0) : printf_1 @[TPU.scala 164:13]
    printf(clock, and(and(UInt<1>("h1"), _T_5), UInt<1>("h1")), "%d\n", io_a_in_1) : printf_2 @[TPU.scala 164:13]
    printf(clock, and(and(UInt<1>("h1"), _T_7), UInt<1>("h1")), "%d\n", io_a_in_2) : printf_3 @[TPU.scala 164:13]
    printf(clock, and(and(UInt<1>("h1"), _T_9), UInt<1>("h1")), "%d\n", io_a_in_3) : printf_4 @[TPU.scala 164:13]
    printf(clock, and(and(UInt<1>("h1"), _T_11), UInt<1>("h1")), "MY B_IN:\n") : printf_5 @[TPU.scala 166:11]
    printf(clock, and(and(UInt<1>("h1"), _T_13), UInt<1>("h1")), "Vec(%d, %d)\n", io_b_in_0_0, io_b_in_0_1) : printf_6 @[TPU.scala 168:13]
    printf(clock, and(and(UInt<1>("h1"), _T_15), UInt<1>("h1")), "Vec(%d, %d)\n", io_b_in_1_0, io_b_in_1_1) : printf_7 @[TPU.scala 168:13]
    printf(clock, and(and(UInt<1>("h1"), _T_17), UInt<1>("h1")), "Vec(%d, %d)\n", io_b_in_2_0, io_b_in_2_1) : printf_8 @[TPU.scala 168:13]
    printf(clock, and(and(UInt<1>("h1"), _T_19), UInt<1>("h1")), "Vec(%d, %d)\n", io_b_in_3_0, io_b_in_3_1) : printf_9 @[TPU.scala 168:13]
    printf(clock, and(and(UInt<1>("h1"), _T_21), UInt<1>("h1")), "MY A_REG:\n") : printf_10 @[TPU.scala 170:11]
    printf(clock, and(and(UInt<1>("h1"), _T_23), UInt<1>("h1")), "Vec(%d, %d, %d, %d)\n", a_reg_0_0, a_reg_0_1, a_reg_0_2, a_reg_0_3) : printf_11 @[TPU.scala 172:13]
    printf(clock, and(and(UInt<1>("h1"), _T_25), UInt<1>("h1")), "Vec(%d, %d, %d, %d)\n", a_reg_1_0, a_reg_1_1, a_reg_1_2, a_reg_1_3) : printf_12 @[TPU.scala 172:13]
    printf(clock, and(and(UInt<1>("h1"), _T_27), UInt<1>("h1")), "Vec(%d, %d, %d, %d)\n", a_reg_2_0, a_reg_2_1, a_reg_2_2, a_reg_2_3) : printf_13 @[TPU.scala 172:13]
    printf(clock, and(and(UInt<1>("h1"), _T_29), UInt<1>("h1")), "Vec(%d, %d, %d, %d)\n", a_reg_3_0, a_reg_3_1, a_reg_3_2, a_reg_3_3) : printf_14 @[TPU.scala 172:13]
    printf(clock, and(and(UInt<1>("h1"), _T_31), UInt<1>("h1")), "MY CSM:\n") : printf_15 @[TPU.scala 174:11]
    printf(clock, and(and(UInt<1>("h1"), _T_33), UInt<1>("h1")), "Vec(%d, %d)\n", cms_reg_0_0, cms_reg_0_1) : printf_16 @[TPU.scala 176:13]
    printf(clock, and(and(UInt<1>("h1"), _T_35), UInt<1>("h1")), "Vec(%d, %d)\n", cms_reg_1_0, cms_reg_1_1) : printf_17 @[TPU.scala 176:13]
    printf(clock, and(and(UInt<1>("h1"), _T_37), UInt<1>("h1")), "Vec(%d, %d)\n", cms_reg_2_0, cms_reg_2_1) : printf_18 @[TPU.scala 176:13]
    printf(clock, and(and(UInt<1>("h1"), _T_39), UInt<1>("h1")), "Vec(%d, %d)\n", cms_reg_3_0, cms_reg_3_1) : printf_19 @[TPU.scala 176:13]
    printf(clock, and(and(UInt<1>("h1"), _T_41), UInt<1>("h1")), "MY OUT:\n") : printf_20 @[TPU.scala 178:11]
    printf(clock, and(and(UInt<1>("h1"), _T_43), UInt<1>("h1")), "%d\n", io_out_0) : printf_21 @[TPU.scala 180:13]
    printf(clock, and(and(UInt<1>("h1"), _T_45), UInt<1>("h1")), "%d\n", io_out_1) : printf_22 @[TPU.scala 180:13]

  module ChiselTPU :
    input clock : Clock
    input reset : UInt<1>
    output io_a_ready : UInt<1>
    input io_a_valid : UInt<1>
    input io_a_bits_0_0 : SInt<32>
    input io_a_bits_0_1 : SInt<32>
    input io_a_bits_0_2 : SInt<32>
    input io_a_bits_0_3 : SInt<32>
    input io_a_bits_1_0 : SInt<32>
    input io_a_bits_1_1 : SInt<32>
    input io_a_bits_1_2 : SInt<32>
    input io_a_bits_1_3 : SInt<32>
    output io_b_ready : UInt<1>
    input io_b_valid : UInt<1>
    input io_b_bits_0_0 : SInt<32>
    input io_b_bits_0_1 : SInt<32>
    input io_b_bits_1_0 : SInt<32>
    input io_b_bits_1_1 : SInt<32>
    input io_b_bits_2_0 : SInt<32>
    input io_b_bits_2_1 : SInt<32>
    input io_b_bits_3_0 : SInt<32>
    input io_b_bits_3_1 : SInt<32>
    output io_out_0_0 : SInt<32>
    output io_out_0_1 : SInt<32>
    output io_out_1_0 : SInt<32>
    output io_out_1_1 : SInt<32>

    inst actReg of ActReg @[TPU.scala 28:22]
    inst systArr of SystArr @[TPU.scala 30:23]
    reg state : UInt<2>, clock with :
      reset => (UInt<1>("h0"), state) @[TPU.scala 25:22]
    node _T = eq(state, UInt<2>("h2")) @[TPU.scala 26:37]
    reg cycle : UInt<4>, clock with :
      reset => (UInt<1>("h0"), cycle) @[Counter.scala 61:40]
    node wrap_wrap = eq(cycle, UInt<4>("ha")) @[Counter.scala 73:24]
    node _wrap_value_T = add(cycle, UInt<1>("h1")) @[Counter.scala 77:24]
    node _wrap_value_T_1 = tail(_wrap_value_T, 1) @[Counter.scala 77:24]
    node _GEN_0 = mux(wrap_wrap, UInt<1>("h0"), _wrap_value_T_1) @[Counter.scala 77:15 87:{20,28}]
    node _GEN_1 = mux(_T, _GEN_0, cycle) @[Counter.scala 118:16 61:40]
    node _GEN_2 = mux(_T, wrap_wrap, UInt<1>("h0")) @[Counter.scala 118:{16,23}]
    reg myOut_0_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), myOut_0_0) @[TPU.scala 31:22]
    reg myOut_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), myOut_0_1) @[TPU.scala 31:22]
    reg myOut_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), myOut_1_0) @[TPU.scala 31:22]
    reg myOut_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), myOut_1_1) @[TPU.scala 31:22]
    reg a_ready : UInt<1>, clock with :
      reset => (UInt<1>("h0"), a_ready) @[TPU.scala 32:24]
    reg b_ready : UInt<1>, clock with :
      reset => (UInt<1>("h0"), b_ready) @[TPU.scala 33:24]
    node _actReg_io_index_T = sub(UInt<3>("h4"), cycle) @[TPU.scala 40:35]
    node _actReg_io_index_T_1 = tail(_actReg_io_index_T, 1) @[TPU.scala 40:35]
    reg REG_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), REG_0) @[TPU.scala 41:29]
    reg REG_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), REG_1) @[TPU.scala 41:29]
    reg REG_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), REG_2) @[TPU.scala 41:29]
    reg REG_3 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), REG_3) @[TPU.scala 41:29]
    reg act_in_0_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), act_in_0_0) @[TPU.scala 42:23]
    reg act_in_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), act_in_0_1) @[TPU.scala 42:23]
    reg act_in_0_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), act_in_0_2) @[TPU.scala 42:23]
    reg act_in_0_3 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), act_in_0_3) @[TPU.scala 42:23]
    reg act_in_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), act_in_1_0) @[TPU.scala 42:23]
    reg act_in_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), act_in_1_1) @[TPU.scala 42:23]
    reg act_in_1_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), act_in_1_2) @[TPU.scala 42:23]
    reg act_in_1_3 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), act_in_1_3) @[TPU.scala 42:23]
    reg syst_in_0_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), syst_in_0_0) @[TPU.scala 43:24]
    reg syst_in_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), syst_in_0_1) @[TPU.scala 43:24]
    reg syst_in_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), syst_in_1_0) @[TPU.scala 43:24]
    reg syst_in_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), syst_in_1_1) @[TPU.scala 43:24]
    reg syst_in_2_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), syst_in_2_0) @[TPU.scala 43:24]
    reg syst_in_2_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), syst_in_2_1) @[TPU.scala 43:24]
    reg syst_in_3_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), syst_in_3_0) @[TPU.scala 43:24]
    reg syst_in_3_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), syst_in_3_1) @[TPU.scala 43:24]
    node _T_1 = eq(state, UInt<2>("h0")) @[TPU.scala 47:14]
    node _T_2 = and(io_a_valid, io_a_ready) @[TPU.scala 48:23]
    node _GEN_3 = mux(_T_2, UInt<2>("h1"), state) @[TPU.scala 48:37 49:15 25:22]
    node _GEN_4 = mux(_T_2, io_a_bits_0_0, act_in_0_0) @[TPU.scala 48:37 50:16 42:23]
    node _GEN_5 = mux(_T_2, io_a_bits_0_1, act_in_0_1) @[TPU.scala 48:37 50:16 42:23]
    node _GEN_6 = mux(_T_2, io_a_bits_0_2, act_in_0_2) @[TPU.scala 48:37 50:16 42:23]
    node _GEN_7 = mux(_T_2, io_a_bits_0_3, act_in_0_3) @[TPU.scala 48:37 50:16 42:23]
    node _GEN_8 = mux(_T_2, io_a_bits_1_0, act_in_1_0) @[TPU.scala 48:37 50:16 42:23]
    node _GEN_9 = mux(_T_2, io_a_bits_1_1, act_in_1_1) @[TPU.scala 48:37 50:16 42:23]
    node _GEN_10 = mux(_T_2, io_a_bits_1_2, act_in_1_2) @[TPU.scala 48:37 50:16 42:23]
    node _GEN_11 = mux(_T_2, io_a_bits_1_3, act_in_1_3) @[TPU.scala 48:37 50:16 42:23]
    node _GEN_12 = mux(_T_2, UInt<1>("h0"), a_ready) @[TPU.scala 48:37 51:17 32:24]
    node _T_3 = eq(state, UInt<2>("h1")) @[TPU.scala 54:19]
    node _T_4 = and(io_b_valid, io_b_ready) @[TPU.scala 55:23]
    node _GEN_13 = mux(_T_4, UInt<2>("h2"), state) @[TPU.scala 55:37 56:15 25:22]
    node _GEN_14 = mux(_T_4, io_b_bits_0_0, syst_in_0_0) @[TPU.scala 55:37 57:17 43:24]
    node _GEN_15 = mux(_T_4, io_b_bits_0_1, syst_in_0_1) @[TPU.scala 55:37 57:17 43:24]
    node _GEN_16 = mux(_T_4, io_b_bits_1_0, syst_in_1_0) @[TPU.scala 55:37 57:17 43:24]
    node _GEN_17 = mux(_T_4, io_b_bits_1_1, syst_in_1_1) @[TPU.scala 55:37 57:17 43:24]
    node _GEN_18 = mux(_T_4, io_b_bits_2_0, syst_in_2_0) @[TPU.scala 55:37 57:17 43:24]
    node _GEN_19 = mux(_T_4, io_b_bits_2_1, syst_in_2_1) @[TPU.scala 55:37 57:17 43:24]
    node _GEN_20 = mux(_T_4, io_b_bits_3_0, syst_in_3_0) @[TPU.scala 55:37 57:17 43:24]
    node _GEN_21 = mux(_T_4, io_b_bits_3_1, syst_in_3_1) @[TPU.scala 55:37 57:17 43:24]
    node _GEN_22 = mux(_T_4, UInt<1>("h0"), b_ready) @[TPU.scala 55:37 58:17 33:24]
    node _T_5 = eq(state, UInt<2>("h2")) @[TPU.scala 62:19]
    node _T_6 = eq(cycle, UInt<4>("ha")) @[TPU.scala 63:16]
    node _GEN_23 = mux(_T_6, UInt<2>("h3"), state) @[TPU.scala 63:36 64:13 25:22]
    node _T_7 = gt(cycle, UInt<2>("h2")) @[TPU.scala 67:18]
    node _T_8 = lt(cycle, UInt<3>("h5")) @[TPU.scala 67:39]
    node _T_9 = and(_T_7, _T_8) @[TPU.scala 67:30]
    node _T_10 = sub(cycle, UInt<2>("h3")) @[TPU.scala 68:21]
    node _T_11 = tail(_T_10, 1) @[TPU.scala 68:21]
    node _T_12 = bits(_T_11, 0, 0)
    node _myOut_T_12_0 = systArr.io_out_0 @[TPU.scala 68:{41,41}]
    node _GEN_24 = mux(eq(UInt<1>("h0"), _T_12), _myOut_T_12_0, myOut_0_0) @[TPU.scala 31:22 68:{41,41}]
    node _GEN_25 = mux(eq(UInt<1>("h1"), _T_12), _myOut_T_12_0, myOut_1_0) @[TPU.scala 31:22 68:{41,41}]
    node _GEN_26 = mux(_T_9, _GEN_24, myOut_0_0) @[TPU.scala 31:22 67:57]
    node _GEN_27 = mux(_T_9, _GEN_25, myOut_1_0) @[TPU.scala 31:22 67:57]
    node _T_13 = gt(cycle, UInt<2>("h3")) @[TPU.scala 67:18]
    node _T_14 = lt(cycle, UInt<3>("h6")) @[TPU.scala 67:39]
    node _T_15 = and(_T_13, _T_14) @[TPU.scala 67:30]
    node _T_16 = sub(cycle, UInt<3>("h4")) @[TPU.scala 68:21]
    node _T_17 = tail(_T_16, 1) @[TPU.scala 68:21]
    node _T_18 = bits(_T_17, 0, 0)
    node _myOut_T_18_1 = systArr.io_out_1 @[TPU.scala 68:{41,41}]
    node _GEN_28 = mux(eq(UInt<1>("h0"), _T_18), _myOut_T_18_1, myOut_0_1) @[TPU.scala 31:22 68:{41,41}]
    node _GEN_29 = mux(eq(UInt<1>("h1"), _T_18), _myOut_T_18_1, myOut_1_1) @[TPU.scala 31:22 68:{41,41}]
    node _GEN_30 = mux(_T_15, _GEN_28, myOut_0_1) @[TPU.scala 31:22 67:57]
    node _GEN_31 = mux(_T_15, _GEN_29, myOut_1_1) @[TPU.scala 31:22 67:57]
    node _T_19 = eq(state, UInt<2>("h3")) @[TPU.scala 86:19]
    node _T_20 = bits(reset, 0, 0) @[TPU.scala 87:11]
    node _T_21 = eq(_T_20, UInt<1>("h0")) @[TPU.scala 87:11]
    node _GEN_32 = mux(_T_19, UInt<2>("h1"), state) @[TPU.scala 86:29 88:11 25:22]
    node _GEN_33 = mux(_T_19, UInt<1>("h1"), b_ready) @[TPU.scala 86:29 89:13 33:24]
    node _WIRE_0_0 = asSInt(UInt<32>("h11")) @[TPU.scala 90:{36,36}]
    node _GEN_34 = mux(_T_19, _WIRE_0_0, myOut_0_0) @[TPU.scala 86:29 90:11 31:22]
    node _WIRE_0_1 = asSInt(UInt<32>("h11")) @[TPU.scala 90:{36,36}]
    node _GEN_35 = mux(_T_19, _WIRE_0_1, myOut_0_1) @[TPU.scala 86:29 90:11 31:22]
    node _WIRE_1_0 = asSInt(UInt<32>("h11")) @[TPU.scala 90:{36,36}]
    node _GEN_36 = mux(_T_19, _WIRE_1_0, myOut_1_0) @[TPU.scala 86:29 90:11 31:22]
    node _WIRE_1_1 = asSInt(UInt<32>("h11")) @[TPU.scala 90:{36,36}]
    node _GEN_37 = mux(_T_19, _WIRE_1_1, myOut_1_1) @[TPU.scala 86:29 90:11 31:22]
    node _GEN_38 = mux(_T_5, _GEN_23, _GEN_32) @[TPU.scala 62:32]
    node _GEN_39 = mux(_T_5, _GEN_26, _GEN_34) @[TPU.scala 62:32]
    node _GEN_40 = mux(_T_5, _GEN_27, _GEN_36) @[TPU.scala 62:32]
    node _GEN_41 = mux(_T_5, _GEN_30, _GEN_35) @[TPU.scala 62:32]
    node _GEN_42 = mux(_T_5, _GEN_31, _GEN_37) @[TPU.scala 62:32]
    node _GEN_43 = mux(_T_5, b_ready, _GEN_33) @[TPU.scala 33:24 62:32]
    node _GEN_44 = mux(_T_3, _GEN_13, _GEN_38) @[TPU.scala 54:28]
    node _GEN_45 = mux(_T_3, _GEN_14, syst_in_0_0) @[TPU.scala 43:24 54:28]
    node _GEN_46 = mux(_T_3, _GEN_15, syst_in_0_1) @[TPU.scala 43:24 54:28]
    node _GEN_47 = mux(_T_3, _GEN_16, syst_in_1_0) @[TPU.scala 43:24 54:28]
    node _GEN_48 = mux(_T_3, _GEN_17, syst_in_1_1) @[TPU.scala 43:24 54:28]
    node _GEN_49 = mux(_T_3, _GEN_18, syst_in_2_0) @[TPU.scala 43:24 54:28]
    node _GEN_50 = mux(_T_3, _GEN_19, syst_in_2_1) @[TPU.scala 43:24 54:28]
    node _GEN_51 = mux(_T_3, _GEN_20, syst_in_3_0) @[TPU.scala 43:24 54:28]
    node _GEN_52 = mux(_T_3, _GEN_21, syst_in_3_1) @[TPU.scala 43:24 54:28]
    node _GEN_53 = mux(_T_3, _GEN_22, _GEN_43) @[TPU.scala 54:28]
    node _GEN_54 = mux(_T_3, UInt<1>("h0"), _GEN_1) @[TPU.scala 54:28 60:13]
    node _GEN_55 = mux(_T_3, myOut_0_0, _GEN_39) @[TPU.scala 31:22 54:28]
    node _GEN_56 = mux(_T_3, myOut_1_0, _GEN_40) @[TPU.scala 31:22 54:28]
    node _GEN_57 = mux(_T_3, myOut_0_1, _GEN_41) @[TPU.scala 31:22 54:28]
    node _GEN_58 = mux(_T_3, myOut_1_1, _GEN_42) @[TPU.scala 31:22 54:28]
    node _GEN_59 = mux(_T_1, _GEN_3, _GEN_44) @[TPU.scala 47:23]
    node _GEN_60 = mux(_T_1, _GEN_4, act_in_0_0) @[TPU.scala 42:23 47:23]
    node _GEN_61 = mux(_T_1, _GEN_5, act_in_0_1) @[TPU.scala 42:23 47:23]
    node _GEN_62 = mux(_T_1, _GEN_6, act_in_0_2) @[TPU.scala 42:23 47:23]
    node _GEN_63 = mux(_T_1, _GEN_7, act_in_0_3) @[TPU.scala 42:23 47:23]
    node _GEN_64 = mux(_T_1, _GEN_8, act_in_1_0) @[TPU.scala 42:23 47:23]
    node _GEN_65 = mux(_T_1, _GEN_9, act_in_1_1) @[TPU.scala 42:23 47:23]
    node _GEN_66 = mux(_T_1, _GEN_10, act_in_1_2) @[TPU.scala 42:23 47:23]
    node _GEN_67 = mux(_T_1, _GEN_11, act_in_1_3) @[TPU.scala 42:23 47:23]
    node _GEN_68 = mux(_T_1, _GEN_12, a_ready) @[TPU.scala 47:23 32:24]
    node _GEN_69 = mux(_T_1, syst_in_0_0, _GEN_45) @[TPU.scala 47:23 43:24]
    node _GEN_70 = mux(_T_1, syst_in_0_1, _GEN_46) @[TPU.scala 47:23 43:24]
    node _GEN_71 = mux(_T_1, syst_in_1_0, _GEN_47) @[TPU.scala 47:23 43:24]
    node _GEN_72 = mux(_T_1, syst_in_1_1, _GEN_48) @[TPU.scala 47:23 43:24]
    node _GEN_73 = mux(_T_1, syst_in_2_0, _GEN_49) @[TPU.scala 47:23 43:24]
    node _GEN_74 = mux(_T_1, syst_in_2_1, _GEN_50) @[TPU.scala 47:23 43:24]
    node _GEN_75 = mux(_T_1, syst_in_3_0, _GEN_51) @[TPU.scala 47:23 43:24]
    node _GEN_76 = mux(_T_1, syst_in_3_1, _GEN_52) @[TPU.scala 47:23 43:24]
    node _GEN_77 = mux(_T_1, b_ready, _GEN_53) @[TPU.scala 47:23 33:24]
    node _GEN_78 = mux(_T_1, _GEN_1, _GEN_54) @[TPU.scala 47:23]
    node _GEN_79 = mux(_T_1, myOut_0_0, _GEN_55) @[TPU.scala 31:22 47:23]
    node _GEN_80 = mux(_T_1, myOut_1_0, _GEN_56) @[TPU.scala 31:22 47:23]
    node _GEN_81 = mux(_T_1, myOut_0_1, _GEN_57) @[TPU.scala 31:22 47:23]
    node _GEN_82 = mux(_T_1, myOut_1_1, _GEN_58) @[TPU.scala 31:22 47:23]
    node wrap = _GEN_2
    node _myOut_WIRE_0_0 = asSInt(UInt<32>("h0")) @[TPU.scala 31:{45,45}]
    node _myOut_WIRE_0_1 = asSInt(UInt<32>("h0")) @[TPU.scala 31:{45,45}]
    node _myOut_WIRE_1_0 = asSInt(UInt<32>("h0")) @[TPU.scala 31:{45,45}]
    node _myOut_WIRE_1_1 = asSInt(UInt<32>("h0")) @[TPU.scala 31:{45,45}]
    node _act_in_WIRE_0_0 = asSInt(UInt<32>("h0")) @[TPU.scala 42:{46,46}]
    node _act_in_WIRE_0_1 = asSInt(UInt<32>("h0")) @[TPU.scala 42:{46,46}]
    node _act_in_WIRE_0_2 = asSInt(UInt<32>("h0")) @[TPU.scala 42:{46,46}]
    node _act_in_WIRE_0_3 = asSInt(UInt<32>("h0")) @[TPU.scala 42:{46,46}]
    node _act_in_WIRE_1_0 = asSInt(UInt<32>("h0")) @[TPU.scala 42:{46,46}]
    node _act_in_WIRE_1_1 = asSInt(UInt<32>("h0")) @[TPU.scala 42:{46,46}]
    node _act_in_WIRE_1_2 = asSInt(UInt<32>("h0")) @[TPU.scala 42:{46,46}]
    node _act_in_WIRE_1_3 = asSInt(UInt<32>("h0")) @[TPU.scala 42:{46,46}]
    node _syst_in_WIRE_0_0 = asSInt(UInt<32>("h0")) @[TPU.scala 43:{47,47}]
    node _syst_in_WIRE_0_1 = asSInt(UInt<32>("h0")) @[TPU.scala 43:{47,47}]
    node _syst_in_WIRE_1_0 = asSInt(UInt<32>("h0")) @[TPU.scala 43:{47,47}]
    node _syst_in_WIRE_1_1 = asSInt(UInt<32>("h0")) @[TPU.scala 43:{47,47}]
    node _syst_in_WIRE_2_0 = asSInt(UInt<32>("h0")) @[TPU.scala 43:{47,47}]
    node _syst_in_WIRE_2_1 = asSInt(UInt<32>("h0")) @[TPU.scala 43:{47,47}]
    node _syst_in_WIRE_3_0 = asSInt(UInt<32>("h0")) @[TPU.scala 43:{47,47}]
    node _syst_in_WIRE_3_1 = asSInt(UInt<32>("h0")) @[TPU.scala 43:{47,47}]
    io_a_ready <= a_ready @[TPU.scala 35:14]
    io_b_ready <= b_ready @[TPU.scala 36:14]
    io_out_0_0 <= myOut_0_0 @[TPU.scala 37:10]
    io_out_0_1 <= myOut_0_1 @[TPU.scala 37:10]
    io_out_1_0 <= myOut_1_0 @[TPU.scala 37:10]
    io_out_1_1 <= myOut_1_1 @[TPU.scala 37:10]
    state <= mux(reset, UInt<2>("h0"), _GEN_59) @[TPU.scala 25:{22,22}]
    cycle <= mux(reset, UInt<4>("h0"), _GEN_78) @[Counter.scala 61:{40,40}]
    actReg.clock <= clock
    actReg.reset <= reset
    actReg.io_index <= _actReg_io_index_T_1 @[TPU.scala 40:19]
    actReg.io_a_0_0 <= act_in_0_0 @[TPU.scala 44:15]
    actReg.io_a_0_1 <= act_in_0_1 @[TPU.scala 44:15]
    actReg.io_a_0_2 <= act_in_0_2 @[TPU.scala 44:15]
    actReg.io_a_0_3 <= act_in_0_3 @[TPU.scala 44:15]
    actReg.io_a_1_0 <= act_in_1_0 @[TPU.scala 44:15]
    actReg.io_a_1_1 <= act_in_1_1 @[TPU.scala 44:15]
    actReg.io_a_1_2 <= act_in_1_2 @[TPU.scala 44:15]
    actReg.io_a_1_3 <= act_in_1_3 @[TPU.scala 44:15]
    systArr.clock <= clock
    systArr.reset <= reset
    systArr.io_a_in_0 <= REG_0 @[TPU.scala 41:19]
    systArr.io_a_in_1 <= REG_1 @[TPU.scala 41:19]
    systArr.io_a_in_2 <= REG_2 @[TPU.scala 41:19]
    systArr.io_a_in_3 <= REG_3 @[TPU.scala 41:19]
    systArr.io_b_in_0_0 <= syst_in_0_0 @[TPU.scala 45:19]
    systArr.io_b_in_0_1 <= syst_in_0_1 @[TPU.scala 45:19]
    systArr.io_b_in_1_0 <= syst_in_1_0 @[TPU.scala 45:19]
    systArr.io_b_in_1_1 <= syst_in_1_1 @[TPU.scala 45:19]
    systArr.io_b_in_2_0 <= syst_in_2_0 @[TPU.scala 45:19]
    systArr.io_b_in_2_1 <= syst_in_2_1 @[TPU.scala 45:19]
    systArr.io_b_in_3_0 <= syst_in_3_0 @[TPU.scala 45:19]
    systArr.io_b_in_3_1 <= syst_in_3_1 @[TPU.scala 45:19]
    myOut_0_0 <= mux(reset, _myOut_WIRE_0_0, _GEN_79) @[TPU.scala 31:{22,22}]
    myOut_0_1 <= mux(reset, _myOut_WIRE_0_1, _GEN_81) @[TPU.scala 31:{22,22}]
    myOut_1_0 <= mux(reset, _myOut_WIRE_1_0, _GEN_80) @[TPU.scala 31:{22,22}]
    myOut_1_1 <= mux(reset, _myOut_WIRE_1_1, _GEN_82) @[TPU.scala 31:{22,22}]
    a_ready <= mux(reset, UInt<1>("h1"), _GEN_68) @[TPU.scala 32:{24,24}]
    b_ready <= mux(reset, UInt<1>("h1"), _GEN_77) @[TPU.scala 33:{24,24}]
    REG_0 <= actReg.io_a_out_0 @[TPU.scala 41:29]
    REG_1 <= actReg.io_a_out_1 @[TPU.scala 41:29]
    REG_2 <= actReg.io_a_out_2 @[TPU.scala 41:29]
    REG_3 <= actReg.io_a_out_3 @[TPU.scala 41:29]
    act_in_0_0 <= mux(reset, _act_in_WIRE_0_0, _GEN_60) @[TPU.scala 42:{23,23}]
    act_in_0_1 <= mux(reset, _act_in_WIRE_0_1, _GEN_61) @[TPU.scala 42:{23,23}]
    act_in_0_2 <= mux(reset, _act_in_WIRE_0_2, _GEN_62) @[TPU.scala 42:{23,23}]
    act_in_0_3 <= mux(reset, _act_in_WIRE_0_3, _GEN_63) @[TPU.scala 42:{23,23}]
    act_in_1_0 <= mux(reset, _act_in_WIRE_1_0, _GEN_64) @[TPU.scala 42:{23,23}]
    act_in_1_1 <= mux(reset, _act_in_WIRE_1_1, _GEN_65) @[TPU.scala 42:{23,23}]
    act_in_1_2 <= mux(reset, _act_in_WIRE_1_2, _GEN_66) @[TPU.scala 42:{23,23}]
    act_in_1_3 <= mux(reset, _act_in_WIRE_1_3, _GEN_67) @[TPU.scala 42:{23,23}]
    syst_in_0_0 <= mux(reset, _syst_in_WIRE_0_0, _GEN_69) @[TPU.scala 43:{24,24}]
    syst_in_0_1 <= mux(reset, _syst_in_WIRE_0_1, _GEN_70) @[TPU.scala 43:{24,24}]
    syst_in_1_0 <= mux(reset, _syst_in_WIRE_1_0, _GEN_71) @[TPU.scala 43:{24,24}]
    syst_in_1_1 <= mux(reset, _syst_in_WIRE_1_1, _GEN_72) @[TPU.scala 43:{24,24}]
    syst_in_2_0 <= mux(reset, _syst_in_WIRE_2_0, _GEN_73) @[TPU.scala 43:{24,24}]
    syst_in_2_1 <= mux(reset, _syst_in_WIRE_2_1, _GEN_74) @[TPU.scala 43:{24,24}]
    syst_in_3_0 <= mux(reset, _syst_in_WIRE_3_0, _GEN_75) @[TPU.scala 43:{24,24}]
    syst_in_3_1 <= mux(reset, _syst_in_WIRE_3_1, _GEN_76) @[TPU.scala 43:{24,24}]
    printf(clock, and(and(and(and(and(and(UInt<1>("h1"), eq(_T_1, UInt<1>("h0"))), eq(_T_3, UInt<1>("h0"))), eq(_T_5, UInt<1>("h0"))), _T_19), _T_21), UInt<1>("h1")), "FINISH\n") : printf @[TPU.scala 87:11]
