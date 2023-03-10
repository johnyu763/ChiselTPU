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
    output io_b_reg_debug_0_0 : SInt<32>
    output io_b_reg_debug_0_1 : SInt<32>
    output io_b_reg_debug_0_2 : SInt<32>
    output io_b_reg_debug_1_0 : SInt<32>
    output io_b_reg_debug_1_1 : SInt<32>
    output io_b_reg_debug_1_2 : SInt<32>
    output io_b_reg_debug_2_0 : SInt<32>
    output io_b_reg_debug_2_1 : SInt<32>
    output io_b_reg_debug_2_2 : SInt<32>

    reg a_reg_0_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_0_0) @[TPU.scala 127:20]
    reg a_reg_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_0_1) @[TPU.scala 127:20]
    reg a_reg_0_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_0_2) @[TPU.scala 127:20]
    reg a_reg_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_1_0) @[TPU.scala 127:20]
    reg a_reg_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_1_1) @[TPU.scala 127:20]
    reg a_reg_1_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_1_2) @[TPU.scala 127:20]
    reg a_reg_2_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_2_0) @[TPU.scala 127:20]
    reg a_reg_2_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_2_1) @[TPU.scala 127:20]
    reg a_reg_2_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_2_2) @[TPU.scala 127:20]
    reg b_reg_0_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_0_0) @[TPU.scala 128:20]
    reg b_reg_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_0_1) @[TPU.scala 128:20]
    reg b_reg_0_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_0_2) @[TPU.scala 128:20]
    reg b_reg_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_1_0) @[TPU.scala 128:20]
    reg b_reg_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_1_1) @[TPU.scala 128:20]
    reg b_reg_1_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_1_2) @[TPU.scala 128:20]
    reg b_reg_2_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_2_0) @[TPU.scala 128:20]
    reg b_reg_2_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_2_1) @[TPU.scala 128:20]
    reg b_reg_2_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_2_2) @[TPU.scala 128:20]
    reg cms_reg_0_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_0_0) @[TPU.scala 129:22]
    reg cms_reg_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_0_1) @[TPU.scala 129:22]
    reg cms_reg_0_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_0_2) @[TPU.scala 129:22]
    reg cms_reg_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_1_0) @[TPU.scala 129:22]
    reg cms_reg_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_1_1) @[TPU.scala 129:22]
    reg cms_reg_1_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_1_2) @[TPU.scala 129:22]
    reg cms_reg_2_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_2_0) @[TPU.scala 129:22]
    reg cms_reg_2_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_2_1) @[TPU.scala 129:22]
    reg cms_reg_2_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_2_2) @[TPU.scala 129:22]
    node _cmp_input_T = mul(io_a_in_0, b_reg_0_0) @[TPU.scala 141:48]
    node _cmp_input_T_1 = mul(a_reg_0_0, b_reg_0_1) @[TPU.scala 144:54]
    node _cmp_input_T_2 = mul(a_reg_0_1, b_reg_0_2) @[TPU.scala 144:54]
    node _cmp_input_T_3 = mul(io_a_in_1, b_reg_1_0) @[TPU.scala 149:49]
    node _cmp_input_T_4 = add(_cmp_input_T_3, cms_reg_0_0) @[TPU.scala 149:70]
    node _cmp_input_T_5 = tail(_cmp_input_T_4, 1) @[TPU.scala 149:70]
    node _cmp_input_T_6 = asSInt(_cmp_input_T_5) @[TPU.scala 149:70]
    node _cmp_input_T_7 = mul(a_reg_1_0, b_reg_1_1) @[TPU.scala 152:55]
    node _cmp_input_T_8 = add(_cmp_input_T_7, cms_reg_0_1) @[TPU.scala 152:76]
    node _cmp_input_T_9 = tail(_cmp_input_T_8, 1) @[TPU.scala 152:76]
    node _cmp_input_T_10 = asSInt(_cmp_input_T_9) @[TPU.scala 152:76]
    node _cmp_input_T_11 = mul(a_reg_1_1, b_reg_1_2) @[TPU.scala 152:55]
    node _cmp_input_T_12 = add(_cmp_input_T_11, cms_reg_0_2) @[TPU.scala 152:76]
    node _cmp_input_T_13 = tail(_cmp_input_T_12, 1) @[TPU.scala 152:76]
    node _cmp_input_T_14 = asSInt(_cmp_input_T_13) @[TPU.scala 152:76]
    node _cmp_input_T_15 = mul(io_a_in_2, b_reg_2_0) @[TPU.scala 149:49]
    node _cmp_input_T_16 = add(_cmp_input_T_15, cms_reg_1_0) @[TPU.scala 149:70]
    node _cmp_input_T_17 = tail(_cmp_input_T_16, 1) @[TPU.scala 149:70]
    node _cmp_input_T_18 = asSInt(_cmp_input_T_17) @[TPU.scala 149:70]
    node _cmp_input_T_19 = mul(a_reg_2_0, b_reg_2_1) @[TPU.scala 152:55]
    node _cmp_input_T_20 = add(_cmp_input_T_19, cms_reg_1_1) @[TPU.scala 152:76]
    node _cmp_input_T_21 = tail(_cmp_input_T_20, 1) @[TPU.scala 152:76]
    node _cmp_input_T_22 = asSInt(_cmp_input_T_21) @[TPU.scala 152:76]
    node _cmp_input_T_23 = mul(a_reg_2_1, b_reg_2_2) @[TPU.scala 152:55]
    node _cmp_input_T_24 = add(_cmp_input_T_23, cms_reg_1_2) @[TPU.scala 152:76]
    node _cmp_input_T_25 = tail(_cmp_input_T_24, 1) @[TPU.scala 152:76]
    node _cmp_input_T_26 = asSInt(_cmp_input_T_25) @[TPU.scala 152:76]
    node _T = bits(reset, 0, 0) @[TPU.scala 162:11]
    node _T_1 = eq(_T, UInt<1>("h0")) @[TPU.scala 162:11]
    node _T_2 = bits(reset, 0, 0) @[TPU.scala 164:13]
    node _T_3 = eq(_T_2, UInt<1>("h0")) @[TPU.scala 164:13]
    node _T_4 = bits(reset, 0, 0) @[TPU.scala 164:13]
    node _T_5 = eq(_T_4, UInt<1>("h0")) @[TPU.scala 164:13]
    node _T_6 = bits(reset, 0, 0) @[TPU.scala 164:13]
    node _T_7 = eq(_T_6, UInt<1>("h0")) @[TPU.scala 164:13]
    node _T_8 = bits(reset, 0, 0) @[TPU.scala 166:11]
    node _T_9 = eq(_T_8, UInt<1>("h0")) @[TPU.scala 166:11]
    node _T_10 = bits(reset, 0, 0) @[TPU.scala 168:13]
    node _T_11 = eq(_T_10, UInt<1>("h0")) @[TPU.scala 168:13]
    node _T_12 = bits(reset, 0, 0) @[TPU.scala 168:13]
    node _T_13 = eq(_T_12, UInt<1>("h0")) @[TPU.scala 168:13]
    node _T_14 = bits(reset, 0, 0) @[TPU.scala 168:13]
    node _T_15 = eq(_T_14, UInt<1>("h0")) @[TPU.scala 168:13]
    node _T_16 = bits(reset, 0, 0) @[TPU.scala 170:11]
    node _T_17 = eq(_T_16, UInt<1>("h0")) @[TPU.scala 170:11]
    node _T_18 = bits(reset, 0, 0) @[TPU.scala 172:13]
    node _T_19 = eq(_T_18, UInt<1>("h0")) @[TPU.scala 172:13]
    node _T_20 = bits(reset, 0, 0) @[TPU.scala 172:13]
    node _T_21 = eq(_T_20, UInt<1>("h0")) @[TPU.scala 172:13]
    node _T_22 = bits(reset, 0, 0) @[TPU.scala 172:13]
    node _T_23 = eq(_T_22, UInt<1>("h0")) @[TPU.scala 172:13]
    node _T_24 = bits(reset, 0, 0) @[TPU.scala 174:11]
    node _T_25 = eq(_T_24, UInt<1>("h0")) @[TPU.scala 174:11]
    node _T_26 = bits(reset, 0, 0) @[TPU.scala 176:13]
    node _T_27 = eq(_T_26, UInt<1>("h0")) @[TPU.scala 176:13]
    node _T_28 = bits(reset, 0, 0) @[TPU.scala 176:13]
    node _T_29 = eq(_T_28, UInt<1>("h0")) @[TPU.scala 176:13]
    node _T_30 = bits(reset, 0, 0) @[TPU.scala 176:13]
    node _T_31 = eq(_T_30, UInt<1>("h0")) @[TPU.scala 176:13]
    node _T_32 = bits(reset, 0, 0) @[TPU.scala 178:11]
    node _T_33 = eq(_T_32, UInt<1>("h0")) @[TPU.scala 178:11]
    node _T_34 = bits(reset, 0, 0) @[TPU.scala 180:13]
    node _T_35 = eq(_T_34, UInt<1>("h0")) @[TPU.scala 180:13]
    node _T_36 = bits(reset, 0, 0) @[TPU.scala 180:13]
    node _T_37 = eq(_T_36, UInt<1>("h0")) @[TPU.scala 180:13]
    node _T_38 = bits(reset, 0, 0) @[TPU.scala 180:13]
    node _T_39 = eq(_T_38, UInt<1>("h0")) @[TPU.scala 180:13]
    node cmp_input = asSInt(bits(_cmp_input_T, 31, 0)) @[TPU.scala 138:33 141:31]
    node cmp_input_1 = asSInt(bits(_cmp_input_T_1, 31, 0)) @[TPU.scala 138:33 144:31]
    node cmp_input_2 = asSInt(bits(_cmp_input_T_2, 31, 0)) @[TPU.scala 138:33 144:31]
    node cmp_input_3 = asSInt(bits(_cmp_input_T_6, 31, 0)) @[TPU.scala 138:33 149:31]
    node cmp_input_4 = asSInt(bits(_cmp_input_T_10, 31, 0)) @[TPU.scala 138:33 152:31]
    node cmp_input_5 = asSInt(bits(_cmp_input_T_14, 31, 0)) @[TPU.scala 138:33 152:31]
    node cmp_input_6 = asSInt(bits(_cmp_input_T_18, 31, 0)) @[TPU.scala 138:33 149:31]
    node cmp_input_7 = asSInt(bits(_cmp_input_T_22, 31, 0)) @[TPU.scala 138:33 152:31]
    node cmp_input_8 = asSInt(bits(_cmp_input_T_26, 31, 0)) @[TPU.scala 138:33 152:31]
    io_out_0 <= cms_reg_2_0 @[TPU.scala 134:19]
    io_out_1 <= cms_reg_2_1 @[TPU.scala 134:19]
    io_out_2 <= cms_reg_2_2 @[TPU.scala 134:19]
    io_cmp_debug_0_0 <= cms_reg_0_0 @[TPU.scala 130:18]
    io_cmp_debug_0_1 <= cms_reg_0_1 @[TPU.scala 130:18]
    io_cmp_debug_0_2 <= cms_reg_0_2 @[TPU.scala 130:18]
    io_cmp_debug_1_0 <= cms_reg_1_0 @[TPU.scala 130:18]
    io_cmp_debug_1_1 <= cms_reg_1_1 @[TPU.scala 130:18]
    io_cmp_debug_1_2 <= cms_reg_1_2 @[TPU.scala 130:18]
    io_cmp_debug_2_0 <= cms_reg_2_0 @[TPU.scala 130:18]
    io_cmp_debug_2_1 <= cms_reg_2_1 @[TPU.scala 130:18]
    io_cmp_debug_2_2 <= cms_reg_2_2 @[TPU.scala 130:18]
    io_b_reg_debug_0_0 <= b_reg_0_0 @[TPU.scala 131:20]
    io_b_reg_debug_0_1 <= b_reg_0_1 @[TPU.scala 131:20]
    io_b_reg_debug_0_2 <= b_reg_0_2 @[TPU.scala 131:20]
    io_b_reg_debug_1_0 <= b_reg_1_0 @[TPU.scala 131:20]
    io_b_reg_debug_1_1 <= b_reg_1_1 @[TPU.scala 131:20]
    io_b_reg_debug_1_2 <= b_reg_1_2 @[TPU.scala 131:20]
    io_b_reg_debug_2_0 <= b_reg_2_0 @[TPU.scala 131:20]
    io_b_reg_debug_2_1 <= b_reg_2_1 @[TPU.scala 131:20]
    io_b_reg_debug_2_2 <= b_reg_2_2 @[TPU.scala 131:20]
    a_reg_0_0 <= io_a_in_0 @[TPU.scala 142:39]
    a_reg_0_1 <= a_reg_0_0 @[TPU.scala 145:39]
    a_reg_0_2 <= a_reg_0_1 @[TPU.scala 145:39]
    a_reg_1_0 <= io_a_in_1 @[TPU.scala 150:39]
    a_reg_1_1 <= a_reg_1_0 @[TPU.scala 153:39]
    a_reg_1_2 <= a_reg_1_1 @[TPU.scala 153:39]
    a_reg_2_0 <= io_a_in_2 @[TPU.scala 150:39]
    a_reg_2_1 <= a_reg_2_0 @[TPU.scala 153:39]
    a_reg_2_2 <= a_reg_2_1 @[TPU.scala 153:39]
    b_reg_0_0 <= io_b_in_0_0 @[TPU.scala 132:11]
    b_reg_0_1 <= io_b_in_0_1 @[TPU.scala 132:11]
    b_reg_0_2 <= io_b_in_0_2 @[TPU.scala 132:11]
    b_reg_1_0 <= io_b_in_1_0 @[TPU.scala 132:11]
    b_reg_1_1 <= io_b_in_1_1 @[TPU.scala 132:11]
    b_reg_1_2 <= io_b_in_1_2 @[TPU.scala 132:11]
    b_reg_2_0 <= io_b_in_2_0 @[TPU.scala 132:11]
    b_reg_2_1 <= io_b_in_2_1 @[TPU.scala 132:11]
    b_reg_2_2 <= io_b_in_2_2 @[TPU.scala 132:11]
    cms_reg_0_0 <= cmp_input @[TPU.scala 156:33]
    cms_reg_0_1 <= cmp_input_1 @[TPU.scala 156:33]
    cms_reg_0_2 <= cmp_input_2 @[TPU.scala 156:33]
    cms_reg_1_0 <= cmp_input_3 @[TPU.scala 156:33]
    cms_reg_1_1 <= cmp_input_4 @[TPU.scala 156:33]
    cms_reg_1_2 <= cmp_input_5 @[TPU.scala 156:33]
    cms_reg_2_0 <= cmp_input_6 @[TPU.scala 156:33]
    cms_reg_2_1 <= cmp_input_7 @[TPU.scala 156:33]
    cms_reg_2_2 <= cmp_input_8 @[TPU.scala 156:33]
    printf(clock, and(and(UInt<1>("h1"), _T_1), UInt<1>("h1")), "MY A_IN:\n") : printf @[TPU.scala 162:11]
    printf(clock, and(and(UInt<1>("h1"), _T_3), UInt<1>("h1")), "%d\n", io_a_in_0) : printf_1 @[TPU.scala 164:13]
    printf(clock, and(and(UInt<1>("h1"), _T_5), UInt<1>("h1")), "%d\n", io_a_in_1) : printf_2 @[TPU.scala 164:13]
    printf(clock, and(and(UInt<1>("h1"), _T_7), UInt<1>("h1")), "%d\n", io_a_in_2) : printf_3 @[TPU.scala 164:13]
    printf(clock, and(and(UInt<1>("h1"), _T_9), UInt<1>("h1")), "MY B_IN:\n") : printf_4 @[TPU.scala 166:11]
    printf(clock, and(and(UInt<1>("h1"), _T_11), UInt<1>("h1")), "Vec(%d, %d, %d)\n", io_b_in_0_0, io_b_in_0_1, io_b_in_0_2) : printf_5 @[TPU.scala 168:13]
    printf(clock, and(and(UInt<1>("h1"), _T_13), UInt<1>("h1")), "Vec(%d, %d, %d)\n", io_b_in_1_0, io_b_in_1_1, io_b_in_1_2) : printf_6 @[TPU.scala 168:13]
    printf(clock, and(and(UInt<1>("h1"), _T_15), UInt<1>("h1")), "Vec(%d, %d, %d)\n", io_b_in_2_0, io_b_in_2_1, io_b_in_2_2) : printf_7 @[TPU.scala 168:13]
    printf(clock, and(and(UInt<1>("h1"), _T_17), UInt<1>("h1")), "MY A_REG:\n") : printf_8 @[TPU.scala 170:11]
    printf(clock, and(and(UInt<1>("h1"), _T_19), UInt<1>("h1")), "Vec(%d, %d, %d)\n", a_reg_0_0, a_reg_0_1, a_reg_0_2) : printf_9 @[TPU.scala 172:13]
    printf(clock, and(and(UInt<1>("h1"), _T_21), UInt<1>("h1")), "Vec(%d, %d, %d)\n", a_reg_1_0, a_reg_1_1, a_reg_1_2) : printf_10 @[TPU.scala 172:13]
    printf(clock, and(and(UInt<1>("h1"), _T_23), UInt<1>("h1")), "Vec(%d, %d, %d)\n", a_reg_2_0, a_reg_2_1, a_reg_2_2) : printf_11 @[TPU.scala 172:13]
    printf(clock, and(and(UInt<1>("h1"), _T_25), UInt<1>("h1")), "MY CSM:\n") : printf_12 @[TPU.scala 174:11]
    printf(clock, and(and(UInt<1>("h1"), _T_27), UInt<1>("h1")), "Vec(%d, %d, %d)\n", cms_reg_0_0, cms_reg_0_1, cms_reg_0_2) : printf_13 @[TPU.scala 176:13]
    printf(clock, and(and(UInt<1>("h1"), _T_29), UInt<1>("h1")), "Vec(%d, %d, %d)\n", cms_reg_1_0, cms_reg_1_1, cms_reg_1_2) : printf_14 @[TPU.scala 176:13]
    printf(clock, and(and(UInt<1>("h1"), _T_31), UInt<1>("h1")), "Vec(%d, %d, %d)\n", cms_reg_2_0, cms_reg_2_1, cms_reg_2_2) : printf_15 @[TPU.scala 176:13]
    printf(clock, and(and(UInt<1>("h1"), _T_33), UInt<1>("h1")), "MY OUT:\n") : printf_16 @[TPU.scala 178:11]
    printf(clock, and(and(UInt<1>("h1"), _T_35), UInt<1>("h1")), "%d\n", io_out_0) : printf_17 @[TPU.scala 180:13]
    printf(clock, and(and(UInt<1>("h1"), _T_37), UInt<1>("h1")), "%d\n", io_out_1) : printf_18 @[TPU.scala 180:13]
    printf(clock, and(and(UInt<1>("h1"), _T_39), UInt<1>("h1")), "%d\n", io_out_2) : printf_19 @[TPU.scala 180:13]
