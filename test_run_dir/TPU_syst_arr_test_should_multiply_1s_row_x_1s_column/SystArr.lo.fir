circuit SystArr :
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
