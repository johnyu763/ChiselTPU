circuit ChiselTPU :
  module ActReg :
    input clock : Clock
    input reset : UInt<1>
    input io_index : UInt<4>
    input io_a_0_0 : SInt<32>
    input io_a_0_1 : SInt<32>
    input io_a_1_0 : SInt<32>
    input io_a_1_1 : SInt<32>
    input io_a_2_0 : SInt<32>
    input io_a_2_1 : SInt<32>
    input io_a_3_0 : SInt<32>
    input io_a_3_1 : SInt<32>
    output io_a_out_0 : SInt<32>
    output io_a_out_1 : SInt<32>

    node _T = lt(io_index, UInt<3>("h5")) @[TPU.scala 133:19]
    node _T_1 = asSInt(io_index) @[TPU.scala 135:35]
    node _T_2 = sub(asSInt(UInt<2>("h1")), _T_1) @[TPU.scala 135:25]
    node _T_3 = tail(_T_2, 1) @[TPU.scala 135:25]
    node _T_4 = asSInt(_T_3) @[TPU.scala 135:25]
    node _T_5 = leq(_T_4, asSInt(UInt<1>("h0"))) @[TPU.scala 135:42]
    node _T_6 = asSInt(io_index) @[TPU.scala 135:77]
    node _T_7 = sub(asSInt(UInt<4>("h5")), _T_6) @[TPU.scala 135:67]
    node _T_8 = tail(_T_7, 1) @[TPU.scala 135:67]
    node _T_9 = asSInt(_T_8) @[TPU.scala 135:67]
    node _T_10 = gt(_T_9, asSInt(UInt<1>("h0"))) @[TPU.scala 135:84]
    node _T_11 = and(_T_5, _T_10) @[TPU.scala 135:49]
    node _io_a_out_0_T = sub(UInt<3>("h4"), io_index) @[TPU.scala 136:55]
    node _io_a_out_0_T_1 = tail(_io_a_out_0_T, 1) @[TPU.scala 136:55]
    node _io_a_out_0_T_2 = bits(_io_a_out_0_T_1, 1, 0)
    node _GEN_0 = validif(eq(UInt<1>("h0"), _io_a_out_0_T_2), io_a_0_0) @[TPU.scala 136:{25,25}]
    node _GEN_1 = mux(eq(UInt<1>("h1"), _io_a_out_0_T_2), io_a_1_0, _GEN_0) @[TPU.scala 136:{25,25}]
    node _GEN_2 = mux(eq(UInt<2>("h2"), _io_a_out_0_T_2), io_a_2_0, _GEN_1) @[TPU.scala 136:{25,25}]
    node _GEN_3 = mux(eq(UInt<2>("h3"), _io_a_out_0_T_2), io_a_3_0, _GEN_2) @[TPU.scala 136:{25,25}]
    node _io_a_io_a_out_0_T_2_0 = _GEN_3 @[TPU.scala 136:25]
    node _GEN_4 = mux(_T_11, _io_a_io_a_out_0_T_2_0, asSInt(UInt<1>("h0"))) @[TPU.scala 135:90 136:25 139:25]
    node _T_12 = asSInt(io_index) @[TPU.scala 135:35]
    node _T_13 = sub(asSInt(UInt<1>("h0")), _T_12) @[TPU.scala 135:25]
    node _T_14 = tail(_T_13, 1) @[TPU.scala 135:25]
    node _T_15 = asSInt(_T_14) @[TPU.scala 135:25]
    node _T_16 = leq(_T_15, asSInt(UInt<1>("h0"))) @[TPU.scala 135:42]
    node _T_17 = asSInt(io_index) @[TPU.scala 135:77]
    node _T_18 = sub(asSInt(UInt<4>("h4")), _T_17) @[TPU.scala 135:67]
    node _T_19 = tail(_T_18, 1) @[TPU.scala 135:67]
    node _T_20 = asSInt(_T_19) @[TPU.scala 135:67]
    node _T_21 = gt(_T_20, asSInt(UInt<1>("h0"))) @[TPU.scala 135:84]
    node _T_22 = and(_T_16, _T_21) @[TPU.scala 135:49]
    node _io_a_out_1_T = sub(UInt<2>("h3"), io_index) @[TPU.scala 136:55]
    node _io_a_out_1_T_1 = tail(_io_a_out_1_T, 1) @[TPU.scala 136:55]
    node _io_a_out_1_T_2 = bits(_io_a_out_1_T_1, 1, 0)
    node _GEN_5 = validif(eq(UInt<1>("h0"), _io_a_out_1_T_2), io_a_0_1) @[TPU.scala 136:{25,25}]
    node _GEN_6 = mux(eq(UInt<1>("h1"), _io_a_out_1_T_2), io_a_1_1, _GEN_5) @[TPU.scala 136:{25,25}]
    node _GEN_7 = mux(eq(UInt<2>("h2"), _io_a_out_1_T_2), io_a_2_1, _GEN_6) @[TPU.scala 136:{25,25}]
    node _GEN_8 = mux(eq(UInt<2>("h3"), _io_a_out_1_T_2), io_a_3_1, _GEN_7) @[TPU.scala 136:{25,25}]
    node _io_a_io_a_out_1_T_2_1 = _GEN_8 @[TPU.scala 136:25]
    node _GEN_9 = mux(_T_22, _io_a_io_a_out_1_T_2_1, asSInt(UInt<1>("h0"))) @[TPU.scala 135:90 136:25 139:25]
    node _GEN_10 = mux(_T, _GEN_4, asSInt(UInt<1>("h0"))) @[TPU.scala 133:35 145:23]
    node _GEN_11 = mux(_T, _GEN_9, asSInt(UInt<1>("h0"))) @[TPU.scala 133:35 145:23]
    io_a_out_0 <= _GEN_10
    io_a_out_1 <= _GEN_11

  module SystArr :
    input clock : Clock
    input reset : UInt<1>
    input io_a_in_0 : SInt<32>
    input io_a_in_1 : SInt<32>
    input io_b_in_0_0 : SInt<32>
    input io_b_in_0_1 : SInt<32>
    input io_b_in_0_2 : SInt<32>
    input io_b_in_0_3 : SInt<32>
    input io_b_in_1_0 : SInt<32>
    input io_b_in_1_1 : SInt<32>
    input io_b_in_1_2 : SInt<32>
    input io_b_in_1_3 : SInt<32>
    input io_b_readingin : UInt<1>
    output io_out_0 : SInt<32>
    output io_out_1 : SInt<32>
    output io_out_2 : SInt<32>
    output io_out_3 : SInt<32>
    output io_cmp_debug_0_0 : SInt<32>
    output io_cmp_debug_0_1 : SInt<32>
    output io_cmp_debug_0_2 : SInt<32>
    output io_cmp_debug_0_3 : SInt<32>
    output io_cmp_debug_1_0 : SInt<32>
    output io_cmp_debug_1_1 : SInt<32>
    output io_cmp_debug_1_2 : SInt<32>
    output io_cmp_debug_1_3 : SInt<32>
    output io_debug_b_regs_0_0 : SInt<32>
    output io_debug_b_regs_0_1 : SInt<32>
    output io_debug_b_regs_0_2 : SInt<32>
    output io_debug_b_regs_0_3 : SInt<32>
    output io_debug_b_regs_1_0 : SInt<32>
    output io_debug_b_regs_1_1 : SInt<32>
    output io_debug_b_regs_1_2 : SInt<32>
    output io_debug_b_regs_1_3 : SInt<32>
    output io_debug_a_regs_0_0 : SInt<32>
    output io_debug_a_regs_0_1 : SInt<32>
    output io_debug_a_regs_0_2 : SInt<32>
    output io_debug_a_regs_0_3 : SInt<32>
    output io_debug_a_regs_1_0 : SInt<32>
    output io_debug_a_regs_1_1 : SInt<32>
    output io_debug_a_regs_1_2 : SInt<32>
    output io_debug_a_regs_1_3 : SInt<32>
    output io_debug_00 : SInt<32>

    reg a_reg_0_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_0_0) @[TPU.scala 162:20]
    reg a_reg_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_0_1) @[TPU.scala 162:20]
    reg a_reg_0_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_0_2) @[TPU.scala 162:20]
    reg a_reg_0_3 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_0_3) @[TPU.scala 162:20]
    reg a_reg_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_1_0) @[TPU.scala 162:20]
    reg a_reg_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_1_1) @[TPU.scala 162:20]
    reg a_reg_1_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_1_2) @[TPU.scala 162:20]
    reg a_reg_1_3 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_1_3) @[TPU.scala 162:20]
    reg b_reg_0_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_0_0) @[TPU.scala 163:20]
    reg b_reg_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_0_1) @[TPU.scala 163:20]
    reg b_reg_0_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_0_2) @[TPU.scala 163:20]
    reg b_reg_0_3 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_0_3) @[TPU.scala 163:20]
    reg b_reg_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_1_0) @[TPU.scala 163:20]
    reg b_reg_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_1_1) @[TPU.scala 163:20]
    reg b_reg_1_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_1_2) @[TPU.scala 163:20]
    reg b_reg_1_3 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_1_3) @[TPU.scala 163:20]
    reg cms_reg_0_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_0_0) @[TPU.scala 164:22]
    reg cms_reg_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_0_1) @[TPU.scala 164:22]
    reg cms_reg_0_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_0_2) @[TPU.scala 164:22]
    reg cms_reg_0_3 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_0_3) @[TPU.scala 164:22]
    reg cms_reg_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_1_0) @[TPU.scala 164:22]
    reg cms_reg_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_1_1) @[TPU.scala 164:22]
    reg cms_reg_1_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_1_2) @[TPU.scala 164:22]
    reg cms_reg_1_3 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_1_3) @[TPU.scala 164:22]
    node _io_debug_00_T = mul(io_a_in_0, b_reg_0_0) @[TPU.scala 168:31]
    node _GEN_0 = mux(io_b_readingin, io_b_in_0_0, b_reg_0_0) @[TPU.scala 169:25 170:13 172:13]
    node _GEN_1 = mux(io_b_readingin, io_b_in_0_1, b_reg_0_1) @[TPU.scala 169:25 170:13 172:13]
    node _GEN_2 = mux(io_b_readingin, io_b_in_0_2, b_reg_0_2) @[TPU.scala 169:25 170:13 172:13]
    node _GEN_3 = mux(io_b_readingin, io_b_in_0_3, b_reg_0_3) @[TPU.scala 169:25 170:13 172:13]
    node _GEN_4 = mux(io_b_readingin, io_b_in_1_0, b_reg_1_0) @[TPU.scala 169:25 170:13 172:13]
    node _GEN_5 = mux(io_b_readingin, io_b_in_1_1, b_reg_1_1) @[TPU.scala 169:25 170:13 172:13]
    node _GEN_6 = mux(io_b_readingin, io_b_in_1_2, b_reg_1_2) @[TPU.scala 169:25 170:13 172:13]
    node _GEN_7 = mux(io_b_readingin, io_b_in_1_3, b_reg_1_3) @[TPU.scala 169:25 170:13 172:13]
    node _cmp_input_0_0_T = mul(io_a_in_0, b_reg_0_0) @[TPU.scala 182:60]
    node _cmp_input_0_1_T = mul(a_reg_0_0, b_reg_0_1) @[TPU.scala 185:66]
    node _cmp_input_0_2_T = mul(a_reg_0_1, b_reg_0_2) @[TPU.scala 185:66]
    node _cmp_input_0_3_T = mul(a_reg_0_2, b_reg_0_3) @[TPU.scala 185:66]
    node _cmp_input_1_0_T = mul(io_a_in_1, b_reg_1_0) @[TPU.scala 190:61]
    node _cmp_input_1_0_T_1 = add(_cmp_input_1_0_T, cms_reg_0_0) @[TPU.scala 190:82]
    node _cmp_input_1_0_T_2 = tail(_cmp_input_1_0_T_1, 1) @[TPU.scala 190:82]
    node _cmp_input_1_0_T_3 = asSInt(_cmp_input_1_0_T_2) @[TPU.scala 190:82]
    node _cmp_input_1_1_T = mul(a_reg_1_0, b_reg_1_1) @[TPU.scala 193:67]
    node _cmp_input_1_1_T_1 = add(_cmp_input_1_1_T, cms_reg_0_1) @[TPU.scala 193:88]
    node _cmp_input_1_1_T_2 = tail(_cmp_input_1_1_T_1, 1) @[TPU.scala 193:88]
    node _cmp_input_1_1_T_3 = asSInt(_cmp_input_1_1_T_2) @[TPU.scala 193:88]
    node _cmp_input_1_2_T = mul(a_reg_1_1, b_reg_1_2) @[TPU.scala 193:67]
    node _cmp_input_1_2_T_1 = add(_cmp_input_1_2_T, cms_reg_0_2) @[TPU.scala 193:88]
    node _cmp_input_1_2_T_2 = tail(_cmp_input_1_2_T_1, 1) @[TPU.scala 193:88]
    node _cmp_input_1_2_T_3 = asSInt(_cmp_input_1_2_T_2) @[TPU.scala 193:88]
    node _cmp_input_1_3_T = mul(a_reg_1_2, b_reg_1_3) @[TPU.scala 193:67]
    node _cmp_input_1_3_T_1 = add(_cmp_input_1_3_T, cms_reg_0_3) @[TPU.scala 193:88]
    node _cmp_input_1_3_T_2 = tail(_cmp_input_1_3_T_1, 1) @[TPU.scala 193:88]
    node _cmp_input_1_3_T_3 = asSInt(_cmp_input_1_3_T_2) @[TPU.scala 193:88]
    node cmp_input_0_0 = asSInt(bits(_cmp_input_0_0_T, 31, 0)) @[TPU.scala 177:25 182:43]
    node cmp_input_0_1 = asSInt(bits(_cmp_input_0_1_T, 31, 0)) @[TPU.scala 177:25 185:43]
    node cmp_input_0_2 = asSInt(bits(_cmp_input_0_2_T, 31, 0)) @[TPU.scala 177:25 185:43]
    node cmp_input_0_3 = asSInt(bits(_cmp_input_0_3_T, 31, 0)) @[TPU.scala 177:25 185:43]
    node cmp_input_1_0 = asSInt(bits(_cmp_input_1_0_T_3, 31, 0)) @[TPU.scala 177:25 190:43]
    node cmp_input_1_1 = asSInt(bits(_cmp_input_1_1_T_3, 31, 0)) @[TPU.scala 177:25 193:43]
    node cmp_input_1_2 = asSInt(bits(_cmp_input_1_2_T_3, 31, 0)) @[TPU.scala 177:25 193:43]
    node cmp_input_1_3 = asSInt(bits(_cmp_input_1_3_T_3, 31, 0)) @[TPU.scala 177:25 193:43]
    io_out_0 <= cms_reg_1_0 @[TPU.scala 175:19]
    io_out_1 <= cms_reg_1_1 @[TPU.scala 175:19]
    io_out_2 <= cms_reg_1_2 @[TPU.scala 175:19]
    io_out_3 <= cms_reg_1_3 @[TPU.scala 175:19]
    io_cmp_debug_0_0 <= cms_reg_0_0 @[TPU.scala 165:18]
    io_cmp_debug_0_1 <= cms_reg_0_1 @[TPU.scala 165:18]
    io_cmp_debug_0_2 <= cms_reg_0_2 @[TPU.scala 165:18]
    io_cmp_debug_0_3 <= cms_reg_0_3 @[TPU.scala 165:18]
    io_cmp_debug_1_0 <= cms_reg_1_0 @[TPU.scala 165:18]
    io_cmp_debug_1_1 <= cms_reg_1_1 @[TPU.scala 165:18]
    io_cmp_debug_1_2 <= cms_reg_1_2 @[TPU.scala 165:18]
    io_cmp_debug_1_3 <= cms_reg_1_3 @[TPU.scala 165:18]
    io_debug_b_regs_0_0 <= b_reg_0_0 @[TPU.scala 166:21]
    io_debug_b_regs_0_1 <= b_reg_0_1 @[TPU.scala 166:21]
    io_debug_b_regs_0_2 <= b_reg_0_2 @[TPU.scala 166:21]
    io_debug_b_regs_0_3 <= b_reg_0_3 @[TPU.scala 166:21]
    io_debug_b_regs_1_0 <= b_reg_1_0 @[TPU.scala 166:21]
    io_debug_b_regs_1_1 <= b_reg_1_1 @[TPU.scala 166:21]
    io_debug_b_regs_1_2 <= b_reg_1_2 @[TPU.scala 166:21]
    io_debug_b_regs_1_3 <= b_reg_1_3 @[TPU.scala 166:21]
    io_debug_a_regs_0_0 <= a_reg_0_0 @[TPU.scala 167:21]
    io_debug_a_regs_0_1 <= a_reg_0_1 @[TPU.scala 167:21]
    io_debug_a_regs_0_2 <= a_reg_0_2 @[TPU.scala 167:21]
    io_debug_a_regs_0_3 <= a_reg_0_3 @[TPU.scala 167:21]
    io_debug_a_regs_1_0 <= a_reg_1_0 @[TPU.scala 167:21]
    io_debug_a_regs_1_1 <= a_reg_1_1 @[TPU.scala 167:21]
    io_debug_a_regs_1_2 <= a_reg_1_2 @[TPU.scala 167:21]
    io_debug_a_regs_1_3 <= a_reg_1_3 @[TPU.scala 167:21]
    io_debug_00 <= asSInt(bits(_io_debug_00_T, 31, 0)) @[TPU.scala 168:17]
    a_reg_0_0 <= io_a_in_0 @[TPU.scala 183:39]
    a_reg_0_1 <= a_reg_0_0 @[TPU.scala 186:39]
    a_reg_0_2 <= a_reg_0_1 @[TPU.scala 186:39]
    a_reg_0_3 <= a_reg_0_2 @[TPU.scala 186:39]
    a_reg_1_0 <= io_a_in_1 @[TPU.scala 191:39]
    a_reg_1_1 <= a_reg_1_0 @[TPU.scala 194:39]
    a_reg_1_2 <= a_reg_1_1 @[TPU.scala 194:39]
    a_reg_1_3 <= a_reg_1_2 @[TPU.scala 194:39]
    b_reg_0_0 <= _GEN_0
    b_reg_0_1 <= _GEN_1
    b_reg_0_2 <= _GEN_2
    b_reg_0_3 <= _GEN_3
    b_reg_1_0 <= _GEN_4
    b_reg_1_1 <= _GEN_5
    b_reg_1_2 <= _GEN_6
    b_reg_1_3 <= _GEN_7
    cms_reg_0_0 <= cmp_input_0_0 @[TPU.scala 197:21]
    cms_reg_0_1 <= cmp_input_0_1 @[TPU.scala 197:21]
    cms_reg_0_2 <= cmp_input_0_2 @[TPU.scala 197:21]
    cms_reg_0_3 <= cmp_input_0_3 @[TPU.scala 197:21]
    cms_reg_1_0 <= cmp_input_1_0 @[TPU.scala 197:21]
    cms_reg_1_1 <= cmp_input_1_1 @[TPU.scala 197:21]
    cms_reg_1_2 <= cmp_input_1_2 @[TPU.scala 197:21]
    cms_reg_1_3 <= cmp_input_1_3 @[TPU.scala 197:21]

  module ChiselTPU :
    input clock : Clock
    input reset : UInt<1>
    output io_a_ready : UInt<1>
    input io_a_valid : UInt<1>
    input io_a_bits_0_0 : SInt<32>
    input io_a_bits_0_1 : SInt<32>
    input io_a_bits_1_0 : SInt<32>
    input io_a_bits_1_1 : SInt<32>
    input io_a_bits_2_0 : SInt<32>
    input io_a_bits_2_1 : SInt<32>
    input io_a_bits_3_0 : SInt<32>
    input io_a_bits_3_1 : SInt<32>
    output io_b_ready : UInt<1>
    input io_b_valid : UInt<1>
    input io_b_bits_0_0 : SInt<32>
    input io_b_bits_0_1 : SInt<32>
    input io_b_bits_0_2 : SInt<32>
    input io_b_bits_0_3 : SInt<32>
    input io_b_bits_1_0 : SInt<32>
    input io_b_bits_1_1 : SInt<32>
    input io_b_bits_1_2 : SInt<32>
    input io_b_bits_1_3 : SInt<32>
    output io_out_0_0 : SInt<32>
    output io_out_0_1 : SInt<32>
    output io_out_0_2 : SInt<32>
    output io_out_0_3 : SInt<32>
    output io_out_1_0 : SInt<32>
    output io_out_1_1 : SInt<32>
    output io_out_1_2 : SInt<32>
    output io_out_1_3 : SInt<32>
    output io_out_2_0 : SInt<32>
    output io_out_2_1 : SInt<32>
    output io_out_2_2 : SInt<32>
    output io_out_2_3 : SInt<32>
    output io_out_3_0 : SInt<32>
    output io_out_3_1 : SInt<32>
    output io_out_3_2 : SInt<32>
    output io_out_3_3 : SInt<32>
    output io_debug_1_0_0 : SInt<32>
    output io_debug_1_0_1 : SInt<32>
    output io_debug_1_0_2 : SInt<32>
    output io_debug_1_0_3 : SInt<32>
    output io_debug_1_1_0 : SInt<32>
    output io_debug_1_1_1 : SInt<32>
    output io_debug_1_1_2 : SInt<32>
    output io_debug_1_1_3 : SInt<32>
    output io_debug_a_out_0 : SInt<32>
    output io_debug_a_out_1 : SInt<32>
    output io_debug_a_regs_0_0 : SInt<32>
    output io_debug_a_regs_0_1 : SInt<32>
    output io_debug_a_regs_0_2 : SInt<32>
    output io_debug_a_regs_0_3 : SInt<32>
    output io_debug_a_regs_1_0 : SInt<32>
    output io_debug_a_regs_1_1 : SInt<32>
    output io_debug_a_regs_1_2 : SInt<32>
    output io_debug_a_regs_1_3 : SInt<32>
    output io_debug_b_regs_0_0 : SInt<32>
    output io_debug_b_regs_0_1 : SInt<32>
    output io_debug_b_regs_0_2 : SInt<32>
    output io_debug_b_regs_0_3 : SInt<32>
    output io_debug_b_regs_1_0 : SInt<32>
    output io_debug_b_regs_1_1 : SInt<32>
    output io_debug_b_regs_1_2 : SInt<32>
    output io_debug_b_regs_1_3 : SInt<32>
    output io_debug_cycleOut : UInt<32>
    output io_debug_00 : SInt<32>

    inst actReg of ActReg @[TPU.scala 35:22]
    inst systArr of SystArr @[TPU.scala 37:23]
    reg state : UInt<2>, clock with :
      reset => (UInt<1>("h0"), state) @[TPU.scala 31:22]
    reg cycle : UInt<4>, clock with :
      reset => (UInt<1>("h0"), cycle) @[Counter.scala 61:40]
    node wrap_wrap = eq(cycle, UInt<4>("h8")) @[Counter.scala 73:24]
    node _wrap_value_T = add(cycle, UInt<1>("h1")) @[Counter.scala 77:24]
    node _wrap_value_T_1 = tail(_wrap_value_T, 1) @[Counter.scala 77:24]
    node _GEN_0 = mux(wrap_wrap, UInt<1>("h0"), _wrap_value_T_1) @[Counter.scala 77:15 87:{20,28}]
    node _T = eq(state, UInt<2>("h0")) @[TPU.scala 65:14]
    node _T_2 = eq(state, UInt<2>("h1")) @[TPU.scala 73:19]
    node _T_3 = and(io_b_valid, io_b_ready) @[TPU.scala 74:23]
    node _GEN_22 = mux(_T_3, UInt<1>("h1"), UInt<1>("h0")) @[TPU.scala 53:26 74:37 78:32]
    node _T_4 = eq(state, UInt<2>("h2")) @[TPU.scala 86:19]
    node _GEN_59 = mux(_T_4, UInt<1>("h1"), UInt<1>("h0")) @[TPU.scala 46:15 86:32 90:17]
    node _GEN_89 = mux(_T_2, _GEN_22, _GEN_59) @[TPU.scala 73:28]
    node _GEN_127 = mux(_T, UInt<1>("h0"), _GEN_89) @[TPU.scala 46:15 65:23]
    node counterFlag = _GEN_127 @[TPU.scala 32:25]
    node _GEN_1 = mux(counterFlag, _GEN_0, cycle) @[Counter.scala 118:16 61:40]
    node _GEN_2 = mux(counterFlag, wrap_wrap, UInt<1>("h0")) @[Counter.scala 118:{16,23}]
    reg myOut_0_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), myOut_0_0) @[TPU.scala 38:22]
    reg myOut_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), myOut_0_1) @[TPU.scala 38:22]
    reg myOut_0_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), myOut_0_2) @[TPU.scala 38:22]
    reg myOut_0_3 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), myOut_0_3) @[TPU.scala 38:22]
    reg myOut_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), myOut_1_0) @[TPU.scala 38:22]
    reg myOut_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), myOut_1_1) @[TPU.scala 38:22]
    reg myOut_1_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), myOut_1_2) @[TPU.scala 38:22]
    reg myOut_1_3 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), myOut_1_3) @[TPU.scala 38:22]
    reg myOut_2_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), myOut_2_0) @[TPU.scala 38:22]
    reg myOut_2_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), myOut_2_1) @[TPU.scala 38:22]
    reg myOut_2_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), myOut_2_2) @[TPU.scala 38:22]
    reg myOut_2_3 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), myOut_2_3) @[TPU.scala 38:22]
    reg myOut_3_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), myOut_3_0) @[TPU.scala 38:22]
    reg myOut_3_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), myOut_3_1) @[TPU.scala 38:22]
    reg myOut_3_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), myOut_3_2) @[TPU.scala 38:22]
    reg myOut_3_3 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), myOut_3_3) @[TPU.scala 38:22]
    reg a_ready : UInt<1>, clock with :
      reset => (UInt<1>("h0"), a_ready) @[TPU.scala 39:24]
    reg b_ready : UInt<1>, clock with :
      reset => (UInt<1>("h0"), b_ready) @[TPU.scala 40:24]
    node _actReg_io_index_T = sub(UInt<3>("h5"), cycle) @[TPU.scala 48:35]
    node _actReg_io_index_T_1 = tail(_actReg_io_index_T, 1) @[TPU.scala 48:35]
    reg act_in_0_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), act_in_0_0) @[TPU.scala 51:23]
    reg act_in_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), act_in_0_1) @[TPU.scala 51:23]
    reg act_in_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), act_in_1_0) @[TPU.scala 51:23]
    reg act_in_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), act_in_1_1) @[TPU.scala 51:23]
    reg act_in_2_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), act_in_2_0) @[TPU.scala 51:23]
    reg act_in_2_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), act_in_2_1) @[TPU.scala 51:23]
    reg act_in_3_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), act_in_3_0) @[TPU.scala 51:23]
    reg act_in_3_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), act_in_3_1) @[TPU.scala 51:23]
    node _T_1 = and(io_a_valid, io_a_ready) @[TPU.scala 66:23]
    node _GEN_3 = mux(_T_1, UInt<2>("h1"), state) @[TPU.scala 66:37 67:15 31:22]
    node _GEN_4 = mux(_T_1, io_a_bits_0_0, act_in_0_0) @[TPU.scala 66:37 68:16 51:23]
    node _GEN_5 = mux(_T_1, io_a_bits_0_1, act_in_0_1) @[TPU.scala 66:37 68:16 51:23]
    node _GEN_6 = mux(_T_1, io_a_bits_1_0, act_in_1_0) @[TPU.scala 66:37 68:16 51:23]
    node _GEN_7 = mux(_T_1, io_a_bits_1_1, act_in_1_1) @[TPU.scala 66:37 68:16 51:23]
    node _GEN_8 = mux(_T_1, io_a_bits_2_0, act_in_2_0) @[TPU.scala 66:37 68:16 51:23]
    node _GEN_9 = mux(_T_1, io_a_bits_2_1, act_in_2_1) @[TPU.scala 66:37 68:16 51:23]
    node _GEN_10 = mux(_T_1, io_a_bits_3_0, act_in_3_0) @[TPU.scala 66:37 68:16 51:23]
    node _GEN_11 = mux(_T_1, io_a_bits_3_1, act_in_3_1) @[TPU.scala 66:37 68:16 51:23]
    node _GEN_12 = mux(_T_1, UInt<1>("h0"), a_ready) @[TPU.scala 66:37 69:17 39:24]
    node _GEN_13 = mux(_T_3, UInt<2>("h2"), state) @[TPU.scala 74:37 75:15 31:22]
    node _GEN_14 = mux(_T_3, io_b_bits_0_0, io_b_bits_0_0) @[TPU.scala 56:19 74:37 77:25]
    node _GEN_15 = mux(_T_3, io_b_bits_0_1, io_b_bits_0_1) @[TPU.scala 56:19 74:37 77:25]
    node _GEN_16 = mux(_T_3, io_b_bits_0_2, io_b_bits_0_2) @[TPU.scala 56:19 74:37 77:25]
    node _GEN_17 = mux(_T_3, io_b_bits_0_3, io_b_bits_0_3) @[TPU.scala 56:19 74:37 77:25]
    node _GEN_18 = mux(_T_3, io_b_bits_1_0, io_b_bits_1_0) @[TPU.scala 56:19 74:37 77:25]
    node _GEN_19 = mux(_T_3, io_b_bits_1_1, io_b_bits_1_1) @[TPU.scala 56:19 74:37 77:25]
    node _GEN_20 = mux(_T_3, io_b_bits_1_2, io_b_bits_1_2) @[TPU.scala 56:19 74:37 77:25]
    node _GEN_21 = mux(_T_3, io_b_bits_1_3, io_b_bits_1_3) @[TPU.scala 56:19 74:37 77:25]
    node _GEN_23 = mux(_T_3, UInt<1>("h0"), b_ready) @[TPU.scala 74:37 79:17 40:24]
    node _T_5 = eq(cycle, UInt<4>("h8")) @[TPU.scala 91:16]
    node _GEN_24 = mux(_T_5, UInt<2>("h3"), state) @[TPU.scala 91:36 92:13 31:22]
    node _T_6 = sub(cycle, UInt<3>("h5")) @[TPU.scala 96:21]
    node _T_7 = tail(_T_6, 1) @[TPU.scala 96:21]
    node _T_8 = bits(_T_7, 1, 0)
    node _myOut_T_8_0 = systArr.io_out_0 @[TPU.scala 96:{41,41}]
    node _GEN_25 = mux(eq(UInt<1>("h0"), _T_8), _myOut_T_8_0, myOut_0_0) @[TPU.scala 38:22 96:{41,41}]
    node _GEN_26 = mux(eq(UInt<1>("h1"), _T_8), _myOut_T_8_0, myOut_1_0) @[TPU.scala 38:22 96:{41,41}]
    node _GEN_27 = mux(eq(UInt<2>("h2"), _T_8), _myOut_T_8_0, myOut_2_0) @[TPU.scala 38:22 96:{41,41}]
    node _GEN_28 = mux(eq(UInt<2>("h3"), _T_8), _myOut_T_8_0, myOut_3_0) @[TPU.scala 38:22 96:{41,41}]
    node _T_9 = sub(cycle, UInt<3>("h6")) @[TPU.scala 96:21]
    node _T_10 = tail(_T_9, 1) @[TPU.scala 96:21]
    node _T_11 = bits(_T_10, 1, 0)
    node _myOut_T_11_1 = systArr.io_out_1 @[TPU.scala 96:{41,41}]
    node _GEN_29 = mux(eq(UInt<1>("h0"), _T_11), _myOut_T_11_1, myOut_0_1) @[TPU.scala 38:22 96:{41,41}]
    node _GEN_30 = mux(eq(UInt<1>("h1"), _T_11), _myOut_T_11_1, myOut_1_1) @[TPU.scala 38:22 96:{41,41}]
    node _GEN_31 = mux(eq(UInt<2>("h2"), _T_11), _myOut_T_11_1, myOut_2_1) @[TPU.scala 38:22 96:{41,41}]
    node _GEN_32 = mux(eq(UInt<2>("h3"), _T_11), _myOut_T_11_1, myOut_3_1) @[TPU.scala 38:22 96:{41,41}]
    node _T_12 = sub(cycle, UInt<3>("h7")) @[TPU.scala 96:21]
    node _T_13 = tail(_T_12, 1) @[TPU.scala 96:21]
    node _T_14 = bits(_T_13, 1, 0)
    node _myOut_T_14_2 = systArr.io_out_2 @[TPU.scala 96:{41,41}]
    node _GEN_33 = mux(eq(UInt<1>("h0"), _T_14), _myOut_T_14_2, myOut_0_2) @[TPU.scala 38:22 96:{41,41}]
    node _GEN_34 = mux(eq(UInt<1>("h1"), _T_14), _myOut_T_14_2, myOut_1_2) @[TPU.scala 38:22 96:{41,41}]
    node _GEN_35 = mux(eq(UInt<2>("h2"), _T_14), _myOut_T_14_2, myOut_2_2) @[TPU.scala 38:22 96:{41,41}]
    node _GEN_36 = mux(eq(UInt<2>("h3"), _T_14), _myOut_T_14_2, myOut_3_2) @[TPU.scala 38:22 96:{41,41}]
    node _T_15 = sub(cycle, UInt<4>("h8")) @[TPU.scala 96:21]
    node _T_16 = tail(_T_15, 1) @[TPU.scala 96:21]
    node _T_17 = bits(_T_16, 1, 0)
    node _myOut_T_17_3 = systArr.io_out_3 @[TPU.scala 96:{41,41}]
    node _GEN_37 = mux(eq(UInt<1>("h0"), _T_17), _myOut_T_17_3, myOut_0_3) @[TPU.scala 38:22 96:{41,41}]
    node _GEN_38 = mux(eq(UInt<1>("h1"), _T_17), _myOut_T_17_3, myOut_1_3) @[TPU.scala 38:22 96:{41,41}]
    node _GEN_39 = mux(eq(UInt<2>("h2"), _T_17), _myOut_T_17_3, myOut_2_3) @[TPU.scala 38:22 96:{41,41}]
    node _GEN_40 = mux(eq(UInt<2>("h3"), _T_17), _myOut_T_17_3, myOut_3_3) @[TPU.scala 38:22 96:{41,41}]
    node _T_18 = eq(state, UInt<2>("h3")) @[TPU.scala 118:19]
    node _T_19 = bits(reset, 0, 0) @[TPU.scala 119:11]
    node _T_20 = eq(_T_19, UInt<1>("h0")) @[TPU.scala 119:11]
    node _GEN_41 = mux(_T_18, UInt<2>("h1"), state) @[TPU.scala 118:29 120:11 31:22]
    node _GEN_42 = mux(_T_18, UInt<1>("h1"), b_ready) @[TPU.scala 118:29 121:13 40:24]
    node _WIRE_0_0 = asSInt(UInt<32>("h11")) @[TPU.scala 122:{36,36}]
    node _GEN_43 = mux(_T_18, _WIRE_0_0, myOut_0_0) @[TPU.scala 118:29 122:11 38:22]
    node _WIRE_0_1 = asSInt(UInt<32>("h11")) @[TPU.scala 122:{36,36}]
    node _GEN_44 = mux(_T_18, _WIRE_0_1, myOut_0_1) @[TPU.scala 118:29 122:11 38:22]
    node _WIRE_0_2 = asSInt(UInt<32>("h11")) @[TPU.scala 122:{36,36}]
    node _GEN_45 = mux(_T_18, _WIRE_0_2, myOut_0_2) @[TPU.scala 118:29 122:11 38:22]
    node _WIRE_0_3 = asSInt(UInt<32>("h11")) @[TPU.scala 122:{36,36}]
    node _GEN_46 = mux(_T_18, _WIRE_0_3, myOut_0_3) @[TPU.scala 118:29 122:11 38:22]
    node _WIRE_1_0 = asSInt(UInt<32>("h11")) @[TPU.scala 122:{36,36}]
    node _GEN_47 = mux(_T_18, _WIRE_1_0, myOut_1_0) @[TPU.scala 118:29 122:11 38:22]
    node _WIRE_1_1 = asSInt(UInt<32>("h11")) @[TPU.scala 122:{36,36}]
    node _GEN_48 = mux(_T_18, _WIRE_1_1, myOut_1_1) @[TPU.scala 118:29 122:11 38:22]
    node _WIRE_1_2 = asSInt(UInt<32>("h11")) @[TPU.scala 122:{36,36}]
    node _GEN_49 = mux(_T_18, _WIRE_1_2, myOut_1_2) @[TPU.scala 118:29 122:11 38:22]
    node _WIRE_1_3 = asSInt(UInt<32>("h11")) @[TPU.scala 122:{36,36}]
    node _GEN_50 = mux(_T_18, _WIRE_1_3, myOut_1_3) @[TPU.scala 118:29 122:11 38:22]
    node _WIRE_2_0 = asSInt(UInt<32>("h11")) @[TPU.scala 122:{36,36}]
    node _GEN_51 = mux(_T_18, _WIRE_2_0, myOut_2_0) @[TPU.scala 118:29 122:11 38:22]
    node _WIRE_2_1 = asSInt(UInt<32>("h11")) @[TPU.scala 122:{36,36}]
    node _GEN_52 = mux(_T_18, _WIRE_2_1, myOut_2_1) @[TPU.scala 118:29 122:11 38:22]
    node _WIRE_2_2 = asSInt(UInt<32>("h11")) @[TPU.scala 122:{36,36}]
    node _GEN_53 = mux(_T_18, _WIRE_2_2, myOut_2_2) @[TPU.scala 118:29 122:11 38:22]
    node _WIRE_2_3 = asSInt(UInt<32>("h11")) @[TPU.scala 122:{36,36}]
    node _GEN_54 = mux(_T_18, _WIRE_2_3, myOut_2_3) @[TPU.scala 118:29 122:11 38:22]
    node _WIRE_3_0 = asSInt(UInt<32>("h11")) @[TPU.scala 122:{36,36}]
    node _GEN_55 = mux(_T_18, _WIRE_3_0, myOut_3_0) @[TPU.scala 118:29 122:11 38:22]
    node _WIRE_3_1 = asSInt(UInt<32>("h11")) @[TPU.scala 122:{36,36}]
    node _GEN_56 = mux(_T_18, _WIRE_3_1, myOut_3_1) @[TPU.scala 118:29 122:11 38:22]
    node _WIRE_3_2 = asSInt(UInt<32>("h11")) @[TPU.scala 122:{36,36}]
    node _GEN_57 = mux(_T_18, _WIRE_3_2, myOut_3_2) @[TPU.scala 118:29 122:11 38:22]
    node _WIRE_3_3 = asSInt(UInt<32>("h11")) @[TPU.scala 122:{36,36}]
    node _GEN_58 = mux(_T_18, _WIRE_3_3, myOut_3_3) @[TPU.scala 118:29 122:11 38:22]
    node _GEN_60 = mux(_T_4, _GEN_24, _GEN_41) @[TPU.scala 86:32]
    node _GEN_61 = mux(_T_4, _GEN_25, _GEN_43) @[TPU.scala 86:32]
    node _GEN_62 = mux(_T_4, _GEN_26, _GEN_47) @[TPU.scala 86:32]
    node _GEN_63 = mux(_T_4, _GEN_27, _GEN_51) @[TPU.scala 86:32]
    node _GEN_64 = mux(_T_4, _GEN_28, _GEN_55) @[TPU.scala 86:32]
    node _GEN_65 = mux(_T_4, _GEN_29, _GEN_44) @[TPU.scala 86:32]
    node _GEN_66 = mux(_T_4, _GEN_30, _GEN_48) @[TPU.scala 86:32]
    node _GEN_67 = mux(_T_4, _GEN_31, _GEN_52) @[TPU.scala 86:32]
    node _GEN_68 = mux(_T_4, _GEN_32, _GEN_56) @[TPU.scala 86:32]
    node _GEN_69 = mux(_T_4, _GEN_33, _GEN_45) @[TPU.scala 86:32]
    node _GEN_70 = mux(_T_4, _GEN_34, _GEN_49) @[TPU.scala 86:32]
    node _GEN_71 = mux(_T_4, _GEN_35, _GEN_53) @[TPU.scala 86:32]
    node _GEN_72 = mux(_T_4, _GEN_36, _GEN_57) @[TPU.scala 86:32]
    node _GEN_73 = mux(_T_4, _GEN_37, _GEN_46) @[TPU.scala 86:32]
    node _GEN_74 = mux(_T_4, _GEN_38, _GEN_50) @[TPU.scala 86:32]
    node _GEN_75 = mux(_T_4, _GEN_39, _GEN_54) @[TPU.scala 86:32]
    node _GEN_76 = mux(_T_4, _GEN_40, _GEN_58) @[TPU.scala 86:32]
    node _GEN_77 = mux(_T_4, b_ready, _GEN_42) @[TPU.scala 40:24 86:32]
    node _GEN_78 = mux(_T_2, _GEN_13, _GEN_60) @[TPU.scala 73:28]
    node _GEN_79 = mux(_T_2, _GEN_14, io_b_bits_0_0) @[TPU.scala 56:19 73:28]
    node _GEN_80 = mux(_T_2, _GEN_15, io_b_bits_0_1) @[TPU.scala 56:19 73:28]
    node _GEN_81 = mux(_T_2, _GEN_16, io_b_bits_0_2) @[TPU.scala 56:19 73:28]
    node _GEN_82 = mux(_T_2, _GEN_17, io_b_bits_0_3) @[TPU.scala 56:19 73:28]
    node _GEN_83 = mux(_T_2, _GEN_18, io_b_bits_1_0) @[TPU.scala 56:19 73:28]
    node _GEN_84 = mux(_T_2, _GEN_19, io_b_bits_1_1) @[TPU.scala 56:19 73:28]
    node _GEN_85 = mux(_T_2, _GEN_20, io_b_bits_1_2) @[TPU.scala 56:19 73:28]
    node _GEN_86 = mux(_T_2, _GEN_21, io_b_bits_1_3) @[TPU.scala 56:19 73:28]
    node _GEN_87 = mux(_T_2, _GEN_22, UInt<1>("h0")) @[TPU.scala 53:26 73:28]
    node _GEN_88 = mux(_T_2, _GEN_23, _GEN_77) @[TPU.scala 73:28]
    node _GEN_90 = mux(_T_2, myOut_0_0, _GEN_61) @[TPU.scala 38:22 73:28]
    node _GEN_91 = mux(_T_2, myOut_1_0, _GEN_62) @[TPU.scala 38:22 73:28]
    node _GEN_92 = mux(_T_2, myOut_2_0, _GEN_63) @[TPU.scala 38:22 73:28]
    node _GEN_93 = mux(_T_2, myOut_3_0, _GEN_64) @[TPU.scala 38:22 73:28]
    node _GEN_94 = mux(_T_2, myOut_0_1, _GEN_65) @[TPU.scala 38:22 73:28]
    node _GEN_95 = mux(_T_2, myOut_1_1, _GEN_66) @[TPU.scala 38:22 73:28]
    node _GEN_96 = mux(_T_2, myOut_2_1, _GEN_67) @[TPU.scala 38:22 73:28]
    node _GEN_97 = mux(_T_2, myOut_3_1, _GEN_68) @[TPU.scala 38:22 73:28]
    node _GEN_98 = mux(_T_2, myOut_0_2, _GEN_69) @[TPU.scala 38:22 73:28]
    node _GEN_99 = mux(_T_2, myOut_1_2, _GEN_70) @[TPU.scala 38:22 73:28]
    node _GEN_100 = mux(_T_2, myOut_2_2, _GEN_71) @[TPU.scala 38:22 73:28]
    node _GEN_101 = mux(_T_2, myOut_3_2, _GEN_72) @[TPU.scala 38:22 73:28]
    node _GEN_102 = mux(_T_2, myOut_0_3, _GEN_73) @[TPU.scala 38:22 73:28]
    node _GEN_103 = mux(_T_2, myOut_1_3, _GEN_74) @[TPU.scala 38:22 73:28]
    node _GEN_104 = mux(_T_2, myOut_2_3, _GEN_75) @[TPU.scala 38:22 73:28]
    node _GEN_105 = mux(_T_2, myOut_3_3, _GEN_76) @[TPU.scala 38:22 73:28]
    node _GEN_106 = mux(_T, _GEN_3, _GEN_78) @[TPU.scala 65:23]
    node _GEN_107 = mux(_T, _GEN_4, act_in_0_0) @[TPU.scala 51:23 65:23]
    node _GEN_108 = mux(_T, _GEN_5, act_in_0_1) @[TPU.scala 51:23 65:23]
    node _GEN_109 = mux(_T, _GEN_6, act_in_1_0) @[TPU.scala 51:23 65:23]
    node _GEN_110 = mux(_T, _GEN_7, act_in_1_1) @[TPU.scala 51:23 65:23]
    node _GEN_111 = mux(_T, _GEN_8, act_in_2_0) @[TPU.scala 51:23 65:23]
    node _GEN_112 = mux(_T, _GEN_9, act_in_2_1) @[TPU.scala 51:23 65:23]
    node _GEN_113 = mux(_T, _GEN_10, act_in_3_0) @[TPU.scala 51:23 65:23]
    node _GEN_114 = mux(_T, _GEN_11, act_in_3_1) @[TPU.scala 51:23 65:23]
    node _GEN_115 = mux(_T, _GEN_12, a_ready) @[TPU.scala 65:23 39:24]
    node _GEN_116 = mux(_T, UInt<1>("h0"), _GEN_1) @[TPU.scala 65:23 71:13]
    node _GEN_117 = mux(_T, io_b_bits_0_0, _GEN_79) @[TPU.scala 56:19 65:23]
    node _GEN_118 = mux(_T, io_b_bits_0_1, _GEN_80) @[TPU.scala 56:19 65:23]
    node _GEN_119 = mux(_T, io_b_bits_0_2, _GEN_81) @[TPU.scala 56:19 65:23]
    node _GEN_120 = mux(_T, io_b_bits_0_3, _GEN_82) @[TPU.scala 56:19 65:23]
    node _GEN_121 = mux(_T, io_b_bits_1_0, _GEN_83) @[TPU.scala 56:19 65:23]
    node _GEN_122 = mux(_T, io_b_bits_1_1, _GEN_84) @[TPU.scala 56:19 65:23]
    node _GEN_123 = mux(_T, io_b_bits_1_2, _GEN_85) @[TPU.scala 56:19 65:23]
    node _GEN_124 = mux(_T, io_b_bits_1_3, _GEN_86) @[TPU.scala 56:19 65:23]
    node _GEN_125 = mux(_T, UInt<1>("h0"), _GEN_87) @[TPU.scala 65:23 53:26]
    node _GEN_126 = mux(_T, b_ready, _GEN_88) @[TPU.scala 65:23 40:24]
    node _GEN_128 = mux(_T, myOut_0_0, _GEN_90) @[TPU.scala 38:22 65:23]
    node _GEN_129 = mux(_T, myOut_1_0, _GEN_91) @[TPU.scala 38:22 65:23]
    node _GEN_130 = mux(_T, myOut_2_0, _GEN_92) @[TPU.scala 38:22 65:23]
    node _GEN_131 = mux(_T, myOut_3_0, _GEN_93) @[TPU.scala 38:22 65:23]
    node _GEN_132 = mux(_T, myOut_0_1, _GEN_94) @[TPU.scala 38:22 65:23]
    node _GEN_133 = mux(_T, myOut_1_1, _GEN_95) @[TPU.scala 38:22 65:23]
    node _GEN_134 = mux(_T, myOut_2_1, _GEN_96) @[TPU.scala 38:22 65:23]
    node _GEN_135 = mux(_T, myOut_3_1, _GEN_97) @[TPU.scala 38:22 65:23]
    node _GEN_136 = mux(_T, myOut_0_2, _GEN_98) @[TPU.scala 38:22 65:23]
    node _GEN_137 = mux(_T, myOut_1_2, _GEN_99) @[TPU.scala 38:22 65:23]
    node _GEN_138 = mux(_T, myOut_2_2, _GEN_100) @[TPU.scala 38:22 65:23]
    node _GEN_139 = mux(_T, myOut_3_2, _GEN_101) @[TPU.scala 38:22 65:23]
    node _GEN_140 = mux(_T, myOut_0_3, _GEN_102) @[TPU.scala 38:22 65:23]
    node _GEN_141 = mux(_T, myOut_1_3, _GEN_103) @[TPU.scala 38:22 65:23]
    node _GEN_142 = mux(_T, myOut_2_3, _GEN_104) @[TPU.scala 38:22 65:23]
    node _GEN_143 = mux(_T, myOut_3_3, _GEN_105) @[TPU.scala 38:22 65:23]
    node wrap = _GEN_2
    node _myOut_WIRE_0_0 = asSInt(UInt<32>("h0")) @[TPU.scala 38:{45,45}]
    node _myOut_WIRE_0_1 = asSInt(UInt<32>("h0")) @[TPU.scala 38:{45,45}]
    node _myOut_WIRE_0_2 = asSInt(UInt<32>("h0")) @[TPU.scala 38:{45,45}]
    node _myOut_WIRE_0_3 = asSInt(UInt<32>("h0")) @[TPU.scala 38:{45,45}]
    node _myOut_WIRE_1_0 = asSInt(UInt<32>("h0")) @[TPU.scala 38:{45,45}]
    node _myOut_WIRE_1_1 = asSInt(UInt<32>("h0")) @[TPU.scala 38:{45,45}]
    node _myOut_WIRE_1_2 = asSInt(UInt<32>("h0")) @[TPU.scala 38:{45,45}]
    node _myOut_WIRE_1_3 = asSInt(UInt<32>("h0")) @[TPU.scala 38:{45,45}]
    node _myOut_WIRE_2_0 = asSInt(UInt<32>("h0")) @[TPU.scala 38:{45,45}]
    node _myOut_WIRE_2_1 = asSInt(UInt<32>("h0")) @[TPU.scala 38:{45,45}]
    node _myOut_WIRE_2_2 = asSInt(UInt<32>("h0")) @[TPU.scala 38:{45,45}]
    node _myOut_WIRE_2_3 = asSInt(UInt<32>("h0")) @[TPU.scala 38:{45,45}]
    node _myOut_WIRE_3_0 = asSInt(UInt<32>("h0")) @[TPU.scala 38:{45,45}]
    node _myOut_WIRE_3_1 = asSInt(UInt<32>("h0")) @[TPU.scala 38:{45,45}]
    node _myOut_WIRE_3_2 = asSInt(UInt<32>("h0")) @[TPU.scala 38:{45,45}]
    node _myOut_WIRE_3_3 = asSInt(UInt<32>("h0")) @[TPU.scala 38:{45,45}]
    node _act_in_WIRE_0_0 = asSInt(UInt<32>("h0")) @[TPU.scala 51:{46,46}]
    node _act_in_WIRE_0_1 = asSInt(UInt<32>("h0")) @[TPU.scala 51:{46,46}]
    node _act_in_WIRE_1_0 = asSInt(UInt<32>("h0")) @[TPU.scala 51:{46,46}]
    node _act_in_WIRE_1_1 = asSInt(UInt<32>("h0")) @[TPU.scala 51:{46,46}]
    node _act_in_WIRE_2_0 = asSInt(UInt<32>("h0")) @[TPU.scala 51:{46,46}]
    node _act_in_WIRE_2_1 = asSInt(UInt<32>("h0")) @[TPU.scala 51:{46,46}]
    node _act_in_WIRE_3_0 = asSInt(UInt<32>("h0")) @[TPU.scala 51:{46,46}]
    node _act_in_WIRE_3_1 = asSInt(UInt<32>("h0")) @[TPU.scala 51:{46,46}]
    io_a_ready <= a_ready @[TPU.scala 42:14]
    io_b_ready <= b_ready @[TPU.scala 43:14]
    io_out_0_0 <= myOut_0_0 @[TPU.scala 44:10]
    io_out_0_1 <= myOut_0_1 @[TPU.scala 44:10]
    io_out_0_2 <= myOut_0_2 @[TPU.scala 44:10]
    io_out_0_3 <= myOut_0_3 @[TPU.scala 44:10]
    io_out_1_0 <= myOut_1_0 @[TPU.scala 44:10]
    io_out_1_1 <= myOut_1_1 @[TPU.scala 44:10]
    io_out_1_2 <= myOut_1_2 @[TPU.scala 44:10]
    io_out_1_3 <= myOut_1_3 @[TPU.scala 44:10]
    io_out_2_0 <= myOut_2_0 @[TPU.scala 44:10]
    io_out_2_1 <= myOut_2_1 @[TPU.scala 44:10]
    io_out_2_2 <= myOut_2_2 @[TPU.scala 44:10]
    io_out_2_3 <= myOut_2_3 @[TPU.scala 44:10]
    io_out_3_0 <= myOut_3_0 @[TPU.scala 44:10]
    io_out_3_1 <= myOut_3_1 @[TPU.scala 44:10]
    io_out_3_2 <= myOut_3_2 @[TPU.scala 44:10]
    io_out_3_3 <= myOut_3_3 @[TPU.scala 44:10]
    io_debug_1_0_0 <= systArr.io_cmp_debug_0_0 @[TPU.scala 59:14]
    io_debug_1_0_1 <= systArr.io_cmp_debug_0_1 @[TPU.scala 59:14]
    io_debug_1_0_2 <= systArr.io_cmp_debug_0_2 @[TPU.scala 59:14]
    io_debug_1_0_3 <= systArr.io_cmp_debug_0_3 @[TPU.scala 59:14]
    io_debug_1_1_0 <= systArr.io_cmp_debug_1_0 @[TPU.scala 59:14]
    io_debug_1_1_1 <= systArr.io_cmp_debug_1_1 @[TPU.scala 59:14]
    io_debug_1_1_2 <= systArr.io_cmp_debug_1_2 @[TPU.scala 59:14]
    io_debug_1_1_3 <= systArr.io_cmp_debug_1_3 @[TPU.scala 59:14]
    io_debug_a_out_0 <= actReg.io_a_out_0 @[TPU.scala 60:18]
    io_debug_a_out_1 <= actReg.io_a_out_1 @[TPU.scala 60:18]
    io_debug_a_regs_0_0 <= systArr.io_debug_a_regs_0_0 @[TPU.scala 61:19]
    io_debug_a_regs_0_1 <= systArr.io_debug_a_regs_0_1 @[TPU.scala 61:19]
    io_debug_a_regs_0_2 <= systArr.io_debug_a_regs_0_2 @[TPU.scala 61:19]
    io_debug_a_regs_0_3 <= systArr.io_debug_a_regs_0_3 @[TPU.scala 61:19]
    io_debug_a_regs_1_0 <= systArr.io_debug_a_regs_1_0 @[TPU.scala 61:19]
    io_debug_a_regs_1_1 <= systArr.io_debug_a_regs_1_1 @[TPU.scala 61:19]
    io_debug_a_regs_1_2 <= systArr.io_debug_a_regs_1_2 @[TPU.scala 61:19]
    io_debug_a_regs_1_3 <= systArr.io_debug_a_regs_1_3 @[TPU.scala 61:19]
    io_debug_b_regs_0_0 <= systArr.io_debug_b_regs_0_0 @[TPU.scala 62:19]
    io_debug_b_regs_0_1 <= systArr.io_debug_b_regs_0_1 @[TPU.scala 62:19]
    io_debug_b_regs_0_2 <= systArr.io_debug_b_regs_0_2 @[TPU.scala 62:19]
    io_debug_b_regs_0_3 <= systArr.io_debug_b_regs_0_3 @[TPU.scala 62:19]
    io_debug_b_regs_1_0 <= systArr.io_debug_b_regs_1_0 @[TPU.scala 62:19]
    io_debug_b_regs_1_1 <= systArr.io_debug_b_regs_1_1 @[TPU.scala 62:19]
    io_debug_b_regs_1_2 <= systArr.io_debug_b_regs_1_2 @[TPU.scala 62:19]
    io_debug_b_regs_1_3 <= systArr.io_debug_b_regs_1_3 @[TPU.scala 62:19]
    io_debug_cycleOut <= pad(cycle, 32) @[TPU.scala 63:21]
    io_debug_00 <= systArr.io_debug_00 @[TPU.scala 64:15]
    state <= mux(reset, UInt<2>("h0"), _GEN_106) @[TPU.scala 31:{22,22}]
    cycle <= mux(reset, UInt<4>("h0"), _GEN_116) @[Counter.scala 61:{40,40}]
    actReg.clock <= clock
    actReg.reset <= reset
    actReg.io_index <= _actReg_io_index_T_1 @[TPU.scala 48:19]
    actReg.io_a_0_0 <= act_in_0_0 @[TPU.scala 54:15]
    actReg.io_a_0_1 <= act_in_0_1 @[TPU.scala 54:15]
    actReg.io_a_1_0 <= act_in_1_0 @[TPU.scala 54:15]
    actReg.io_a_1_1 <= act_in_1_1 @[TPU.scala 54:15]
    actReg.io_a_2_0 <= act_in_2_0 @[TPU.scala 54:15]
    actReg.io_a_2_1 <= act_in_2_1 @[TPU.scala 54:15]
    actReg.io_a_3_0 <= act_in_3_0 @[TPU.scala 54:15]
    actReg.io_a_3_1 <= act_in_3_1 @[TPU.scala 54:15]
    systArr.clock <= clock
    systArr.reset <= reset
    systArr.io_a_in_0 <= actReg.io_a_out_0 @[TPU.scala 50:19]
    systArr.io_a_in_1 <= actReg.io_a_out_1 @[TPU.scala 50:19]
    systArr.io_b_in_0_0 <= _GEN_117
    systArr.io_b_in_0_1 <= _GEN_118
    systArr.io_b_in_0_2 <= _GEN_119
    systArr.io_b_in_0_3 <= _GEN_120
    systArr.io_b_in_1_0 <= _GEN_121
    systArr.io_b_in_1_1 <= _GEN_122
    systArr.io_b_in_1_2 <= _GEN_123
    systArr.io_b_in_1_3 <= _GEN_124
    systArr.io_b_readingin <= _GEN_125
    myOut_0_0 <= mux(reset, _myOut_WIRE_0_0, _GEN_128) @[TPU.scala 38:{22,22}]
    myOut_0_1 <= mux(reset, _myOut_WIRE_0_1, _GEN_132) @[TPU.scala 38:{22,22}]
    myOut_0_2 <= mux(reset, _myOut_WIRE_0_2, _GEN_136) @[TPU.scala 38:{22,22}]
    myOut_0_3 <= mux(reset, _myOut_WIRE_0_3, _GEN_140) @[TPU.scala 38:{22,22}]
    myOut_1_0 <= mux(reset, _myOut_WIRE_1_0, _GEN_129) @[TPU.scala 38:{22,22}]
    myOut_1_1 <= mux(reset, _myOut_WIRE_1_1, _GEN_133) @[TPU.scala 38:{22,22}]
    myOut_1_2 <= mux(reset, _myOut_WIRE_1_2, _GEN_137) @[TPU.scala 38:{22,22}]
    myOut_1_3 <= mux(reset, _myOut_WIRE_1_3, _GEN_141) @[TPU.scala 38:{22,22}]
    myOut_2_0 <= mux(reset, _myOut_WIRE_2_0, _GEN_130) @[TPU.scala 38:{22,22}]
    myOut_2_1 <= mux(reset, _myOut_WIRE_2_1, _GEN_134) @[TPU.scala 38:{22,22}]
    myOut_2_2 <= mux(reset, _myOut_WIRE_2_2, _GEN_138) @[TPU.scala 38:{22,22}]
    myOut_2_3 <= mux(reset, _myOut_WIRE_2_3, _GEN_142) @[TPU.scala 38:{22,22}]
    myOut_3_0 <= mux(reset, _myOut_WIRE_3_0, _GEN_131) @[TPU.scala 38:{22,22}]
    myOut_3_1 <= mux(reset, _myOut_WIRE_3_1, _GEN_135) @[TPU.scala 38:{22,22}]
    myOut_3_2 <= mux(reset, _myOut_WIRE_3_2, _GEN_139) @[TPU.scala 38:{22,22}]
    myOut_3_3 <= mux(reset, _myOut_WIRE_3_3, _GEN_143) @[TPU.scala 38:{22,22}]
    a_ready <= mux(reset, UInt<1>("h1"), _GEN_115) @[TPU.scala 39:{24,24}]
    b_ready <= mux(reset, UInt<1>("h1"), _GEN_126) @[TPU.scala 40:{24,24}]
    act_in_0_0 <= mux(reset, _act_in_WIRE_0_0, _GEN_107) @[TPU.scala 51:{23,23}]
    act_in_0_1 <= mux(reset, _act_in_WIRE_0_1, _GEN_108) @[TPU.scala 51:{23,23}]
    act_in_1_0 <= mux(reset, _act_in_WIRE_1_0, _GEN_109) @[TPU.scala 51:{23,23}]
    act_in_1_1 <= mux(reset, _act_in_WIRE_1_1, _GEN_110) @[TPU.scala 51:{23,23}]
    act_in_2_0 <= mux(reset, _act_in_WIRE_2_0, _GEN_111) @[TPU.scala 51:{23,23}]
    act_in_2_1 <= mux(reset, _act_in_WIRE_2_1, _GEN_112) @[TPU.scala 51:{23,23}]
    act_in_3_0 <= mux(reset, _act_in_WIRE_3_0, _GEN_113) @[TPU.scala 51:{23,23}]
    act_in_3_1 <= mux(reset, _act_in_WIRE_3_1, _GEN_114) @[TPU.scala 51:{23,23}]
    printf(clock, and(and(and(and(and(and(UInt<1>("h1"), eq(_T, UInt<1>("h0"))), eq(_T_2, UInt<1>("h0"))), eq(_T_4, UInt<1>("h0"))), _T_18), _T_20), UInt<1>("h1")), "FINISH\n") : printf @[TPU.scala 119:11]
