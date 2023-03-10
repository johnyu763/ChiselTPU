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
      reset => (UInt<1>("h0"), a_reg_0_0) @[TPU.scala 162:20]
    reg a_reg_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_0_1) @[TPU.scala 162:20]
    reg a_reg_0_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_0_2) @[TPU.scala 162:20]
    reg a_reg_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_1_0) @[TPU.scala 162:20]
    reg a_reg_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_1_1) @[TPU.scala 162:20]
    reg a_reg_1_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_1_2) @[TPU.scala 162:20]
    reg a_reg_2_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_2_0) @[TPU.scala 162:20]
    reg a_reg_2_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_2_1) @[TPU.scala 162:20]
    reg a_reg_2_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), a_reg_2_2) @[TPU.scala 162:20]
    reg b_reg_0_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_0_0) @[TPU.scala 163:20]
    reg b_reg_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_0_1) @[TPU.scala 163:20]
    reg b_reg_0_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_0_2) @[TPU.scala 163:20]
    reg b_reg_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_1_0) @[TPU.scala 163:20]
    reg b_reg_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_1_1) @[TPU.scala 163:20]
    reg b_reg_1_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_1_2) @[TPU.scala 163:20]
    reg b_reg_2_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_2_0) @[TPU.scala 163:20]
    reg b_reg_2_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_2_1) @[TPU.scala 163:20]
    reg b_reg_2_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), b_reg_2_2) @[TPU.scala 163:20]
    reg cms_reg_0_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_0_0) @[TPU.scala 164:22]
    reg cms_reg_0_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_0_1) @[TPU.scala 164:22]
    reg cms_reg_0_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_0_2) @[TPU.scala 164:22]
    reg cms_reg_1_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_1_0) @[TPU.scala 164:22]
    reg cms_reg_1_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_1_1) @[TPU.scala 164:22]
    reg cms_reg_1_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_1_2) @[TPU.scala 164:22]
    reg cms_reg_2_0 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_2_0) @[TPU.scala 164:22]
    reg cms_reg_2_1 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_2_1) @[TPU.scala 164:22]
    reg cms_reg_2_2 : SInt<32>, clock with :
      reset => (UInt<1>("h0"), cms_reg_2_2) @[TPU.scala 164:22]
    node _io_debug_00_T = mul(io_a_in_0, b_reg_0_0) @[TPU.scala 168:31]
    node _GEN_0 = mux(io_b_readingin, io_b_in_0_0, b_reg_0_0) @[TPU.scala 169:25 170:13 172:13]
    node _GEN_1 = mux(io_b_readingin, io_b_in_0_1, b_reg_0_1) @[TPU.scala 169:25 170:13 172:13]
    node _GEN_2 = mux(io_b_readingin, io_b_in_0_2, b_reg_0_2) @[TPU.scala 169:25 170:13 172:13]
    node _GEN_3 = mux(io_b_readingin, io_b_in_1_0, b_reg_1_0) @[TPU.scala 169:25 170:13 172:13]
    node _GEN_4 = mux(io_b_readingin, io_b_in_1_1, b_reg_1_1) @[TPU.scala 169:25 170:13 172:13]
    node _GEN_5 = mux(io_b_readingin, io_b_in_1_2, b_reg_1_2) @[TPU.scala 169:25 170:13 172:13]
    node _GEN_6 = mux(io_b_readingin, io_b_in_2_0, b_reg_2_0) @[TPU.scala 169:25 170:13 172:13]
    node _GEN_7 = mux(io_b_readingin, io_b_in_2_1, b_reg_2_1) @[TPU.scala 169:25 170:13 172:13]
    node _GEN_8 = mux(io_b_readingin, io_b_in_2_2, b_reg_2_2) @[TPU.scala 169:25 170:13 172:13]
    node _cmp_input_0_0_T = mul(io_a_in_0, b_reg_0_0) @[TPU.scala 182:60]
    node _cmp_input_0_1_T = mul(a_reg_0_0, b_reg_0_1) @[TPU.scala 185:66]
    node _cmp_input_0_2_T = mul(a_reg_0_1, b_reg_0_2) @[TPU.scala 185:66]
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
    node _cmp_input_2_0_T = mul(io_a_in_2, b_reg_2_0) @[TPU.scala 190:61]
    node _cmp_input_2_0_T_1 = add(_cmp_input_2_0_T, cms_reg_1_0) @[TPU.scala 190:82]
    node _cmp_input_2_0_T_2 = tail(_cmp_input_2_0_T_1, 1) @[TPU.scala 190:82]
    node _cmp_input_2_0_T_3 = asSInt(_cmp_input_2_0_T_2) @[TPU.scala 190:82]
    node _cmp_input_2_1_T = mul(a_reg_2_0, b_reg_2_1) @[TPU.scala 193:67]
    node _cmp_input_2_1_T_1 = add(_cmp_input_2_1_T, cms_reg_1_1) @[TPU.scala 193:88]
    node _cmp_input_2_1_T_2 = tail(_cmp_input_2_1_T_1, 1) @[TPU.scala 193:88]
    node _cmp_input_2_1_T_3 = asSInt(_cmp_input_2_1_T_2) @[TPU.scala 193:88]
    node _cmp_input_2_2_T = mul(a_reg_2_1, b_reg_2_2) @[TPU.scala 193:67]
    node _cmp_input_2_2_T_1 = add(_cmp_input_2_2_T, cms_reg_1_2) @[TPU.scala 193:88]
    node _cmp_input_2_2_T_2 = tail(_cmp_input_2_2_T_1, 1) @[TPU.scala 193:88]
    node _cmp_input_2_2_T_3 = asSInt(_cmp_input_2_2_T_2) @[TPU.scala 193:88]
    node cmp_input_0_0 = asSInt(bits(_cmp_input_0_0_T, 31, 0)) @[TPU.scala 177:25 182:43]
    node cmp_input_0_1 = asSInt(bits(_cmp_input_0_1_T, 31, 0)) @[TPU.scala 177:25 185:43]
    node cmp_input_0_2 = asSInt(bits(_cmp_input_0_2_T, 31, 0)) @[TPU.scala 177:25 185:43]
    node cmp_input_1_0 = asSInt(bits(_cmp_input_1_0_T_3, 31, 0)) @[TPU.scala 177:25 190:43]
    node cmp_input_1_1 = asSInt(bits(_cmp_input_1_1_T_3, 31, 0)) @[TPU.scala 177:25 193:43]
    node cmp_input_1_2 = asSInt(bits(_cmp_input_1_2_T_3, 31, 0)) @[TPU.scala 177:25 193:43]
    node cmp_input_2_0 = asSInt(bits(_cmp_input_2_0_T_3, 31, 0)) @[TPU.scala 177:25 190:43]
    node cmp_input_2_1 = asSInt(bits(_cmp_input_2_1_T_3, 31, 0)) @[TPU.scala 177:25 193:43]
    node cmp_input_2_2 = asSInt(bits(_cmp_input_2_2_T_3, 31, 0)) @[TPU.scala 177:25 193:43]
    io_out_0 <= cms_reg_2_0 @[TPU.scala 175:19]
    io_out_1 <= cms_reg_2_1 @[TPU.scala 175:19]
    io_out_2 <= cms_reg_2_2 @[TPU.scala 175:19]
    io_cmp_debug_0_0 <= cms_reg_0_0 @[TPU.scala 165:18]
    io_cmp_debug_0_1 <= cms_reg_0_1 @[TPU.scala 165:18]
    io_cmp_debug_0_2 <= cms_reg_0_2 @[TPU.scala 165:18]
    io_cmp_debug_1_0 <= cms_reg_1_0 @[TPU.scala 165:18]
    io_cmp_debug_1_1 <= cms_reg_1_1 @[TPU.scala 165:18]
    io_cmp_debug_1_2 <= cms_reg_1_2 @[TPU.scala 165:18]
    io_cmp_debug_2_0 <= cms_reg_2_0 @[TPU.scala 165:18]
    io_cmp_debug_2_1 <= cms_reg_2_1 @[TPU.scala 165:18]
    io_cmp_debug_2_2 <= cms_reg_2_2 @[TPU.scala 165:18]
    io_debug_b_regs_0_0 <= b_reg_0_0 @[TPU.scala 166:21]
    io_debug_b_regs_0_1 <= b_reg_0_1 @[TPU.scala 166:21]
    io_debug_b_regs_0_2 <= b_reg_0_2 @[TPU.scala 166:21]
    io_debug_b_regs_1_0 <= b_reg_1_0 @[TPU.scala 166:21]
    io_debug_b_regs_1_1 <= b_reg_1_1 @[TPU.scala 166:21]
    io_debug_b_regs_1_2 <= b_reg_1_2 @[TPU.scala 166:21]
    io_debug_b_regs_2_0 <= b_reg_2_0 @[TPU.scala 166:21]
    io_debug_b_regs_2_1 <= b_reg_2_1 @[TPU.scala 166:21]
    io_debug_b_regs_2_2 <= b_reg_2_2 @[TPU.scala 166:21]
    io_debug_a_regs_0_0 <= a_reg_0_0 @[TPU.scala 167:21]
    io_debug_a_regs_0_1 <= a_reg_0_1 @[TPU.scala 167:21]
    io_debug_a_regs_0_2 <= a_reg_0_2 @[TPU.scala 167:21]
    io_debug_a_regs_1_0 <= a_reg_1_0 @[TPU.scala 167:21]
    io_debug_a_regs_1_1 <= a_reg_1_1 @[TPU.scala 167:21]
    io_debug_a_regs_1_2 <= a_reg_1_2 @[TPU.scala 167:21]
    io_debug_a_regs_2_0 <= a_reg_2_0 @[TPU.scala 167:21]
    io_debug_a_regs_2_1 <= a_reg_2_1 @[TPU.scala 167:21]
    io_debug_a_regs_2_2 <= a_reg_2_2 @[TPU.scala 167:21]
    io_debug_00 <= asSInt(bits(_io_debug_00_T, 31, 0)) @[TPU.scala 168:17]
    a_reg_0_0 <= io_a_in_0 @[TPU.scala 183:39]
    a_reg_0_1 <= a_reg_0_0 @[TPU.scala 186:39]
    a_reg_0_2 <= a_reg_0_1 @[TPU.scala 186:39]
    a_reg_1_0 <= io_a_in_1 @[TPU.scala 191:39]
    a_reg_1_1 <= a_reg_1_0 @[TPU.scala 194:39]
    a_reg_1_2 <= a_reg_1_1 @[TPU.scala 194:39]
    a_reg_2_0 <= io_a_in_2 @[TPU.scala 191:39]
    a_reg_2_1 <= a_reg_2_0 @[TPU.scala 194:39]
    a_reg_2_2 <= a_reg_2_1 @[TPU.scala 194:39]
    b_reg_0_0 <= _GEN_0
    b_reg_0_1 <= _GEN_1
    b_reg_0_2 <= _GEN_2
    b_reg_1_0 <= _GEN_3
    b_reg_1_1 <= _GEN_4
    b_reg_1_2 <= _GEN_5
    b_reg_2_0 <= _GEN_6
    b_reg_2_1 <= _GEN_7
    b_reg_2_2 <= _GEN_8
    cms_reg_0_0 <= cmp_input_0_0 @[TPU.scala 197:21]
    cms_reg_0_1 <= cmp_input_0_1 @[TPU.scala 197:21]
    cms_reg_0_2 <= cmp_input_0_2 @[TPU.scala 197:21]
    cms_reg_1_0 <= cmp_input_1_0 @[TPU.scala 197:21]
    cms_reg_1_1 <= cmp_input_1_1 @[TPU.scala 197:21]
    cms_reg_1_2 <= cmp_input_1_2 @[TPU.scala 197:21]
    cms_reg_2_0 <= cmp_input_2_0 @[TPU.scala 197:21]
    cms_reg_2_1 <= cmp_input_2_1 @[TPU.scala 197:21]
    cms_reg_2_2 <= cmp_input_2_2 @[TPU.scala 197:21]
