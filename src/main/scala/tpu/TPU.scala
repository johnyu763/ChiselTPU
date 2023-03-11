package tpu
import chisel3._
import chisel3.util._
import chisel3.util.log2Ceil
import chisel3.internal.firrtl.Width

case class TPUParams(m: Int, k: Int, n: Int) {
  // A (m x k) X B (k x n) = C (m x k)
  val aRows: Int = m
  val aCols: Int = k
  val bRows: Int = k
  val bCols: Int = n
  val cRows: Int = m
  val cCols: Int = n
  // Implementation details
  val w: Int = 32
}
class ChiselTPU(p: TPUParams) extends Module{
  val io = IO(new Bundle{
    val a = Flipped(Decoupled(Vec(p.m, Vec(p.k, SInt(p.w.W)))))
    val b = Flipped(Decoupled(Vec(p.k, Vec(p.n, SInt(p.w.W)))))
    val out = Output(Vec(p.m, Vec(p.n, SInt(p.w.W))))
    val debug_1 = Output(Vec(p.k, Vec(p.n, SInt(p.w.W))))
    val debug_a_out = Output(Vec(p.k, SInt(p.w.W)))
    val debug_systreg_out = Output(Vec(p.n, SInt(p.w.W)))
    val debug_a_regs = Output(Vec(p.k, Vec(p.n, SInt(p.w.W))))
    val debug_b_regs = Output(Vec(p.k, Vec(p.n, SInt(p.w.W))))
    val debug_cycleOut = Output(UInt(32.W))
    val debug_00 = Output(SInt(p.w.W))
    val debug_cycleIdxCols = Output(Vec(p.n, UInt(p.w.W)))
    val debug_cycleIdxRows = Output(Vec(p.n, UInt(p.w.W)))
    val debug_cycleIdx = Output(UInt(p.w.W))
    val debug_systout_upperLim = Output(SInt(p.w.W))
  })
  val load :: fill :: multiply :: clear :: Nil = Enum(4)
  val state = RegInit(load)
  val counterFlag = Wire(Bool())
  //val (cycle, wrap) = Counter(state === multiply, p.k+p.m+p.k+1) //assume input is valid for now
  val (cycle, wrap) = Counter(counterFlag, p.k+p.m+p.n+1) //assume input is valid for now
  val actReg = Module(new ActReg(p))
  //val systParams = TPUParams(p.k, io.a.bits.head.size, io.b.bits.head.size)
  val systArr = Module(new SystArr(p))
  val myOut = RegInit(VecInit.fill(p.m, p.n)(0.S(p.w.W)))
  val a_ready = RegInit(true.B)
  val b_ready = RegInit(true.B)

  io.a.ready := a_ready
  io.b.ready := b_ready
  io.out := myOut
  
  counterFlag := false.B
  //initialize inputs for submodules
  actReg.io.index := (p.k+p.m-1).U-cycle
  //systArr.io.a_in := RegNext(actReg.io.a_out)
  systArr.io.a_in := actReg.io.a_out
  val act_in = RegInit(VecInit.fill(p.m, p.k)(0.S(p.w.W)))
  //val syst_in = RegInit(VecInit.fill(p.k, p.n)(0.S(p.w.W)))
  systArr.io.b_readingin := false.B
  actReg.io.a := act_in
  //systArr.io.b_in := syst_in
  systArr.io.b_in := io.b.bits

  //declare wires for inputs:
  val cycleIdx = Wire(UInt(p.w.W))
  val cycleIdxCols = Wire(Vec(p.n, UInt(p.w.W)))
  val cycleIdxRows = Wire(Vec(p.n, UInt(p.w.W)))
  val systArrOutOffset = Wire(UInt(p.w.W))
  //declare systWires
  val limitingDimension = Wire(UInt(p.w.W))
  cycleIdx := 0.U
  for(i <- 0 until p.n){
    cycleIdxCols(i) := 0.U
    cycleIdxRows(i) := 0.U
  }

  //DEBUGGING:
  io.debug_1 := systArr.io.cmp_debug
  io.debug_a_out := actReg.io.a_out
  io.debug_a_regs := systArr.io.debug_a_regs
  io.debug_b_regs := systArr.io.debug_b_regs
  io.debug_cycleOut := cycle
  io.debug_00 := systArr.io.debug_00
  io.debug_systreg_out := systArr.io.out
  io.debug_cycleIdxCols := cycleIdxCols
  io.debug_cycleIdxRows := cycleIdxRows
  io.debug_cycleIdx := cycleIdx
  io.debug_systout_upperLim := 0.S

  systArrOutOffset := 0.U
  if(p.m<p.n){
    limitingDimension := p.m.U
  }else{
    limitingDimension := p.n.U
  }
  when(state === load){ 
      when(io.a.valid && io.a.ready){
        state := fill
        act_in := io.a.bits
        a_ready := false.B
      }
      cycle := 0.U
  }
  .elsewhen(state === fill){
      when(io.b.valid && io.b.ready){
        state := multiply
        //syst_in := io.b.bits
        systArr.io.b_in := io.b.bits
        systArr.io.b_readingin := true.B
        b_ready := false.B
        counterFlag := true.B
        cycle := 0.U
      }
      // print("cycle:")
      // print(cycle)
      // print("\n")
  }
  .elsewhen(state === multiply){
    // print("cycle:")
    //   print(cycle)
    //   print("\n")
    counterFlag := true.B
    when(cycle === (p.m+p.k+p.n).U){
      state := clear
    }
    when(cycle >= (2+p.k-1).U){ //BUG
      cycleIdx := cycle-(2+p.k-1).U
    }.
    otherwise{
      cycleIdx := cycle
    }
    when(cycle >= (2+p.k-1).U){
      for(c <- 0 until p.n){
        when(cycleIdx>=c.U && cycleIdx<(p.m+c).U){
          for(r <- 0 until p.m){
            if(r==(p.m-1)){
              myOut(r.U)(c.U) := systArr.io.out(c.U)
            }else{
              myOut(r.U)(c.U) := myOut((r+1).U)(c.U)
            }
          }
        }
      } 
    }
    // io.debug_systout_upperLim := cycleIdx(0).asSInt()-p.m.S
    // for(i <- 0 until systArr.io.out.size){
    //   //cycleIdx is the nominal cycle index starting when cycle reaches outputting
    //   when(cycle >= (2+p.k-1).U){ //BUG
    //     //cycleIdx(i) := cycle-(p.m+2).U-1.U
    //     cycleIdx(i) := cycle-(2+p.k-1).U
    //   }.
    //   otherwise{
    //     cycleIdx(i) := cycle
    //   }
    //   //NOTE: these outputs are idx for the OUT reg! an m by n mareix!
    //   //col to start at should increment until it reaches k
    //   when(cycleIdx(i)<(p.m).U){
    //     cycleIdxCols(i) := cycleIdx(i) - i.U
    //   }.otherwise{
    //     cycleIdxCols(i) := (p.m-1).U - i.U
    //   }
    //   //row to start at should increment until it reaches k
    //   when(cycleIdx(i)<(p.m).U){
    //     cycleIdxRows(i) := 0.U + i.U
    //   }.otherwise{
    //     cycleIdxRows(i) := (cycleIdx(i)-(p.m).U+1.U) + i.U
    //   }
    //   //when syst arr is outputting, begin attatching and assigning
    //   systArrOutOffset := 0.U
    //   when(cycle >= (2+p.k-1).U){
    //     when((i.U<cycleIdx(0)+1.U) && (i.U<(p.m+p.n-2).U-cycleIdx(0)+1.U)){ //bug here: i limit for large, not small
    //       when(cycleIdx(0)<p.m.U){
    //         myOut(cycleIdxCols(i))(cycleIdxRows(i)) := systArr.io.out(i.U)
    //       }.otherwise{
    //         myOut(cycleIdxCols(i))(cycleIdxRows(i)) := systArr.io.out(i.U + (cycleIdx(0)-p.m.U+1.U))
    //       }
    //     }
    //   }
      // when(cycle >= (2+p.k-1).U){
      //   when(cycleIdxCols(i)<p.m.U && cycleIdxRows(i)<p.n.U){
      //     when(cycleIdx(0)<p.n.U){
      //       myOut(cycleIdxCols(i))(cycleIdxRows(i)) := systArr.io.out(i.U)
      //     }.otherwise{
      //       myOut(cycleIdxCols(i))(cycleIdxRows(i)) := systArr.io.out(i.U + (cycleIdx(0)-p.n.U+1.U))
      //     }
      //   }
      // }
      // when(cycle >= (2+p.k-1).U){
      //   when(cycleIdx(0)<p.n.U){
      //     when(cycleIdxCols(i)<p.m.U && cycleIdxRows(i)<p.n.U){
      //       myOut(cycleIdxCols(i))(cycleIdxRows(i)) := systArr.io.out(i.U)
      //     }
      //   }.otherwise{
      //     when(cycleIdxCols(i)<p.m.U && cycleIdxRows(i)<p.n.U){
      //       myOut(cycleIdxCols(i))(cycleIdxRows(i)) := systArr.io.out(i.U + (cycleIdx(0)-p.n.U+1.U))
      //     }
      //   }
      // }
    // }
    // printf(cf"-------------------------\n")
    // printf(cf"MY TPU OUT: \n")
    // for(i <- 0 until myOut.size){
    //   printf(cf"${myOut(i)}\n")
    // }
    // printf(cf"actreg out:\n")
    // for(i <- 0 until p.k){
    //   printf(cf"${actReg.io.a_out(i)}  ")
    // }
    // printf("\n")
    // printf(cf"systarr out:\n")
    // for(i <- 0 until p.n){
    //   printf(cf"${systArr.io.out(i)}  ")
    // }
    // printf("\n")
    // printf(cf"index: \n")
    // printf(cf"${(p.k+p.m-2).U-cycle}  ")
    // printf("\n\n\n")
  }
  .elsewhen(state === clear){
    printf(cf"FINISH\n")
    state := fill
    //b_ready := true.B
    io.b.ready := true.B
    myOut := VecInit.fill(p.m, p.n)(17.S(p.w.W))
  }
  
}

class ActReg(p: TPUParams) extends Module{
    val io = IO(new Bundle{
        val index = Input(UInt((log2Ceil(p.k+p.m-1) + 1).W))
        val a = Input(Vec(p.m, Vec(p.k, SInt(p.w.W))))
        val a_out = Output(Vec(p.k, SInt(p.w.W)))
    })
    when(io.index < (p.k+p.m-1).U){
      for (j <- 0 until p.k) {
        when((p.k-j-1).S-io.index.asSInt <= 0.S && (p.m-j-1+p.k).S-io.index.asSInt > 0.S){
          io.a_out(j.U) := io.a((p.k + p.m - 2 - j).U - io.index)(j)
        }
        .otherwise{
          io.a_out(j.U) := 0.S
        }
      }
    }
    .otherwise{
      for(j <- 0 until p.k){
        io.a_out(j.U) := 0.S
      }
    }
}

class SystArr(p: TPUParams) extends Module{
    val io = IO(new Bundle {
        val a_in = Input(Vec(p.k, SInt(p.w.W)))
        val b_in = Input(Vec(p.k, Vec(p.n, SInt(p.w.W))))
        val b_readingin = Input(Bool())
        val out = Output((Vec(p.n, SInt(p.w.W))))
        //debugOuts
        val cmp_debug = Output(Vec(p.k, Vec(p.n, SInt(p.w.W))))
        val debug_b_regs = Output(Vec(p.k, Vec(p.n, SInt(p.w.W))))
        val debug_a_regs = Output(Vec(p.k, Vec(p.n, SInt(p.w.W))))
        val debug_00 = Output(SInt(p.w.W))
    })
    val a_reg = Reg(Vec(p.k, Vec(p.n, SInt(p.w.W))))
    val b_reg = Reg(Vec(p.k, Vec(p.n, SInt(p.w.W))))
    val cms_reg = Reg(Vec(p.k, Vec(p.n, SInt(p.w.W))))
    io.cmp_debug := cms_reg
    io.debug_b_regs := b_reg
    io.debug_a_regs := a_reg
    io.debug_00 := io.a_in(0) * b_reg(0)(0)
    when(io.b_readingin){
      b_reg := io.b_in
    }.otherwise{
      b_reg := b_reg
    }
    for(i <- 0 until p.n){
        io.out(i) := cms_reg(p.k-1)(i)
    }
    val cmp_input = Wire(Vec(p.k, Vec(p.n, SInt(p.w.W))))
    for(kidx <- 0 until p.k){
        for(nidx <- 0 until p.n){
            if(kidx==0){
                if(nidx==0){
                    cmp_input(kidx)(nidx) := io.a_in(kidx) * b_reg(kidx)(nidx)
                    a_reg(kidx)(nidx) := io.a_in(kidx)
                }else{
                    cmp_input(kidx)(nidx) := a_reg(kidx)(nidx-1) * b_reg(kidx)(nidx)
                    a_reg(kidx)(nidx) := a_reg(kidx)(nidx-1)
                }
            }else{
                if(nidx==0){
                    cmp_input(kidx)(nidx) := (io.a_in(kidx) * b_reg(kidx)(nidx)) + cms_reg(kidx-1)(nidx)
                    a_reg(kidx)(nidx) := io.a_in(kidx)
                }else{
                    cmp_input(kidx)(nidx) := (a_reg(kidx)(nidx-1) * b_reg(kidx)(nidx)) + cms_reg(kidx-1)(nidx)
                    a_reg(kidx)(nidx) := a_reg(kidx)(nidx-1)
                }
            }
            cms_reg := cmp_input
        }
        
        
    }
    // printf(cf"-------------------------\n")
    // printf(cf"MY A_IN:\n")
    // for(i <- 0 until io.a_in.size){
    //   printf(cf"${io.a_in(i)} ")
    // }
    // printf("\n\n")
    // printf(cf"MY B_IN:\n")
    // for(i <- 0 until io.b_in.size){
    //   printf(cf"${io.b_in(i)}\n")
    // }
    // printf(cf"MY A_REG:\n")
    // for(i <- 0 until a_reg.size){
    //   printf(cf"${a_reg(i)}\n")
    // }
    // printf(cf"MY CSM:\n")
    // for(i <- 0 until cms_reg.size){
    //   printf(cf"${cms_reg(i)}\n")
    // }
    // printf(cf"MY OUT:\n")
    // for(i <- 0 until io.out.size){
    //   printf(cf"${io.out(i)}\n")
    // // }
    // printf(cf"\n\n")
}