package tpu
import chisel3._
import chisel3.util._
import chisel3.util.log2Ceil
import chisel3.internal.firrtl.Width

case class TPUParams(m: Int, k: Int, n: Int, actM: Int,  s1: Int, s2: Int) {
  // A (m x k) X B (k x n) = C (m x k)
  val aRows: Int = m
  val aCols: Int = k
  val bRows: Int = k
  val bCols: Int = n
  val cRows: Int = m
  val cCols: Int = n
  val actRows: Int = actM
  val sRows: Int = s1
  val sCols: Int = s2
  // Implementation details
  val w: Int = 32
}



class ChiselTPU(p: TPUParams) extends Module{
  type Matrix = Vec[Vec[SInt]]

  val io = IO(new Bundle{
    val a = Flipped(Decoupled(Vec(p.m, Vec(p.k, SInt(p.w.W)))))
    val b = Flipped(Decoupled(Vec(p.k, Vec(p.n, SInt(p.w.W)))))
    val out = Output(Vec(p.m, Vec(p.n, SInt(p.w.W))))
    // val debug_1 = Output(Vec(p.k, Vec(p.n, SInt(p.w.W))))
    // val debug_a_out = Output(Vec(p.k, SInt(p.w.W)))
    // val debug_systreg_out = Output(Vec(p.n, SInt(p.w.W)))
    // val debug_a_regs = Output(Vec(p.k, Vec(p.n, SInt(p.w.W))))
    // val debug_b_regs = Output(Vec(p.k, Vec(p.n, SInt(p.w.W))))
    // val debug_cycleOut = Output(UInt(32.W))
    // val debug_00 = Output(SInt(p.w.W))
    // val debug_cycleIdxCols = Output(Vec(p.n, UInt(p.w.W)))
    // val debug_cycleIdxRows = Output(Vec(p.n, UInt(p.w.W)))
    // val debug_cycleIdx = Output(UInt(p.w.W))
    // val debug_systout_upperLim = Output(SInt(p.w.W))
  })

  val load :: fill :: slice :: multiply :: clear :: Nil = Enum(5)
  val state = RegInit(load)
  val counterFlag = Wire(Bool())
  val actReg = Module(new ActReg(p)) // to switch to non slice, use p instead of systParams
  val systArr = Module(new SystArr(p))
  val myOut = RegInit(VecInit.fill(p.m, p.n)(0.S(p.w.W)))
  val a_ready = RegInit(true.B)
  val b_ready = RegInit(true.B)
  val enabledSyst = RegInit(false.B)
  systArr.io.en := enabledSyst
  val (cycle, wrap) = Counter(counterFlag && systArr.io.en, p.k+p.m+p.n+1) //assume input is valid for now
  

  // slice parameters
  // dimensions of padded input matrices
  val paddedMDim = if(p.m >= p.actM) p.actM*(((p.m-1)/p.actM)+1) else p.actM
  val paddedKDim = if(p.k >= p.s1) p.s1*(((p.k-1)/p.s1)+1) else p.s1 
  val paddedNDim = if(p.n >= p.s2) p.s2*(((p.n-1)/p.s2)+1) else p.s2

  // used for calculating slice boundaries
  val numSliceM = paddedMDim/p.actM
  val numSliceK = paddedKDim/p.s1
  val numSliceN = paddedNDim/p.s2

  // initialization of padded input matrices
  val paddedA = RegInit(VecInit.fill(paddedMDim, paddedKDim)(0.S(p.w.W)))
  val paddedB = RegInit(VecInit.fill(paddedKDim, paddedNDim)(0.S(p.w.W)))
  val paddedOut = RegInit(VecInit.fill(paddedMDim, paddedNDim)(0.S(p.w.W)))
  val slicedA = RegInit(VecInit.fill(p.actM, p.s1)(0.S(p.w.W)))
  val slicedB = RegInit(VecInit.fill(p.s1, p.s2)(0.S(p.w.W)))
  val slicedOut = RegInit(VecInit.fill(p.actM, p.s2)(0.S(p.w.W)))

  val (totalCycle, totalWrap) = Counter(true.B, 1000)
  val (sliceCycle, sliceWrap) = Counter(state === slice, numSliceM * numSliceK * numSliceN)
  
  

  //declare wires for inputs:
  val cycleIdx = Wire(UInt(p.w.W))
  val cycleIdxCols = Wire(Vec(p.n, UInt(p.w.W)))
  val cycleIdxRows = Wire(Vec(p.n, UInt(p.w.W)))
  //declare systWires
  cycleIdx := 0.U
  for(i <- 0 until p.n){
    cycleIdxCols(i) := 0.U
    cycleIdxRows(i) := 0.U
  }

  //DEBUGGING:
  // io.debug_1 := systArr.io.cmp_debug
  // io.debug_a_out := actReg.io.a_out
  // io.debug_a_regs := systArr.io.debug_a_regs
  // io.debug_b_regs := systArr.io.debug_b_regs
  // io.debug_cycleOut := cycle
  // io.debug_00 := systArr.io.debug_00
  // io.debug_systreg_out := systArr.io.out
  // io.debug_cycleIdxCols := cycleIdxCols
  // io.debug_cycleIdxRows := cycleIdxRows
  // io.debug_cycleIdx := cycleIdx
  // io.debug_systout_upperLim := 0.S

  def copyMatrixToPadded(in: Matrix, out: Matrix) = {
    for(i <- 0 until out.size){
      for(j <- 0 until out.head.size){
        if(i < in.size && j < in.head.size){
          out(i)(j) := in(i)(j)
        }
        else{
          out(i)(j) := 0.S
        }
      }
    }
  }
   val boundK = p.s1.U * (sliceCycle % numSliceK.U)//p.s1.U * (sliceCycle % (numSliceK*numSliceN).U)/p.s1.U
   val boundM = p.actM.U * ((sliceCycle % (numSliceM*numSliceK).U) / numSliceK.U) //(sliceCycle*p.actM.U % (numSliceK*numSliceM).U)/p.actM.U
   val boundN = p.s2.U * (sliceCycle / (numSliceK*numSliceM).U)
  printf(cf"bound K: ${boundK}\n")
  printf(cf"bound M: ${boundM}\n")
  printf(cf"bound N: ${boundN}\n")
  def updateMatricesSlices() = {
    for(i <- 0 until p.s1){
      for(j <- 0 until p.actM){
        slicedA(j)(i) := paddedA(boundM+j.U)(boundK+i.U)
      }
      for(j <- 0 until p.s2){
        slicedB(i)(j) := paddedB(boundK+i.U)(boundN+j.U) 
      }
    }
  }
  
  // printf(cf"SLICE CYCLE: ${sliceCycle}\n")
  updateMatricesSlices()

  // val ()
  // only allows reading input in input states
  io.a.ready := a_ready
  io.b.ready := b_ready
  io.out := myOut
  
  counterFlag := false.B
  //initialize inputs for submodules
  actReg.io.index := (p.s1+p.actM-1).U-cycle
  systArr.io.a_in := actReg.io.a_out
  val act_in = RegInit(VecInit.fill(p.m, p.k)(0.S(p.w.W)))
  val allowReadB = RegInit(false.B)
  systArr.io.b_readingin := allowReadB
  actReg.io.a :=  slicedA//act_in// //switch between slice and act_in
  systArr.io.b_in := slicedB//io.b.bits//   //switch between slice and b.bits

  when(state === load){ 
    printf(cf"\nLOAD TOTAL CYCLE ${totalCycle}\n")
    printf(cf"LOAD\n")
    when(io.a.valid && io.a.ready){
        state := fill
        act_in := io.a.bits
        copyMatrixToPadded(io.a.bits, paddedA)
        copyMatrixToPadded(io.a.bits, actReg.io.a)
        a_ready := false.B
      }
      cycle := 0.U
  }
  .elsewhen(state === fill){
      enabledSyst := false.B
      when(io.b.valid && io.b.ready){
        //syst_in := io.b.bits
        copyMatrixToPadded(io.b.bits, paddedB)
        copyMatrixToPadded(io.b.bits, systArr.io.b_in)
        allowReadB := true.B
        b_ready := false.B
        counterFlag := true.B
        cycle := 0.U
      }
      .elsewhen(!io.b.ready){
        state := slice
        sliceCycle := (numSliceM * numSliceK * numSliceN - 1).U
      }
  }
  .elsewhen(state === slice){
    printf(cf"\nSLICE TOTAL CYCLE ${totalCycle}\n")
    state := multiply
    // allowReadB := true.B
    cycle := 0.U
    counterFlag := false.B
    slicedOut := VecInit.fill(p.actM, p.s2)(0.S(p.w.W))    
    enabledSyst := false.B
  }
  .elsewhen(state === multiply){
    enabledSyst := true.B
    counterFlag := true.B
    when(cycle === 0.U){
      allowReadB := true.B
    }
    .otherwise{
      allowReadB := false.B
    }
    when(cycle >= (2+p.s1-1).U){ //BUG
      cycleIdx := cycle-(2+p.s1-1).U
    }.
    otherwise{
      cycleIdx := cycle
    }
    when(cycle >= (2+p.s1-1).U){
      for(c <- 0 until p.s2){
        when(cycleIdx>=c.U && cycleIdx<(p.actM+c).U){
          for(r <- 0 until p.actM){
            if(r==(p.actM-1)){
              slicedOut(r.U)(c.U) := systArr.io.out(c.U)
            }else{
              slicedOut(r.U)(c.U) := slicedOut((r+1).U)(c.U)
            }
          }
        }
      } 
    }

    

    when(sliceCycle === (numSliceM * numSliceK * numSliceN - 1).U && cycle === (p.m+p.k+p.n).U){
      for(i <- 0 until slicedOut.size){
        for(j <- 0 until slicedOut.head.size){
          paddedOut(i.U + boundM)(j.U + boundN) := paddedOut(i.U + boundM)(j.U + boundN) + slicedOut(i.U)(j.U)
          when(i.U + boundM < p.m.U && j.U + boundN < p.n.U){
            myOut(i.U + boundM)(j.U + boundN) := myOut(i.U + boundM)(j.U + boundN) + slicedOut(i.U)(j.U)
          }
        }
      }
       printf(cf"\nSTART CLEAR TOTAL CYCLE ${totalCycle}\n")
      state := clear
    }
    .elsewhen(cycle === (p.m+p.k+p.n).U){
      for(i <- 0 until slicedOut.size){
        for(j <- 0 until slicedOut.head.size){
          paddedOut(i.U + boundM)(j.U + boundN) := paddedOut(i.U + boundM)(j.U + boundN) + slicedOut(i.U)(j.U)
          when(i.U + boundM < p.m.U && j.U + boundN < p.n.U){
            myOut(i.U + boundM)(j.U + boundN) := myOut(i.U + boundM)(j.U + boundN) + slicedOut(i.U)(j.U)
          }
        }
      }
      state := slice
    }
      printf(cf"PADDED A: \n")
    for(i <- 0 until paddedA.size){
      printf(cf"${paddedA(i)}\n")
   }
     printf(cf"PADDED B: \n")
    for(i <- 0 until paddedB.size){
      printf(cf"${paddedB(i)}\n")
    }
    printf(cf"SLICED A: \n")
    for(i <- 0 until slicedA.size){
      printf(cf"${slicedA(i)}\n")
    }
    printf(cf"SLICED B: \n")
    for(i <- 0 until slicedB.size){
      printf(cf"${slicedB(i)}\n")
    }
    printf(cf"MY SLICED OUT: \n")
    for(i <- 0 until slicedOut.size){
      printf(cf"${slicedOut(i)}\n")
    }
        // // printf(cf"\n-------------------------\n")
    printf(cf"MY OUT: \n")
    for(i <- 0 until io.out.size){
      printf(cf"${io.out(i)}\n")
    }
  }
  
  .elsewhen(state === clear){
    printf(cf"\nCLEAR TOTAL CYCLE ${totalCycle}\n")
    state := fill
    b_ready := true.B
    // io.b.ready := true.B
    myOut := VecInit.fill(p.m, p.n)(0.S(p.w.W))
  }
  
}

class ActReg(p: TPUParams) extends Module{
    val io = IO(new Bundle{
        val index = Input(UInt((log2Ceil(p.s1+p.actM-1) + 1).W))
        val a = Input(Vec(p.actM, Vec(p.s1, SInt(p.w.W))))
        val a_out = Output(Vec(p.s1, SInt(p.w.W)))
    })
    
    when(io.index < (p.s1+p.actM-1).U){
      for (j <- 0 until p.s1) {
        when((p.s1-j-1).S-io.index.asSInt <= 0.S && (p.actM-j-1+p.s1).S-io.index.asSInt > 0.S){
          io.a_out(j.U) := io.a((p.s1 + p.actM - 2 - j).U - io.index)(j)
        }
        .otherwise{
          io.a_out(j.U) := 0.S
        }
      }
    }
    .otherwise{
      for(j <- 0 until p.s1){
        io.a_out(j.U) := 0.S
      }
    }
}

class SystArr(p: TPUParams) extends Module{
    val io = IO(new Bundle {
        val a_in = Input(Vec(p.s1, SInt(p.w.W)))
        val b_in = Input(Vec(p.s1, Vec(p.s2, SInt(p.w.W))))
        val en = Input(Bool())
        val b_readingin = Input(Bool())
        val out = Output((Vec(p.s2, SInt(p.w.W))))
        //debugOuts
        val cmp_debug = Output(Vec(p.s1, Vec(p.s2, SInt(p.w.W))))
        val debug_b_regs = Output(Vec(p.s1, Vec(p.s2, SInt(p.w.W))))
        val debug_a_regs = Output(Vec(p.s1, Vec(p.s2, SInt(p.w.W))))
        val debug_00 = Output(SInt(p.w.W))
    })
    val a_reg = Reg(Vec(p.s1, Vec(p.s2, SInt(p.w.W))))
    val b_reg = Reg(Vec(p.s1, Vec(p.s2, SInt(p.w.W))))
    val cms_reg = Reg(Vec(p.s1, Vec(p.s2, SInt(p.w.W))))

    when(io.en){
      
      io.cmp_debug := cms_reg
      io.debug_b_regs := b_reg
      io.debug_a_regs := a_reg
      io.debug_00 := io.a_in(0) * b_reg(0)(0)
      when(io.b_readingin){
        b_reg := io.b_in
      }.otherwise{
        b_reg := b_reg
      }
      for(i <- 0 until p.s2){
          io.out(i) := cms_reg(p.s1-1)(i)
      }
      val cmp_input = Wire(Vec(p.s1, Vec(p.s2, SInt(p.w.W))))
      for(kidx <- 0 until p.s1){
          for(nidx <- 0 until p.s2){
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
    }
    .otherwise{
      io.out := VecInit.fill(p.s2)(0.S(p.w.W))
      io.cmp_debug := VecInit.fill(p.s1, p.s2)(0.S(p.w.W))
      io.debug_b_regs := VecInit.fill(p.s1, p.s2)(0.S(p.w.W))
      io.debug_a_regs := VecInit.fill(p.s1, p.s2)(0.S(p.w.W))
      io.debug_00 := 0.S
      cms_reg := VecInit.fill(p.s1, p.s2)(0.S(p.w.W))
    }
    
}