package tpu
import chisel3._
import chisel3.util._
import chisel3.util.log2Ceil
import chisel3.internal.firrtl.Width

case class TPUParams(m: Int, k: Int, n: Int, s1: Int, s2: Int) {
  // A (m x k) X B (k x n) = C (m x k)
  val aRows: Int = m
  val aCols: Int = k
  val bRows: Int = k
  val bCols: Int = n
  val cRows: Int = m
  val cCols: Int = n
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
  val (cycle, wrap) = Counter(counterFlag, p.k+p.m+p.n+1) //assume input is valid for now
  val systParams = TPUParams(2, 2, 2, p.s1, p.s2) //hard-coded systolic array size of 3 for slicing
  val actReg = Module(new ActReg(systParams)) // to switch to non slice, use p instead of systParams
  val systArr = Module(new SystArr(systParams))
  val myOut = RegInit(VecInit.fill(p.m, p.n)(0.S(p.w.W)))
  val a_ready = RegInit(true.B)
  val b_ready = RegInit(true.B)

  // slice parameters
  // dimensions of padded input matrices
  val paddedMDim = if(p.m >= systParams.m) p.m % systParams.m + p.m else p.m % systParams.m + systParams.m % p.m
  val paddedKDim = if(p.k >= systParams.k) p.k % systParams.k + p.k else p.k % systParams.k + systParams.k % p.k
  val paddedNDim = if(p.n >= systParams.n) p.n % systParams.n + p.n else p.n % systParams.n + systParams.n % p.n

  // used for calculating slice boundaries
  val numSliceM = paddedMDim/systParams.m
  val numSliceK = paddedKDim/systParams.k
  val numSliceN = paddedNDim/systParams.n

  // initialization of padded input matrices
  val paddedA = RegInit(VecInit.fill(paddedMDim, paddedKDim)(0.S(p.w.W)))
  val paddedB = RegInit(VecInit.fill(paddedKDim, paddedNDim)(0.S(p.w.W)))
  val paddedOut = RegInit(VecInit.fill(paddedMDim, paddedNDim)(0.S(p.w.W)))
  val slicedA = RegInit(VecInit.fill(systParams.m, systParams.k)(0.S(p.w.W)))
  val slicedB = RegInit(VecInit.fill(systParams.k, systParams.n)(0.S(p.w.W)))
  val slicedOut = RegInit(VecInit.fill(systParams.m, systParams.n)(0.S(p.w.W)))

  val (sliceCycle, sliceWrap) = Counter(state === slice, numSliceM * numSliceK * numSliceN)
  // val ()
  // only allows reading input in input states
  io.a.ready := a_ready
  io.b.ready := b_ready
  io.out := myOut
  
  counterFlag := false.B
  //initialize inputs for submodules
  actReg.io.index := (systParams.k+systParams.m-1).U-cycle
  systArr.io.a_in := actReg.io.a_out
  val act_in = RegInit(VecInit.fill(p.m, p.k)(0.S(p.w.W)))
  systArr.io.b_readingin := false.B
  actReg.io.a :=  slicedA//act_in// //switch between slice and act_in
  systArr.io.b_in := slicedB//io.b.bits//   //switch between slice and b.bits

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
   val boundK = systParams.k.U * ((sliceCycle % (numSliceK*numSliceN).U) % numSliceK.U)//systParams.k.U * (sliceCycle % (numSliceK*numSliceN).U)/systParams.k.U
   val boundM = systParams.m.U * ((sliceCycle % (numSliceM*numSliceK).U) / numSliceK.U) //(sliceCycle*systParams.m.U % (numSliceK*numSliceM).U)/systParams.m.U
   val boundN = systParams.n.U * (sliceCycle / (numSliceK*numSliceN).U)

  def updateMatricesSlices() = {
   

    // printf(cf"\nbound K: ${boundK} -${boundK+systParams.k.U}\n")
    // printf(cf"bound M: ${boundM} -${boundM+systParams.m.U}\n")
    // printf(cf"bound N: ${boundN} -${boundN+systParams.n.U}\n")
    // printf(cf"NUM M SLICE: ${numSliceM}\n")
    // printf(cf"NUM K SLICE: ${numSliceK}\n")
    // printf(cf"NUM N SLICE: ${numSliceN}\n")
    for(i <- 0 until systParams.k){
      for(j <- 0 until systParams.m){
        slicedA(j)(i) := paddedA(boundM+j.U)(boundK+i.U)
      }
      for(j <- 0 until systParams.n){
        slicedB(i)(j) := paddedB(boundK+i.U)(boundN+j.U) 
      }
    }

    // printf(cf"SLICED 2 A: \n")
    // for(i <- 0 until slicedA.size){
    //   printf(cf"${slicedA(i)}\n")
    // }
    // printf(cf"SLICED 2 B: \n")
    // for(i <- 0 until slicedB.size){
    //   printf(cf"${slicedB(i)}\n")
    // }
    // printf(cf"\nFIRST SLICED A ${paddedA(0.U).slice(0,2)}\n")
    // slicedA := paddedA(boundM, boundM+systParams.m.U)(boundK, boundK+systParams.k.U)
    // slicedB := paddedB(boundK, boundK+systParams.k.U)(boundN, boundN+systParams.n.U)
  }
  
  // printf(cf"SLICE CYCLE: ${sliceCycle}\n")
  updateMatricesSlices()
  when(state === load){ 
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
      when(io.b.valid && io.b.ready){
        //syst_in := io.b.bits
        copyMatrixToPadded(io.b.bits, paddedB)
        copyMatrixToPadded(io.b.bits, systArr.io.b_in)
        // systArr.io.b_in := io.b.bits//slicedB//   //switch between slice and b.bits
        systArr.io.b_readingin := true.B
        b_ready := false.B
        counterFlag := true.B
        cycle := 0.U
      }
      .elsewhen(!io.b.ready){
        state := slice
        sliceCycle := (numSliceM * numSliceK * numSliceN - 1).U
      }
      // print("cycle:")
      // print(cycle)
      // print("\n")
  }
  .elsewhen(state === slice){
    // printf(cf"SLICE CYCLE: ${sliceCycle}\n")
    // printf(cf"IN SLICE STATE\n")
    // updateMatricesSlices()
    state := multiply
    printf(cf"PADDED THING\n")
    for(i <- 0 until io.out.size){
      printf(cf"${io.out(i)}\n")
    }
    systArr.io.b_readingin := true.B
    // printf(cf"SLICED A: \n")
    // for(i <- 0 until slicedA.size){
    //   printf(cf"${slicedA(i)}\n")
    // }
    // printf(cf"SLICED B: \n")
    // for(i <- 0 until systArr.io.b_in.size){
    //   printf(cf"${systArr.io.b_in(i)}\n")
    // }
    cycle := 0.U
    slicedOut := VecInit.fill(systParams.m, systParams.n)(0.S(p.w.W))
  }
  .elsewhen(state === multiply){
    counterFlag := true.B
    // printf(cf"SLICE CYCLE: ${sliceCycle}\n")
    // printf(cf"\nCYCLE COUNT: ${cycle} AND MAX ${p.m+p.k+p.n}\n")
    // when(cycle === (p.m+p.k+p.n).U){
    //   state := clear
    // }
    // printf(cf"IN MULTIPLE\n")
    
    // for(i <- 0 until systArr.io.out.size){
    //   when(cycle > (p.n+i).U && cycle < (p.n+p.n+1+i).U){
    //     myOut(cycle - (p.n+i+1).U)(i.U) := systArr.io.out(i.U)
    //   }
    // }
    when(cycle >= (2+systParams.k-1).U){ //BUG
      cycleIdx := cycle-(2+systParams.k-1).U
    }.
    otherwise{
      cycleIdx := cycle
    }
    when(cycle >= (2+systParams.k-1).U){
      for(c <- 0 until systParams.n){
        when(cycleIdx>=c.U && cycleIdx<(systParams.m+c).U){
          for(r <- 0 until systParams.m){
            if(r==(systParams.m-1)){
              slicedOut(r.U)(c.U) := systArr.io.out(c.U)
              // paddedOut(r.U)(c.U) := systArr.io.out(c.U)
            }else{
              slicedOut(r.U)(c.U) := slicedOut((r+1).U)(c.U)
              // paddedOut(r.U + boundM)(c.U + boundN) := paddedOut((r+1).U + boundM)(c.U + boundN)
            }
          }
        }
      } 
    }

    

    when(sliceCycle === (numSliceM * numSliceK * numSliceN - 1).U && cycle === (p.m+p.k+p.n).U){
      // printf(cf"SLICE MY OUT\n")
      printf(cf"${boundM} ${boundN}\n")
      for(i <- 0 until slicedOut.size){
        for(j <- 0 until slicedOut.head.size){
          when(i.U + boundM < p.m.U && j.U + boundN < p.n.U){
            myOut(i.U + boundM)(j.U + boundN) := myOut(i.U + boundM)(j.U + boundN) + slicedOut(i.U)(j.U)
          }
        }
      }
      // copyMatrixToPadded(paddedOut, myOut)
      // printf(cf"MY PADDED OUT: \n")
      // for(i <- 0 until paddedOut.size){
      //   printf(cf"${paddedOut(i)}\n")
      // }
      state := clear
    }
    .elsewhen(cycle === (p.m+p.k+p.n).U){
      // printf(cf"CLEAR MY OUT\n")
      for(i <- 0 until slicedOut.size){
        for(j <- 0 until slicedOut.head.size){
          when(i.U + boundM < p.m.U && j.U + boundN < p.n.U){
            myOut(i.U + boundM)(j.U + boundN) := myOut(i.U + boundM)(j.U + boundN) + slicedOut(i.U)(j.U)
          }
        }
      }
      // copyMatrixToPadded(paddedOut, myOut)
      // printf(cf"MY PADDED OUT: \n")
      // for(i <- 0 until paddedOut.size){
      //   printf(cf"${paddedOut(i)}\n")
      // }
      state := slice
      
    }

    // printf(cf"MY SLICED OUT: \n")
    // for(i <- 0 until slicedOut.size){
    //   printf(cf"${slicedOut(i)}\n")
    // }
    // // // printf(cf"\n-------------------------\n")
    // printf(cf"MY OUT: \n")
    // for(i <- 0 until io.out.size){
    //   printf(cf"${io.out(i)}\n")
    // }
    // printf(cf"SLICE CYCLE ${sliceCycle}\n")
    // printf(cf"NORMAL A: \n")
    // for(i <- 0 until io.a.bits.size){
    //   printf(cf"${io.a.bits(i)}\n")
    // }
    // printf(cf"NORMAL B: \n")
    // for(i <- 0 until io.b.bits.size){
    //   printf(cf"${io.b.bits(i)}\n")
    // }
  //   printf(cf"PADDED A: \n")
  //   for(i <- 0 until paddedA.size){
  //     printf(cf"${paddedA(i)}\n")
  //  }
  //    printf(cf"PADDED B: \n")
  //   for(i <- 0 until paddedB.size){
  //     printf(cf"${paddedB(i)}\n")
  //   }
    // printf(cf"SLICED A: \n")
    // for(i <- 0 until slicedA.size){
    //   printf(cf"${slicedA(i)}\n")
    // }
    // printf(cf"SLICED B: \n")
    // for(i <- 0 until slicedB.size){
    //   printf(cf"${slicedB(i)}\n")
    // }
    // printf(cf"actreg out:\n")
    // for(i <- 0 until actReg.io.a_out.size){
    //   printf(cf"${actReg.io.a_out(i)}  ")
    // }
    // printf("\n")
    // printf(cf"systarr out:\n")
    // for(i <- 0 until systArr.io.out.size){
    //   printf(cf"${systArr.io.out(i)}  ")
    // }
    // printf("\n")
    // printf(cf"index: \n")
    // printf(cf"${(p.k+p.m-2).U-cycle}  ")
    // printf("\n\n\n")
  }
  .elsewhen(state === clear){
    // printf(cf"PADDED THING CLEAR\n")
    // for(i <- 0 until io.out.size){
    //   printf(cf"${io.out(i)}\n")
    // }
    // printf(cf"FINISH\n")
    state := fill
    b_ready := true.B
    // io.b.ready := true.B
    myOut := VecInit.fill(p.m, p.n)(0.S(p.w.W))
  }
  
}

class ActReg(p: TPUParams) extends Module{
    val io = IO(new Bundle{
        val index = Input(UInt((log2Ceil(p.k+p.m-1) + 1).W))
        val a = Input(Vec(p.m, Vec(p.k, SInt(p.w.W))))
        val a_out = Output(Vec(p.k, SInt(p.w.W)))
    })
    // printf(cf"ACT REG CYCLE: ${io.index}\n")
    // printf(cf"ACT REG INPUT: \n")
    // for(i <- 0 until io.a.size){
    //   printf(cf"${io.a(i)} \n")
    // }
    // printf("\n")
    
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

    // printf(cf"ACT REG INPUT: ${io.a_out}\n")
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
    printf(cf"\nMY A_IN:\n")
    for(i <- 0 until io.a_in.size){
      printf(cf"${io.a_in(i)} ")
    }
    printf("\n\n")
    printf(cf"MY B_IN:\n")
    for(i <- 0 until io.b_in.size){
      printf(cf"${io.b_in(i)}\n")
    }
    printf(cf"MY A_REG:\n")
    for(i <- 0 until a_reg.size){
      printf(cf"${a_reg(i)}\n")
    }
    printf(cf"\nMY CSM:\n")
    for(i <- 0 until cms_reg.size){
      printf(cf"${cms_reg(i)}\n")
    }
    printf(cf"MY SYST OUT:\n")
    for(i <- 0 until io.out.size){
      printf(cf"${io.out(i)}\n")
    }
    // printf(cf"\n\n")
}