package tpu
import chisel3._
import chisel3.util._
import chisel3.util.log2Ceil

case class TPUParams(m: Int, k: Int, n: Int, w: Int = 8) {
}
class ChiselTPU(p: TPUParams) extends Module{
  val io = IO(new Bundle{
    val a = Flipped(Decoupled(Vec(p.m, Vec(p.k, SInt(p.w.W)))))
    val b = Flipped(Decoupled(Vec(p.k, Vec(p.n, SInt(p.w.W)))))
    val out = Output(Vec(p.m, Vec(p.n, SInt(p.w.W))))
  })
  val load :: fill :: multiply :: clear :: Nil = Enum(4)
  val state = RegInit(load)
  val (cycle, wrap) = Counter(state === multiply, p.k+p.m+p.k+1) //assume input is valid for now
  
  val actReg = Module(new ActReg(p))
  val systParams = TPUParams(p.k, io.a.bits.head.size, io.b.bits.head.size)
  val systArr = Module(new SystArr(systParams))
  val myOut = RegInit(VecInit.fill(p.m, p.n)(0.S(p.w.W)))
  val a_ready = RegInit(true.B)
  val b_ready = RegInit(true.B)

  io.a.ready := a_ready
  io.b.ready := b_ready
  io.out := myOut

  //initialize inputs for submodules
  actReg.io.index := (p.k+p.m-2).U-cycle
  systArr.io.a_in := RegNext(actReg.io.a_out)
  val act_in = RegInit(VecInit.fill(p.m, p.k)(0.S(p.w.W)))
  val syst_in = RegInit(VecInit.fill(p.k, p.n)(0.S(p.w.W)))
  actReg.io.a := act_in
  systArr.io.b_in := syst_in

  when(state === load){ 
      when(io.a.valid && io.a.ready){
        state := fill
        act_in := io.a.bits
        a_ready := false.B
      }
  }
  .elsewhen(state === fill){
      when(io.b.valid && io.b.ready){
        state := multiply
        syst_in := io.b.bits
        b_ready := false.B
      }
      cycle := 0.U
  }
  .elsewhen(state === multiply){
    when(cycle === (p.k+p.m+p.k).U){
      state := clear
    }
    for(i <- 0 until systArr.io.out.size){
      when(cycle > (p.n+i).U && cycle < (p.n+p.n+1+i).U){
        myOut(cycle - (p.n+i+1).U)(i.U) := systArr.io.out(i.U)
      }
    }
    printf(cf"MY TPU OUT: \n")
    for(i <- 0 until myOut.size){
      printf(cf"${myOut(i)}\n")
    }
  }
  .elsewhen(state === clear){
    printf(cf"FINISH\n")
    state := fill
    b_ready := true.B
    myOut := VecInit.fill(p.m, p.n)(0.S(p.w.W))
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
        val out = Output((Vec(p.n, SInt(p.w.W))))
        //debugOuts
        val cmp_debug = Output(Vec(p.k, Vec(p.n, SInt(p.w.W))))
        val b_reg_debug = Output(Vec(p.k, Vec(p.n, SInt(p.w.W))))
    })
    val a_reg = Reg(Vec(p.m, Vec(p.k, SInt(p.w.W))))
    val b_reg = Reg(Vec(p.k, Vec(p.n, SInt(p.w.W))))
    val cms_reg = Reg(Vec(p.m, Vec(p.k, SInt(p.w.W))))
    io.cmp_debug := cms_reg
    io.b_reg_debug := b_reg
    b_reg := io.b_in
    for(i <- 0 until p.n){
        io.out(i) := cms_reg(p.k-1)(i)
    }
    for(kidx <- 0 until p.k){
        for(nidx <- 0 until p.n){
            val cmp_input = Wire(SInt(p.w.W))
            if(kidx==0){
                if(nidx==0){
                    cmp_input := io.a_in(kidx) * b_reg(kidx)(nidx)
                    a_reg(kidx)(nidx) := io.a_in(kidx)
                }else{
                    cmp_input := a_reg(kidx)(nidx-1) * b_reg(kidx)(nidx)
                    a_reg(kidx)(nidx) := a_reg(kidx)(nidx-1)
                }
            }else{
                if(nidx==0){
                    cmp_input := (io.a_in(kidx) * b_reg(kidx)(nidx)) + cms_reg(kidx-1)(nidx)
                    a_reg(kidx)(nidx) := io.a_in(kidx)
                }else{
                    cmp_input := (a_reg(kidx)(nidx-1) * b_reg(kidx)(nidx)) + cms_reg(kidx-1)(nidx)
                    a_reg(kidx)(nidx) := a_reg(kidx)(nidx-1)
                }
            }
            cms_reg(kidx)(nidx) := cmp_input
        }
        
        
    }
    // printf(cf"MY A_IN:\n")
    // for(i <- 0 until io.a_in.size){
    //   printf(cf"${io.a_in(i)}\n")
    // }
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
    // }
}