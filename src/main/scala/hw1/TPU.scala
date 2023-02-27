package hw1
import chisel3._
import chisel3.util._
case class TPUParams(m: Int, k: Int, n: Int, w: Int = 8) {
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
}