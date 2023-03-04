package tpu

//import CacheModel.CacheBlockModel
import scala.collection.mutable.ArrayBuffer

class TPUModel(p: TPUParams) {
    val m=p.m
    val k=p.k
    val n=p.n
    def staggerMatrix(m_in:Array[Array[Int]]) : Array[Array[Int]] = {
        val outWidth = (m_in(0).size + m_in.size)-1
        //val m_out = [[0 for i in range(outWidth)] for j in range(len(m_in[0]))]
        val m_out = Array.fill(m_in.size)( Array.fill(m_in(0).size)( 0 ) )
        var offset = 0
        for(j <- 0 until m_in(0).size){
            for(i <- m_in.size-1 until -1 by -1){
                m_out(j)(outWidth-i-1-offset) = m_in(i)(j)
            }
            offset = offset + 1
        }
        return m_out
    }
    def getNextCSM(csm_prevCyc:Array[Array[Int]], A_prevCyc:Array[Array[Int]], B:Array[Array[Int]], A_in:Array[Int]) : Array[Int] = {
        var csm_nextCyc = Array.fill(n)( Array.fill(k)(0) )
        var Ain_nextCyc = Array.fill(m)( Array.fill(n)(0) )
        //how the newInput reg should behave
        for(nidx <- 0 until n){
            for(kidx <- 0 until k){
                if(kidx == 0){
                    Ain_nextCyc(nidx)(kidx) = A_in(nidx)
                }else{
                    Ain_nextCyc(nidx)(kidx) = A_prevCyc(nidx)(kidx-1)
                }
            }
        }
        //how the csm reg should behave
        for(nidx <- 0 until n){
            for(kidx <- 0 until k){
                if(nidx == 0){
                    if(kidx == 0){
                        csm_nextCyc(nidx)(kidx) = A_in(nidx) * B(nidx)(kidx)
                    }else{
                        csm_nextCyc(nidx)(kidx) = A_prevCyc(nidx)(kidx-1) * B(nidx)(kidx)
                    }
                }else{
                    if(kidx == 0){
                        csm_nextCyc(nidx)(kidx) = (A_in(nidx) * B(nidx)(kidx)) + csm_prevCyc(nidx-1)(kidx)
                    }else{
                        csm_nextCyc(nidx)(kidx) = (A_prevCyc(nidx)(kidx-1) * B(nidx)(kidx)) + csm_prevCyc(nidx-1)(kidx)
                    }
                }
            }
        }
        //bookkeeping: make sure each input array is set (pass by ref:)
        for(nidx <- 0 until n){
            for(kidx <- 0 until k){
                csm_prevCyc(nidx)(kidx) = csm_nextCyc(nidx)(kidx)
            }
        }
        for(nidx <- 0 until n){
            for(kidx <- 0 until k){
                A_prevCyc(nidx)(kidx) = Ain_nextCyc(nidx)(kidx)
            }
        }
            print("\n")
            for(csm_row <- csm_nextCyc){
                for(i <- csm_row){
                    print(i)
                    print(" ")
                }
                print("\n")
            }
            print("\n")
        return csm_prevCyc(csm_prevCyc.size-1)
    }   
}


object TPUModel {
    type Matrix = Array[Array[Int]]

    def apply(p: TPUParams): TPUModel = {
        return new TPUModel(p)
    }
}
