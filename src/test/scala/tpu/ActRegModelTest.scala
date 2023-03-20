
package tpu
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import tpu.ActRegModel
import tpu.ActRegModel.Matrix
import scala.collection.mutable.ArrayBuffer

object ActRegModelData {
  def genIdentity(n: Int): Matrix = Seq.tabulate(n,n) { (i,j) => if (i==j) 1 else 0 }

  def genOnesRow(n: Int): Matrix = Seq(Seq.fill(n)(1))

  def genOnesCol(n: Int): Matrix = Seq.fill(n)(Seq(1))

  val a  = Seq(Seq(1,2,3),
               Seq(4,5,6),
               Seq(7,8,9))

  val a2 = Seq(Seq(3,2,1),
               Seq(6,5,4),
               Seq(9,8,7))
  
  val a4x4 = Seq(Seq(1,2,3,4),
                Seq(5,6,7,8),
                Seq(9,10,11,12),
                Seq(13,14,15,16))

  val a4x3 = Seq(Seq(1,2,3),
                Seq(5,6,7),
                Seq(9,10,11),
                Seq(13,14,15))
  
  val a3x4 = Seq(Seq(1,2,3,4),
                Seq(5,6,7,8),
                Seq(9,10,11,12))

  val b  = Seq(Seq(2,5,5),
               Seq(4,1,3),
               Seq(2,3,1))

  val staggerAns = Seq(Seq(0, 0, 7, 4, 1), 
                       Seq(0, 8, 5, 2, 0), 
                       Seq(9, 6, 3, 0, 0))

  val staggerAns2 = Seq(Seq(0, 0, 9, 6, 3), 
                       Seq(0, 8, 5, 2, 0), 
                       Seq(7, 4, 1, 0, 0))

  val staggerAns4x4 = Seq(Seq(0, 0, 0, 13, 9, 5, 1), 
                        Seq(0, 0, 14, 10, 6, 2, 0), 
                        Seq(0, 15, 11, 7, 3, 0, 0), 
                        Seq(16, 12, 8, 4, 0, 0, 0))

  val staggerAns4x3 = Seq(Seq(0, 0, 13, 9, 5, 1), 
                        Seq(0, 14, 10, 6, 2, 0), 
                        Seq(15, 11, 7, 3, 0, 0))
  
  val staggerAns3x4 = Seq(Seq(0, 0, 0, 9, 5, 1), 
                        Seq(0, 0, 10, 6, 2, 0), 
                        Seq(0, 11, 7, 3, 0, 0), 
                        Seq(12, 8, 4, 0, 0, 0))                   
}


class ActRegModelTester extends AnyFlatSpec with ChiselScalatestTester {
  val sam = new ActRegModel(ActRegModelData.a, ActRegModelData.b)

  behavior of "ActRegModel"
  it should "get the staggered input array" in {
    assert(sam.getStagger(ActRegModelData.a) == ActRegModelData.staggerAns)
  }

  it should "get the staggered input array 2" in {
    assert(sam.getStagger(ActRegModelData.a2) == ActRegModelData.staggerAns2)
  }

  it should "get the staggered input array 4x4" in {
    assert(sam.getStagger(ActRegModelData.a4x4) == ActRegModelData.staggerAns4x4)
  }

  it should "get the staggered input array 4x3" in {
    assert(sam.getStagger(ActRegModelData.a4x3) == ActRegModelData.staggerAns4x3)
  }

  it should "get the staggered input array 3x4" in {
    assert(sam.getStagger(ActRegModelData.a3x4) == ActRegModelData.staggerAns3x4)
  }
}