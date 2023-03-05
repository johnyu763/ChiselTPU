
package tpu
import tpu.ActRegModel.Matrix
import scala.collection.mutable.ArrayBuffer

object ActRegModel {
    type Matrix = Seq[Seq[Int]]
    def apply(a: Matrix, b: Matrix): ActRegModel = {
      assert(a.head.size == b.size)
      return new ActRegModel(a, b)
    }
}

class ActRegModel(a: Matrix, b: Matrix) {
    
    def getStagger(a: Matrix) : Matrix = {
      val width = (a.size + a.head.size)-1
      
      return Seq.tabulate(a.head.size){
        j => Seq.tabulate(width){
          i => {
            if(a.head.size-j-1-i <= 0 && a.size-j-1-i+a.head.size > 0){
              a(width - i - 1 - j)(j)
            }
            else{
              0
            }
          }
        }
      }
    }
}