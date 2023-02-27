package hw1

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.mutable.ArrayBuffer


// class TPUModelTester extends AnyFlatSpec with ChiselScalatestTester {
//     behavior of "getNextCSM"

//     it should "multiply a 3x3 matrix" in {
//         val p = TPUParams(3, 3, 3) //note: no params yet
//         var csm = Array.fill(p.n)( Array.fill(p.k)(0) )
//         val a1 = Array(Array(0, 0, 3, 1, 2),
//                 Array(0, 4, 5, 3, 0),
//                 Array(1, 4, 1, 0, 0))
//         val a_prev = Array(Array(0, 0, 0),
//                 Array(0, 0, 0),
//                 Array(0, 0, 0))
//         val b1 = Array(Array(2, 5, 5),
//                 Array(4, 1, 3),
//                 Array(2, 3, 1))
//         val m = TPUModel(p)
//         for(i <- 0 until (p.m+p.k-1) + p.k){
//             val output = m.getNextCSM(csm, a_prev, b1, if(a1(0).size>i){a1.map((x:Array[Int]) => x(a1(0).size-1-i))}else{Array.fill(a1.size)(0)})
//             // for(i <- output){
//             //     print(i)
//             //     print(" ")
//             // }
//             // print("\n")
//         }
//     }
// }
