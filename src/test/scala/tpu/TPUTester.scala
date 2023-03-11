package tpu

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import tpu.TPUModel.Matrix
import tpu.TPUParams

import scala.Array
import scala.util.Random
object TPUTestData {
  def genIdentity(n: Int): Matrix = Array.tabulate(n,n) { (i,j) => if (i==j) 1 else 0 }

  def genOnesRow(n: Int): Matrix = Array(Array.fill(n)(1))

  def genOnesCol(n: Int): Matrix = Array.fill(n)(Array(1))

  val in2x4  = Array(Array(1,2,3,4),
                   Array(5,6,7,8))
  val in4x2  = Array(Array(1,2),
                   Array(3,4),
                   Array(5,6),
                   Array(7,8))
  val out2x2 = Array(Array(50, 60),
                   Array(114,140))
  val out4x4 = Array(Array(11, 14, 17, 20),
                   Array(23, 30, 37, 44),
                   Array(35, 46, 57, 68),
                   Array(47, 62, 77, 92))

  val in5x3 = Array(Array(1, 2, 3),
                   Array(7, 8, 4),
                   Array(2, 3, 1),
                   Array(5, 1, 6),
                   Array(1, 6, 3))
  val in3x7 = Array(Array(1, 9, 0, 3, 4, 5, 1),
                   Array(8, 5, 6, 4, 1, 2, 3),
                   Array(3, 2, 5, 7, 5, 4, 7))
  val in7x5 = Array(Array(1, 9, 0, 3, 4, 5, 1),
                   Array(8, 5, 6, 4, 1, 2, 3),
                   Array(3, 2, 5, 7, 5, 4, 7),
                   Array(6, 7, 2, 9, 4, 3, 6),
                   Array(1, 9, 4, 3, 5, 7, 2),
                   Array(9, 8, 3, 5, 6, 1, 8),
                   Array(2, 4, 3, 1, 6, 5, 9))


  val inA3x3 = Array(Array(2, 3, 1),
                   Array(1, 5, 4),
                   Array(3, 4, 1))
  val inAShifted =  Array(Array(2, 1, 3, 0, 0),
                        Array(0, 3, 5, 4, 0),
                        Array(0, 0, 1, 4, 1))
  val inB3x3 = Array(Array(2, 5, 5),
                   Array(4, 1, 3),
                   Array(2, 3, 1))
  def genRand(rows: Int, cols: Int): Matrix = Array.fill(rows)( Array.fill(cols)(Random.nextInt(30)+1) )
}


class SystArrTester extends AnyFlatSpec with ChiselScalatestTester {
  def doSystArrTest_noShift(a: Matrix, aShifted: Matrix, b: Matrix): Unit = {
    val p = TPUParams(aShifted.size, a.head.size, b.head.size)
    test(new SystArr(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

      // load b_in with b matrix
    for (r <- 0 until p.k) {
        for (c <- 0 until p.n) {
            dut.io.b_in(r)(c).poke(b(r)(c).S)
        }
    }
    dut.clock.step()
    // print("\n---------------\n")
    // for (r <- 0 until p.k) {
    //     for (c <- 0 until p.n) {
    //         print(dut.io.b_reg_debug(r)(c).peek())
    //         print("\t")
    //     }
    //     print("\n")
    // }
      // wait for completion
    for(clock <- 0 until (p.m + p.n - 1) + p.n){
        if (clock < p.m + p.n - 1){
            for (idx <- 0 until p.m) {
                dut.io.a_in(idx).poke(aShifted(idx)(clock))
            }
        }else{
            for (idx <- 0 until p.m) {
                dut.io.a_in(idx).poke(0.S(p.w.W))
            }
        }
        // print("\n-------c_reg--------\n")
        // for (r <- 0 until p.k) {
        //     for (c <- 0 until p.n) {
        //         print(dut.io.cmp_debug(r)(c).peek())
        //         print("\t")
        //     }
        //     print("\n")
        // }
        dut.clock.step()
    }
      
    }
  }

  behavior of "TPU syst arr test"
  it should "multiply (1s row) x (1s column)" in {
    val k = 4
    doSystArrTest_noShift(TPUTestData.inA3x3, TPUTestData.inAShifted, TPUTestData.inB3x3)
  }
}

class TPUTester extends AnyFlatSpec with ChiselScalatestTester {
  def doTPUTest(a: Matrix, b: Matrix): Unit = {
    val p = TPUParams(a.size, a.head.size, b.head.size)
    print("params: \nm: ")
    print(a.size)
    print("\nk: ")
    print(a.head.size)
    print("\nn: ")
    print(b.head.size)
    print("\n\n")
    test(new ChiselTPU(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
    
    dut.io.a.valid.poke(true.B)
    dut.io.a.ready.expect(true.B)
      // load a with a matrix
    for (r <- 0 until p.m) {
        for (c <- 0 until p.k) {
            dut.io.a.bits(r)(c).poke(a(r)(c).S)
        }
    }
    
    dut.io.b.valid.poke(true.B)
    dut.io.b.ready.expect(true.B)
     // load b with b matrix
    for (r <- 0 until p.k) {
        for (c <- 0 until p.n) {
            dut.io.b.bits(r)(c).poke(b(r)(c).S)
        }
    }
    dut.clock.step()
        print("\n") 
        print("-----cycleIdx out-----\n")
        print(dut.io.debug_cycleIdx.peek())
        print("\n") 
        print("-----cycleIdxCols out-----\n")
        print(dut.io.debug_cycleIdxCols.peek())
        print("\n") 
        print("-----cycleIdxCols out-----\n")
        print(dut.io.debug_cycleIdxRows.peek())
        print("\n") 
        print("\n") 
    //for(i <- 0 until p.k+p.m+p.k+2){
    //len to feed a slanted all the way though a_in window, eg:
    //
    for(i <- 0 until 2+(p.k+p.m-1)+(p.n)){
        // print("-----BEFORE:arrRegs out-----\n")
        // for (cmp_i <- 0 until p.k) {
        //   print(dut.io.debug_a_out(cmp_i).peek())
        //   print("  ")
        // }
        // print("\n")
        // print("-----BEFORE:cycle out-----\n")
        // print(dut.io.debug_cycleOut.peek())
        // print("\n")
        // print("-----BEFORE:00-----\n")
        // print(dut.io.debug_00.peek())
        // print("\n")
        // print("-----b before is in syst arr:-----\n")
        // for (r <- 0 until p.k) {
        //     for (c <- 0 until p.n) {
        //         print(dut.io.debug_b_regs(r)(c).peek())
        //         print(" ")
        //     }
        //     print("\n")
        // }
        dut.clock.step()
        // print("-----b after is in syst arr:-----\n")
        // for (r <- 0 until p.k) {
        //     for (c <- 0 until p.n) {
        //         print(dut.io.debug_b_regs(r)(c).peek())
        //         print(" ")
        //     }
        //     print("\n")
        // }
        print("-----AFTER:cycle out-----\n")
        print(dut.io.debug_cycleOut.peek())
        print("\n")       
        // print("-----AFTER:00-----\n")
        // print(dut.io.debug_00.peek())
        // print("\n") 
        // print("-----arrRegs out-----\n")
        // for (cmp_i <- 0 until p.k) {
        //   print(dut.io.debug_a_out(cmp_i).peek())
        //   print("  ")
        // }
        // print("\n")
        // print("-----a regs-----\n")
        // for (cmp_i <- 0 until p.k) {
        //   for (cmp_j <- 0 until p.m) {
        //     print(dut.io.debug_a_regs(cmp_i)(cmp_j).peek())
        //   }
        //   print("\n")
        // }
        print("-----syst arr-----\n")
        for (cmp_i <- 0 until p.k) {
          for (cmp_j <- 0 until p.n) {
            print(dut.io.debug_1(cmp_i)(cmp_j).peek())
          }
          print("\n")
        }
        // print("\n")

        print("-----systreg out-----\n")
        for (cmp_i <- 0 until p.n) {
          print(dut.io.debug_systreg_out(cmp_i).peek())
          print("  ")
        }
        print("\n") 
        print("-----cycleIdx out-----\n")
        print(dut.io.debug_cycleIdx.peek())
        print("\n") 
        print("-----cycleIdxCols out-----\n")
        print(dut.io.debug_cycleIdxCols.peek())
        print("\n") 
        print("-----cycleIdxRows out-----\n")
        print(dut.io.debug_cycleIdxRows.peek())
        print("\n") 
        print("-----outReg-----\n")
        for (cmp_i <- 0 until p.m) {
          for (cmp_j <- 0 until p.n) {
            print(dut.io.out(cmp_i)(cmp_j).peek())
          }
          print("\n")
        }
        print("-----debug_systout_upperLim out-----\n")
        print(dut.io.debug_systout_upperLim.peek())
        print("\n") 
        print("\n")
        print("\n") 
    }

    //check output 1
    val expected = MatMulModel(p, a, b)
    for (r <- 0 until p.cRows) {
      for (c <- 0 until p.cCols) {
        dut.io.out(r)(c).expect(expected(r)(c).S)
      }
    }
    dut.clock.step()
    dut.io.b.valid.poke(true.B)
    dut.io.b.ready.expect(true.B)
     // load b with next b matrix
    for (r <- 0 until p.k) {
        for (c <- 0 until p.n) {
            dut.io.b.bits(r)(c).poke(b(r)(c).S)
        }
    }
    dut.clock.step()
    
    for(i <- 0 until p.k+p.m+p.k+3){
      dut.clock.step()
    }


    dut.clock.step()

    // print("\n---------------\n")
    // for (r <- 0 until p.k) {
    //     for (c <- 0 until p.n) {
    //         print(dut.io.b_reg_debug(r)(c).peek())
    //         print("\t")
    //     }
    //     print("\n")
    // }
    //   // wait for completion
    // for(clock <- 0 until (p.m + p.n - 1) + p.n){
    //     if (clock < p.m + p.n - 1){
    //         for (idx <- 0 until p.m) {
    //             dut.io.a_in(idx).poke(aShifted(idx)(clock))
    //         }
    //     }else{
    //         for (idx <- 0 until p.m) {
    //             dut.io.a_in(idx).poke(0.S(p.w.W))
    //         }
    //     }
    //     print("\n-------c_reg--------\n")
    //     for (r <- 0 until p.k) {
    //         for (c <- 0 until p.n) {
    //             print(dut.io.cmp_debug(r)(c).peek())
    //             print("\t")
    //         }
    //         print("\n")
    //     }
    //     dut.clock.step()
    // }
      
    }
  }

  behavior of "TPU test"
  it should "mult one cycle" in {
    // val k = 4
    doTPUTest(TPUTestData.inA3x3, TPUTestData.inB3x3)
    doTPUTest(TPUTestData.in2x4, TPUTestData.in4x2)
    doTPUTest(TPUTestData.in4x2, TPUTestData.in2x4)
    doTPUTest(TPUTestData.in5x3, TPUTestData.in3x7)
    doTPUTest(TPUTestData.in3x7, TPUTestData.in7x5)
  }
  it should "smult rand" in {
    // val k = 4
    val m = Random.nextInt(10)+1
    val k = Random.nextInt(10)+1
    val n = Random.nextInt(10)+1
    val m1 = TPUTestData.genRand(m, k)
    val m2 = TPUTestData.genRand(k, n)
    print("----------m1----------\n")
    for(r <- 0 until m1.size){
      for(c <- 0 until m1.head.size){
        print(m1(r)(c))
        print(" ")
      }
      print("\n")
    }
    print("----------m2----------\n")
    for(r <- 0 until m2.size){
      for(c <- 0 until m2.head.size){
        print(m2(r)(c))
        print(" ")
      }
      print("\n")
    }
    print("\n")
    print("\n")
    doTPUTest(m1, m2)
  }

  // behavior of "TPU test"
  // it should "stagger b" in {
  //   // val k = 4
  //   doTPUTest(TPUTestData.inA3x3, TPUTestData.inB3x3)
  // }
}