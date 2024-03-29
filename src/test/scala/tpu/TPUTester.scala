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
  val in2x2 = Array(Array(1,2),
                   Array(3,4))
  val ain2x2 = Array(Array(2,3),
                   Array(1,5))
  val bin2x2 = Array(Array(5,0),
                   Array(3,0))
  val in2x4  = Array(Array(1,2,3,4),
                   Array(5,6,7,8))
  val in4x2  = Array(Array(1,2),
                   Array(3,4),
                   Array(5,6),
                   Array(7,8))
  val out2x2 = Array(Array(50, 60),
                   Array(114,140))

  // val alt2x2A = Array(Array(2,3),
  //                  Array(1,5))
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
    val p = TPUParams(a.size, a.head.size, b.head.size, a.size, a.head.size, b.head.size)
    test(new SystArr(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

      // load b_in with b matrix
    for (r <- 0 until p.k) {
        for (c <- 0 until p.n) {
            dut.io.b_in(r)(c).poke(b(r)(c).S)
        }
    }
    dut.clock.step()

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
  def doTPUTest(a: Matrix, b: Matrix, systM: Int, systK: Int, systN: Int): Unit = {
    val p = TPUParams(a.size, a.head.size, b.head.size, systM, systK, systN)
    // slice parameters
    // dimensions of padded input matrices
    val paddedMDim = if(p.m >= p.actM) p.actM*(((p.m-1)/p.actM)+1) else p.actM
    val paddedKDim = if(p.k >= p.s1) p.s1*(((p.k-1)/p.s1)+1) else p.s1
    val paddedNDim = if(p.n >= p.s2) p.s2*(((p.n-1)/p.s2)+1) else p.s2

    // used for calculating slice boundaries
    val numSlice = (paddedMDim/p.actM) * (paddedKDim/p.s1) * (paddedNDim/p.s2)
    print("numSlice: ")
    print(numSlice)
    print("\n")
    val maxM = if(p.m > p.actM) p.m else p.actM
    val maxK = if(p.k > p.s1) p.k else p.s1
    val maxN = if(p.n > p.s2) p.n else p.s2
    
    print(maxK)
    print("\n")
    print(maxN)
    print("\n")
    print(maxM)
    print("\n")

    val numCycles = 2+numSlice*(3+maxM+maxK+maxN)

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
    
    //Plz calculate how many cycles it takes
    for(i <- 0 until numCycles){
        dut.clock.step()
    }
    //check output 1
    val expected = MatMulModel(p, a, b)
    print("EXPECTED OUT\n")
    for (r <- 0 until p.cRows) {
      for (c <- 0 until p.cCols) {
        print(s"${expected(r)(c).S} ")
      }
      print("\n")
    }
    for (r <- 0 until p.cRows) {
      for (c <- 0 until p.cCols) {
        dut.io.out(r)(c).expect(expected(r)(c).S)
      }
    }
    dut.clock.step()
    dut.io.b.valid.poke(true.B)
    dut.io.b.ready.expect(true.B)
    }
  }

  behavior of "TPU test"
  it should "1" in {
    // val k = 4
    val a = TPUTestData.ain2x2
    val b = TPUTestData.bin2x2
    doTPUTest(a, b, a.size, a.head.size, b.head.size)
  }
  it should "test 1 different systarr size" in{
    val a = TPUTestData.ain2x2
    val b = TPUTestData.bin2x2
    doTPUTest(a, b, 1, 1, 1)
  }
  it should "2" in {
    // val k = 4
    val a = TPUTestData.in2x2
    val b = TPUTestData.in2x2
    doTPUTest(a, b, a.size, a.head.size, b.head.size)
  }
  it should "test 2 different systarr size" in {
    // val k = 4
    val a = TPUTestData.in2x2
    val b = TPUTestData.in2x2
    doTPUTest(a, b, 1, 1, 1)
  }
  it should "3" in {
    // val k = 4
    val a = TPUTestData.in2x4
    val b = TPUTestData.in4x2
    doTPUTest(a, b, a.size, a.head.size, b.head.size)
  }
  it should "test 3 different systarr size" in {
    // val k = 4
    val a = TPUTestData.in2x4
    val b = TPUTestData.in4x2
    doTPUTest(a, b, 1, 3, 1)
  }
  it should "4" in {
    // val k = 4
    val a = TPUTestData.in4x2
    val b = TPUTestData.in2x4
    doTPUTest(a, b, a.size, a.head.size, b.head.size)
  }
  it should "test 4 different systarr size" in {
    // val k = 4
    val a = TPUTestData.in4x2
    val b = TPUTestData.in2x4
    doTPUTest(a, b, 3, 1, 3)
  }
  it should "5" in {
    // val k = 4
    val a = TPUTestData.out4x4
    val b = TPUTestData.out4x4
    doTPUTest(a, b, a.size, a.head.size, b.head.size)
  }
  it should "test 5 different systarr size" in {
    // val k = 4
    val a = TPUTestData.out4x4
    val b = TPUTestData.out4x4
    doTPUTest(a, b, 3, 3, 3)
  }
  it should "6" in {
    // val k = 4
    val a = TPUTestData.in5x3
    val b = TPUTestData.in3x7
    doTPUTest(a, b, a.size, a.head.size, b.head.size)
  }
  it should "test 6 different systarr size" in {
    // val k = 4
    val a = TPUTestData.in5x3
    val b = TPUTestData.in3x7
    doTPUTest(a, b, 4, 2, 6)
  }
  it should "7" in {
    // val k = 4
    val a = TPUTestData.in3x7
    val b = TPUTestData.in7x5
    doTPUTest(a, b, a.size, a.head.size, b.head.size)
  }
  it should "test 7 different systarr size" in {
    // val k = 4
    val a = TPUTestData.in3x7
    val b = TPUTestData.in7x5
    doTPUTest(a, b, 2, 6, 4)
  }
  // it should "smult rand" in {
  //   // val k = 4
  //   val m = Random.nextInt(10)+1
  //   val k = Random.nextInt(10)+1
  //   val n = Random.nextInt(10)+1
  //   val m1 = TPUTestData.genRand(m, k)
  //   val m2 = TPUTestData.genRand(k, n)
  //   print("----------m1----------\n")
  //   for(r <- 0 until m1.size){
  //     for(c <- 0 until m1.head.size){
  //       print(m1(r)(c))
  //       print(" ")
  //     }
  //     print("\n")
  //   }
  //   print("----------m2----------\n")
  //   for(r <- 0 until m2.size){
  //     for(c <- 0 until m2.head.size){
  //       print(m2(r)(c))
  //       print(" ")
  //     }
  //     print("\n")
  //   }
  //   print("\n")
  //   print("\n")
  //   doTPUTest(m1, m2)
  // }

  behavior of "TPU test"
  it should "stagger b" in {
    // val k = 4
    val a = TPUTestData.inA3x3
    val b = TPUTestData.inB3x3
    doTPUTest(a, b, a.size, a.head.size, b.head.size)
  }
}