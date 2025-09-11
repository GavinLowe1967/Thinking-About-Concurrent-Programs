package tacp.dataParallel

import ox.scl._

/** Object to find a maximum value in an array and its index. */
object ArrayMax{
  /** A candiate maximum value and its index. */
  type Pair = (Int,Int)

  /** Function used in combining barriers. */
  private def f(pair1: Pair, pair2: Pair): Pair = {
    val (max1,ix1) = pair1; val(max2,ix2) = pair2
    if(max1 >= max2) (max1,ix1) else (max2,ix2)
  }

  /** Find a maximum element in a and its index, using numWorkers workers. */
  def apply(a: Array[Int], numWorkers: Int): (Int, Int) = {
    val n = a.length; require(n >= numWorkers)
    var result: Pair = null
    val barrier = new CombiningBarrier[Pair](numWorkers, f)

    def worker(me: Int) = thread(s"worker($me)"){
      val start = me*n/numWorkers; val end = (me+1)*n/numWorkers
      assert(start < end)
      var maxIx = start; var max = a(start)
      for(i <- start+1 until end) if(a(i) > max){ maxIx = i; max = a(i) }
      val res = barrier.sync(me, (max,maxIx))
      if(me == 0) result = res 
    }

    run(|| (for(i <- 0 until numWorkers) yield worker(i)))
    result
  }
}

// =======================================================

import scala.util.Random

/** Test for ArrayMax. */
object ArrayMaxTest{
  /** Perform a single test. */
  def doTest = {
    val numWorkers = 1+Random.nextInt(20)
    val n = numWorkers + Random.nextInt(100)
    val a = Array.fill(n)(Random.nextInt(100))
    val (max,maxIx) = ArrayMax(a,numWorkers)
    assert(a(maxIx) == max && a.forall(_ <= max), 
      a.mkString(", ")+"\n"+(max,maxIx))
  }

  def main(args: Array[String]) = {
    for(i <- 0 until 10000){ doTest; if(i%100 == 0) print(".") }
    println()
  }
}
