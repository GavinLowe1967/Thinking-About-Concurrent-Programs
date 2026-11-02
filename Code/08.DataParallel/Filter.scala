package tacp.dataParallel

import ox.scl._
import scala.collection.mutable.ArrayBuffer

/** Object that calculates the result of filtering a by p, using numWorkers
  * worker threads. */
class Filter[A: scala.reflect.ClassTag](
    a: Array[A], p: A => Boolean, numWorkers: Int){
  private val n = a.length

  /** Barrier to synchronise workers. */
  private val barrier = new Barrier(numWorkers)

  /** Channels to workers, indexed by the recipient's identity. */
  private val toWorkers = Array.fill(numWorkers)(new OnePlaceBuffChan[Int])

  /** Array that holds the final result. */
  private var result: Array[A] = null

  /** A single worker. */
  private def worker(me: Int) = thread{
    val start = me*n/numWorkers; val end = (me+1)*n/numWorkers
    val ab = new ArrayBuffer[A]
    for(i <- start until end; if p(a(i))) ab += a(i)
    val len = ab.length; var r = 0; var gap = 1; var s = len
    // Use prefix sum pattern to find sum of `len` values for threads up to
    // `me`.  Writing len(i) for the len value of worker(i), we have gap = 2^r
    // and s = \sum len(me-gap..me] (with fictitious values).
    while(gap < numWorkers){
      if(me+gap < numWorkers) toWorkers(me+gap)!s
      if(gap <= me) s += toWorkers(me)?()
      r += 1; gap += gap; barrier.sync(me)
    }
    // Worker numWorkers-1 creates the array.
    if(me == numWorkers-1) result = new Array[A](s)
    barrier.sync(me) // Wait for above to happen.
    // Copy ab into result[s-len..s)
    val offset = s-len  
    for(i <- 0 until len) result(offset+i) = ab(i)
  }

  /** Calculate the filter. */
  def apply(): Array[A] = {
    run(|| (for(i <- 0 until numWorkers) yield worker(i)))
    result
  }
}

// =======================================================

import scala.util.Random

object FilterTest{
  def even(x: Int) = x%2 == 0


  def doTest() = {
    val n = Random.nextInt(1000); val a = Array.fill(n)(Random.nextInt(1000))
    val numWorkers = 1+Random.nextInt(10)
    val seqRes = a.filter(even)
    val filter = new Filter(a, even, numWorkers); val concRes = filter()
    assert(seqRes.sameElements(concRes),
      "a = "+a.mkString(",")+"\nseqRes = "+seqRes.mkString(",")+
        "\nconcRes = "+concRes.mkString(","))
  }

  def main(args: Array[String]) = {
    for(i <- 0 until 10000){ doTest(); if(i%100 == 0) print(".") }
    println()
  }

}
