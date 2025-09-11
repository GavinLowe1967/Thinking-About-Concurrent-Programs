package tacp.dataParallel

import scala.util.Random

/** Test of SmoothShared, comparing it with the sequential implementation. */
object SmoothSharedTest{

  /** Do a single test. */
  def doTest = {
    val n = 10+Random.nextInt(40) // Height of image.
    val w = 10+Random.nextInt(30) // Width of image.
    val p = 1+Random.nextInt(10) // Number of workers.
    val maxIters = 10+Random.nextInt(40) // Maximum number of iterations.
    val a = Array.fill(n,w)(Random.nextFloat() >= 0.55)
    val a1 = a.map(_.clone)
    new SmoothShared(a, p, maxIters)()
    new SmoothSequential(a1, maxIters)()
    assert((0 until n).forall(i => a(i).sameElements(a1(i))),
           { Smooth.printArray(a); Smooth.printArray(a1); n })
  }

  def main(args: Array[String]) = {
    for(i <- 0 until 1000){ doTest; if(i%50 == 0) print(".") }
    println()
  }
}
