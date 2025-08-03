package tacp.interactingPeers

import ox.scl._
import scala.util.Random

object SumTest{
  /** Number of repetitions. */
  val reps = 10000

  /** Array that will hold values chosen by threads, indexed by thread IDs. */
  var xs: Array[Int] = null

  /** Array that will hold results obtained by threads, indexed by thread
    * IDs. */
  var results: Array[Int] = null

  /** A worker thread. */
  def worker(me: Int, summer: Sum) = thread("Thread"+me){
    // Choose value randomly.
    val x = Random.nextInt(1000); xs(me) = x
    val sum = summer(me, x); results(me) = sum
  }

  /** Run a single test. 
    * @param mkSum a function that, given n, creates the Sum object for 
    * n threads.
    * @param minN the minimum value allowed for n. */
  def runTest(mkSum: Int => Sum, minN: Int) = {
    val n = minN+Random.nextInt(20) // Number of peers.
    // println(s"n = $n")
    xs = new Array[Int](n); results = new Array[Int](n); val summer = mkSum(n) 
    run(|| (for (i <- 0 until n) yield worker(i, summer)))
    // Check results.
    val sum = xs.sum
    assert(results.forall(_ == sum),
           "xs = "+xs.mkString(", ")+"\nresults = "+results.mkString(", "))
  }

  def main(args : Array[String]) = {
    // Parse argument.
    if(args.isEmpty){ println("Type of Sum object not specified"); sys.exit() }
    val (mkSum, minN): (Int => Sum, Int) = args(0) match{
      case "--centralised" => (new Centralised(_), 1)
      case "--symmetric" => (new Symmetric(_), 1)
      case "--ring" => (new Ring(_), 2) 
      case "--ringSym" => (new RingSym(_), 1) 
      case "--tree" => (new Tree(_), 1) 
      case arg => (null, -1)
    }
    if(mkSum == null){
      println("Pattern argument not recognised: "+args(0)); sys.exit()
    }

    // Run tests
    for(r <- 0 until reps){
      runTest(mkSum, minN)
      if(r%200 == 0) print(".")
    }
    println()
  }
}
