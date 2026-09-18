package tacp.trapezium

import ox.scl._

/** Class to find the maximum of `a`, using the bag-of-tasks pattern, with
  * `numWorkers` workers, and tasks of size `taskSize`. */
class ArrayMax(a: Array[Int], numWorkers: Int, taskSize: Int){
  require(a.nonEmpty && numWorkers > 0 && taskSize > 0)

  /** A Task (l,r) represents the task of calculating the maximum of a[l..r). */
  type Task = (Int, Int)

  /** Channel from the controller to workers. */
  private val toWorkers = new BuffChan[Task](numWorkers)

  /** Channel from the workers to the controller. */
  private val toController = new BuffChan[Int](numWorkers)

  /** A worker thread. */
  private def worker = thread("worker"){
    var myMax = Int.MinValue
    repeat{
      val (l,r) = toWorkers?()
      for(i <- l until r) myMax = myMax max a(i)
      }
    toController!myMax
  }

  /** The maximum value so far. */
  private var theMax = Int.MinValue

  /** The server. */
  private def controller = thread{
    // Distribute tasks.
    var l = 0; val n = a.length
    while(l < n){ val r = (l+taskSize) min n; toWorkers!(l,r); l = r } 
    toWorkers.endOfStream()
    // Receive workers' maxima, and combine.
    for(_ <- 0 until numWorkers) theMax = theMax max toController?()
  }

  /** The system. */
  private def system = 
    controller || (|| (for (_ <- 0 until numWorkers) yield worker))

  /** Find the maximum. */
  def apply(): Int = { run(system); theMax }
}

// =======================================================

import scala.util.Random

/** A test program for ArrayMax. */
object ArrayMaxTest{
  /** Perform a single test. */
  def doTest() = {
    val a = Array.fill(1+Random.nextInt(1000))(Random.nextInt(Int.MaxValue))
    val numWorkers = 1+Random.nextInt(20); val taskSize = 1+Random.nextInt(100)
    val max = new ArrayMax(a, numWorkers, taskSize)(); 
    assert(max == a.max, a.mkString(",")+s"\nmax = $max")
  }

  def main(args: Array[String]) = {
    for(i <- 0 until 5000){ doTest(); if(i%100 == 0) print(".") }
    println()
  }
}

// =======================================================

import java.lang.System.nanoTime

/** Tuning experiment for ArrayMax. 
  * The task size can be set via a "--taskSize" flag. */
object ArrayMaxExperiment{
  val Million = 1000000

  /** Size of each array. */
  var size = 10*Million

  /** Number of workers. */
  var numWorkers = 8

  /** Size of each task.  Can be set on the command line. */
  var taskSize = 1000

  /** Number of repetitions. */
  var reps = 100

  def runExperiment() = {
    // We reuse the same array on each iteration, so avoid the cost of
    // generating the new arrays.  This houldn't affect the time for running
    // ArrayMax.
    val a = Array.fill(size)(Random.nextInt(Int.MaxValue))
    val start = nanoTime
    for(i <- 0 until reps){ val max = new ArrayMax(a, numWorkers, taskSize)() }
    val duration = nanoTime-start
    println(duration/Million)
  }

  def main(args: Array[String]) = {
    var i = 0
    while(i < args.length) args(i) match{
      case "--taskSize" => taskSize = args(i+1).toInt; i += 2
    }
    runExperiment()
  }


}
