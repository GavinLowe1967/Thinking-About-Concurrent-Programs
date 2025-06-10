package tacp.trapezium

import ox.scl._
import tacp.util.experiments.{Experiments,Graphs}

/** Concurrent program to multiply `a` and `b`, using bag of tasks where each
  * task is to calculate `taskSize` entries of the result, using `numWorkers`
  * workers. */
class MatrixMult(a: Array[Array[Int]], b: Array[Array[Int]],
             numWorkers: Int, taskSize: Int){
  private val n = a.size

  type Matrix = Array[Array[Int]]

  /** Array to store the result. */
  private var c = Array.ofDim[Int](n,n)

  trait Task

  /** The task to calculate rows [start..end). */
  case class MultiRowTask(start: Int, end: Int) extends Task

  /** The task to calculate entries [start..end) of row `row`. */
  case class SingleRowTask(row: Int, start: Int, end: Int) extends Task 

  /** Channel for sending tasks. */
  private val toWorkers = new SyncChan[Task] // (numWorkers)

  /** Calculate and store entry for c(i)(j). */
  private def calculate(i: Int, j: Int) = {
    var sum = 0; var k = 0
    while(k < n){ sum += a(i)(k)*b(k)(j); k += 1 }
    c(i)(j) = sum
  }

  /** A worker: repeatedly receive tasks, and calculate the relevant rows. */
  private def worker = thread{
    repeat{
      toWorkers?() match{
        case SingleRowTask(row, start, end) =>
          for(j <- start until end) calculate(row,j)
        case MultiRowTask(start, end) => 
          for(row <- start until end){
            var j = 0
            while(j < n){ calculate(row,j); j += 1 }
          }
      }
    }
  }

  private def server = thread{
    if(taskSize <= n){ // use SingleRowTasks.
      for(row <- 0 until n){
        var j = 0
        while(j < n){ 
          toWorkers!SingleRowTask(row, j, (j+taskSize) min n); j += taskSize
        }
      }
    }
    else{ // use MultiRowTasks.
      assert(taskSize%n == 0); val taskRows = taskSize/n // # rows per task.
      var row = 0
      while(row < n){
        toWorkers!MultiRowTask(row, (row+taskRows) min n); row += taskRows
      }
    }
    toWorkers.endOfStream()
  }

  def apply(): Matrix = {
    val workers = || (for(i <- 0 until numWorkers) yield worker)
    run(workers || server)
    c
  }
}

// =======================================================

import scala.util.Random

object Matrix{
  /** Max entry in arrays. */
  val Max = 100

  /** Create random matrix of size n. */
  def randomMatrix(n: Int) = Array.fill(n,n)(Random.nextInt(Max))

  def main(args: Array[String]) = {
    var n = -1; var numWorkers = -1; var taskSize = -1; var reps = 1
    var test = false; var timing = false
    // Parse arguments
    var i = 0
    while(i < args.length) args(i) match{
      case "-n" => n = args(i+1).toInt; i += 2
      case "--numWorkers" => numWorkers = args(i+1).toInt; i += 2
      case "--taskSize" => taskSize = args(i+1).toInt; i += 2
      case "--test" => test = true; i += 1
      case "--timing" => timing = true; i += 1
      case "--reps" => reps = args(i+1).toInt; i += 2
    }
    assert(n > 0 && numWorkers > 0 && taskSize > 0)

    var elapsed = 0L
    for(r <- 0 until reps){
      val a = randomMatrix(n); val b = randomMatrix(n)
      val start = java.lang.System.nanoTime
      val res = new MatrixMult(a, b, numWorkers, taskSize)()
      val end = java.lang.System.nanoTime
      elapsed += end-start
      if(test){
        for(i <- 0 until n; j <- 0 until n){
          var sum = 0; for(k <- 0 until n) sum += a(i)(k)*b(k)(j)
          assert(res(i)(j) == sum)
        }
      }
      if(r%20 == 0 && !timing) print(".")
    }
    if(timing) println(elapsed) else println()
  }
}

// =======================================================

object MatrixExperiment{
  val numWorkers = 8 // # worker threads

  val cmd0 =
    s"scala tacp.trapezium.Matrix --numWorkers $numWorkers --timing"

  val Million = 1_000_000

  var samples = 2 // # samples

  /** Values of n to use. */
  val ns = Array(64, 128, 256, 512, 1024) // Array(64, 256, 1024, 2048)

  /** Sizes of task to use. */
  val taskSizes = (4 to 19 by 1).map(1 << _).toArray
  // Array(8, 16, 32, 64, 128, 256, 512, 1024, 2048)

  def options = Array[String](
    //"title = Experiment determining optimal task size for the matrix multiplication problem",
    "ylabel = Time (ms)", "xlabel = Task size",
    "log basis x=2", "scaled ticks = false",
    "height = 0.8\\textheight", "width = 0.8\\textwidth"
  )

  def runExperiment: String = {
    val params = new Experiments.Params(samples, samples, 0.05, 0.01)
    val nN = ns.length; val nTaskSize = taskSizes.length
    val results = Array.ofDim[(Double,Double)](nN, nTaskSize)
    // Run experiments
    for(nIx <- 0 until nN; tsIx <- 0 until nTaskSize){
      val n = ns(nIx); val taskSize = taskSizes(tsIx)
      if(taskSize <= n*n){
        val reps = (1 << 21)/(n*n) max 1
        val cmd = cmd0+s" -n $n --taskSize $taskSize --reps $reps"; println(cmd)
        // Take a single sample
        def measure : Double = { // returns time in nanos
          val t = Experiments.timeProc(cmd, verbose = true)
          print(s"${t/Million}ms ") // print time in millis
          t.toDouble
        }
        val (m, s) = Experiments.iterateMeasurement(measure, params)
        println("\n("+m/Million+"ms, "+s/Million+"ms)")
        results(nIx)(tsIx) = (m/Million, s/Million)
      }
    }

    // Print results
    for(nIx <- 0 until nN; tsIx <- 0 until nTaskSize; 
        if results(nIx)(tsIx) != null){
      val (m,s) = results(nIx)(tsIx)
      println(s"(${ns(nIx)}, ${taskSizes(tsIx)}): \t"+m+" +- "+s)
    }

    // Produce graphs
    val labels = ns.map(n => s"n = $n")
    "{\\Large Experiment determining optimal task size for the matrix "+
      "multiplication problem}\n\n"+
      Graphs.makeLogXGraph(options, labels, taskSizes, results)

/*
    // Produce graphs, with a separate set of axes for each value of n.  
    var graphString = 
      "{\\Large Experiment determining optimal task size for the matrix "+
        "multiplication problem}\n\n"
    for(nIx <- 0 until nN){
      val n = ns(nIx); val results1 =  Array(results(nIx))
      graphString += 
        Graphs.makeLogXGraph(options, Array(s"n = $n"), taskSizes, results1)+"\n\n"
    }
    graphString
 */
  }

  def main(args: Array[String]) = {
    var i = 0
    while(i < args.length) args(i) match{
      case "--samples" => samples = args(i+1).toInt; i += 2
    }

    val fname = "Graphs/MatrixExperiment.tex"
    val graphString = runExperiment
    Graphs.writeStandAloneFile(fname, graphString)

  }
}
