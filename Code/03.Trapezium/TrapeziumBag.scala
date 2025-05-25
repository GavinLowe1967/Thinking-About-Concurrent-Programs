package tacp.trapezium

import ox.scl._

/** Class to calculated the integral of f from a to b using n intervals.  The
  * calculation uses using nWorkers workers threads, and nTasks tasks.
  * @param buffering  If `buffered` > 0, use buffered channels with that 
  * capacity; if 0, use synchronous channels; if negative use unbounded
  * buffered channels. */
class TrapeziumBag(
  f: Double => Double, a: Double, b: Double, n: Long, 
  nWorkers: Int, nTasks: Int, buffering: Int = 0)
    extends TrapeziumT(f, a, b){
  require(0 < nTasks && nTasks <= n && n/nTasks < (1<<31)-1 )

  import Trapezium.{Task, mkChan}

  /** Channel from the controller to the workers, to distribute tasks. */
  private var toWorkers: Chan[Task] = mkChan[Task](buffering)

  /** Channel from the workers to the controller, to return sub-results. */
  private val toController: Chan[Double] = mkChan[Double](buffering)

  /** A worker, which repeatedly receives arguments from the distributor,
    * estimates the integral, and sends the result to the collector. */
  private def worker = thread("worker"){
    var myTotal = 0.0
    repeat{
      val (left, right, taskSize, delta) = toWorkers?()
      assert(taskSize > 0)
      myTotal += integral(left, right, taskSize, delta)
    }
    toController!myTotal
  }

  /** A distributor, who distributes tasks to the clients. */
  private def distributor = thread("distributor"){
    // size of each interval
    val delta = (b-a)/n
    // Number of intervals not yet allocated.
    var remainingIntervals = n
    var left = a // left hand boundary of next task
    for(i <- 0 until nTasks){
      // Number of intervals in the next task; the ceiling of
      // remainingIntervals/(nTasks-i).
      val taskSize = ((remainingIntervals-1) / (nTasks-i) + 1).toInt
      assert(taskSize > 0, s"$n; $nTasks")
      remainingIntervals -= taskSize
      val right = left+taskSize*delta
      toWorkers!(left, right, taskSize, delta)
      left = right
    }
    toWorkers.endOfStream()
  }

  /** This variable ends up holding the result. */
  private var result = 0.0

  /** A collector, that accumulates the sub-results into result. */
  private def collector = thread("collector"){
    result = 0.0
    for(i <- 0 until nWorkers) result += toController?()
  }

  /** The main system. */
  private def system = {
    val workers = || (for (i <- 0 until nWorkers) yield worker)
    workers || distributor || collector
  }

  def apply(): Double = { run(system); result } 
  // Note: apply should be called only once.  On a second call, the (new)
  // workers and distributor will throw a ChanClosed exception, but the
  // collector will hang.
}

// =======================================================
