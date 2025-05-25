package tacp.trapezium

import ox.scl._

/** Class to calculated the integral of `f` from `a` to `b`, using `n`
  * intervals.  The calculation uses `nWorkers` worker threads, each of whom
  * considers a single range. 
  * @param buffering  If `buffered` > 0, use buffered channels with that 
  * capacity; if 0, use synchronous channels; if negative use unbounded
  * buffered channels. */
class Trapezium(
  f: Double => Double, a: Double, b: Double, n: Long, 
  nWorkers: Int, buffering: Int = 0)
    extends TrapeziumT(f, a, b){
  require(n >= nWorkers)
  import Trapezium.{Task, mkChan}

  /** Channel from the controller to the workers, to distribute tasks. */
  private val toWorkers: Chan[Task] = mkChan[Task](buffering)

  /** Channel from the workers to the controller, to return sub-results. */
  private val toController: Chan[Double] = mkChan[Double](buffering)

  /** A worker, which receives arguments from the controller, estimates the
    * integral, and returns the results. */
  private def worker = thread("worker"){
    val (left, right, taskSize, delta) = toWorkers?()
    val result = integral(left, right, taskSize, delta)
    toController!result
  }

  /** This variable ends up holding the result. */
  private var result = 0.0

  /** A controller, who distributes tasks to the clients, and accumulates the
    * sub-results into result. */
  private def controller = thread("controller"){
    // size of each interval
    val delta = (b-a)/n
    // Number of intervals not yet allocated.
    var remainingIntervals = n
    var left = a // left hand boundary of next task
    for(i <- 0 until nWorkers){
      // Number of intervals in the next task; the ceiling of
      // remainingIntervals/(nWorkers-i).
      val taskSize = ((remainingIntervals-1) / (nWorkers-i) + 1).toInt
      remainingIntervals -= taskSize
      val right = left+taskSize*delta
      toWorkers!(left, right, taskSize, delta)
      left = right
    }

    // Receive results, and add them up
    result = 0.0
    for(i <- 0 until nWorkers) result += toController?()
  }    
    
  /** The main system. */
  private def system = {
    val workers = || (for (i <- 0 until nWorkers) yield worker)
    workers || controller
  }

  /** Calculate the integral, and return the result. */
  def apply(): Double = { run(system); result } 
}

// =======================================================

/** Companion object, collecting some code that is common across
  * implementations. */
object Trapezium{
  /** Type of tasks to send to client.  The Task (left, right, taskSize, delta)
    * represents the task of calculating the integral from left to right,
    * using taskSize intervals of size delta. */
  type Task = (Double, Double, Int, Double)

  /** Make a channel, based on the value of `buffering`. */
  def mkChan[A: scala.reflect.ClassTag](buffering: Int): Chan[A] = 
    if(buffering == 0) new SyncChan[A] 
    else if(buffering == 1) new OnePlaceBuffChan[A]
    else if(buffering > 1) new BuffChan[A](buffering) 
    else new UnboundedBuffChan[A]
}
