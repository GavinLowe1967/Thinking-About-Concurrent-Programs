package tacp.trapezium

import ox.scl._

import Trapezium.{Task, mkChan}

/** Class to calculated the integral of f from a to b using n intervals.  The
  * calculation uses using nWorkers workers threads, and nTasks tasks.  This
  * version encapsulates the concurrency within objects.
  * @param buffering  If `buffered` > 0, use buffered channels with that 
  * capacity; if 0, use synchronous channels; if negative use unbounded
  * buffered channels. */ 
class TrapeziumBagObjects(
  f: Double => Double, a: Double, b: Double, n: Long, 
  nWorkers: Int, nTasks: Int, buffering: Int = 0, 
  useMonitor: Boolean = false, useLock: Boolean = false)
    extends TrapeziumT(f, a, b){
  require(0 < nTasks && nTasks <= n && n/nTasks < (1<<31)-1 )

  /** A worker, which repeatedly receives tasks from the BagOfTasks, estimates
    * the integral, and adds the result to the Collector. */
  private def worker(bag: BagOfTasks, collector: Collector) = thread("worker"){
    var myTotal = 0.0; var done = false
    while(!done) bag.getTask() match{
      case null => done = true
      case (left, right, taskSize, delta) =>
        assert(taskSize > 0)
        myTotal += integral(left, right, taskSize, delta)
    }
    collector.add(myTotal)
  }

  /** Calculate the integral. */
  def apply(): Double = {
    val bag: BagOfTasks = 
      if(useMonitor) new BagOfTasksMonitor(a, b, n, nTasks) 
      else if(useLock) new BagOfTasksLock(a, b, n, nTasks)
      else new BagOfTasksChannels(a, b, n, nTasks, nWorkers, buffering)
    val collector: Collector = 
      if(useMonitor) new CollectorMonitor
      else if(useLock) new CollectorLock
      else new CollectorChannels(nWorkers, buffering)
    val workers = || (for (i <- 0 until nWorkers) yield worker(bag, collector))
    run(workers)
    collector.get
  }
}

// ==================================================================


/** The trait for the bag of tasks objects. */
trait BagOfTasks{
  def getTask(): Task
}

// =================================================================

/** The bag of tasks object implemented using channels. */
class BagOfTasksChannels(
  a: Double, b: Double, n: Long, nTasks: Int, numWorkers: Int, buffering: Int)
    extends BagOfTasks{

  /** Channel from the controller to the workers, to distribute tasks. */
  private val toWorkers = mkChan[Task](buffering)

  /** Get a task.
    * @throws Stopped exception if there are no more tasks. */
  def getTask(): Task = toWorkers?() 

  /** A server process, that distributes tasks. */
  private def server = thread("bag of tasks"){
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
    for(i <- 0 until numWorkers) toWorkers!null
  }

  // Start the server running
  fork(server)
}

// ------------------------------------------------------------------

/** The bag of tasks object implemented using a monitor. */
class BagOfTasksMonitor(a: Double, b: Double, n: Long, nTasks: Int)
    extends BagOfTasks{
  // size of each interval
  private val delta = (b-a)/n

  // Number of tasks so far
  private var i = 0

  /** Boundary of the ith task.  The number of intervals in tasks [0..i). */
  @inline private def boundary(i: Int) = n*i/nTasks

  @inline private def getTask(i: Int): Task = {
    val b1 = boundary(i); val b2 = boundary(i+1)
    val taskSize = b2-b1; val left = a+b1*delta; val right = a+b2*delta
    (left, right, taskSize.toInt, delta)
  }

  /** Get a task.
    * @throws Stopped exception if there are no more tasks. */
  def getTask() = {
    var myI = -1
    synchronized{ myI = i; i += 1} // get and increment i
    if(myI < nTasks) getTask(myI)
    else null
  }
}

// ==================================================================

/** A collector object that receives sub-results from the workers, and adds
  * them up. */
trait Collector{
  /** Add x to the result. */
  def add(x: Double): Unit

  /** Get the result. */
  def get: Double
}

// ------------------------------------------------------------------

/** A collector object that receives sub-results from the workers, and adds
  * them up, using channels. */
class CollectorChannels(nWorkers: Int, buffering: Int) extends Collector{
  /** Channel from the workers to the controller, to return sub-results. */
  private val toController = mkChan[Double](buffering)

  /** Channel that sends the final result. */
  private val resultChan = new SyncChan[Double]

  /** A collector, that accumulates the sub-results. */
  private def server = thread("collector"){
    var result = 0.0
    for(i <- 0 until nWorkers) result += toController?()
    resultChan!result
  }

  // Start the server running
  fork(server)

  /** Add x to the result. */
  def add(x: Double) = toController!x

  /** Get the result. */
  def get: Double = resultChan?()
}

// ------------------------------------------------------------------

class CollectorMonitor extends Collector{
  /** The sum so far. */
  private var sum = 0.0

  /** Add x to the result. */
  def add(x: Double) = synchronized{ sum += x }

  /** Get the result. */
  def get = synchronized{ sum }

}
