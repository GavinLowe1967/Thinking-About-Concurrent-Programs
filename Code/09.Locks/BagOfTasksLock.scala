package tacp.trapezium

import ox.scl._
import Trapezium.Task

/* Code for the trapezium example, implementing the concurrent objects using
 * Locks. */

/** The bag of tasks object, using a Lock. */
class BagOfTasksLock(a: Double, b: Double, n: Long, nTasks: Int)
    extends BagOfTasks{ 
  // size of each interval
  private val delta = (b-a)/n

  // Number of tasks so far
  private var i = 0

  /** Lock protecting i. */
  private val lock = new Lock

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
    lock.mutex{ myI = i; i += 1} // Get and increment i.
    if(myI < nTasks) getTask(myI)
    else null
  }
}

// =======================================================

/** The Collector object, using a Lock. */
class CollectorLock extends Collector{
  /** The sum so far. */
  private var sum = 0.0

  /** Lock protecting sum. */
  private val lock = new Lock

  /** Add x to the result. */
  def add(x: Double) = lock.mutex{ sum += x }

  /** Get the result. */
  def get = lock.mutex{ sum }
}
