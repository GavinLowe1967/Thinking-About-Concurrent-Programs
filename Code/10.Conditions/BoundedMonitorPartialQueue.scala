package tacp.datatypes

import ox.scl._
import scala.collection.mutable.Queue

/** A bounded partial queue implemented as a monitor. */
class BoundedMonitorPartialQueue[T](bound: Int) extends PartialQueue1[T]{
  /** The queue itself. */
  private val queue = new Queue[T]

  /** A monitor object, to control the synchronisations. */
  private val lock = new Lock

  /** Condition for signalling that the queue is not full. */
  private val notFull = lock.newCondition

  /** Condition for signalling that the queue is not empty. */
  private val notEmpty = lock.newCondition

  /** Enqueue x.  Blocks while the queue is full. */
  def enqueue(x: T) = lock.mutex{
    notFull.await(queue.length < bound) 
    queue.enqueue(x)
    notEmpty.signal()
  }

  /** Dequeue a value.  Blocks until the queue is non-empty. */
  def dequeue(): T = lock.mutex{
    notEmpty.await(queue.nonEmpty) // Wait for a signal.
    val result = queue.dequeue()
    notFull.signal()
    result
  }
}
