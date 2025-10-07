package tacp.datatypes

import ox.scl._
import scala.collection.mutable.Queue

/** A partial queue implemented as a monitor. */
class MonitorPartialQueue[T] extends PartialQueue1[T]{
  /** The queue itself. */
  private val queue = new Queue[T]

  /** A monitor object, to control the synchronisations. */
  private val lock = new Lock

  /** Condition for signalling that the queue is not empty. */
  private val notEmpty = lock.newCondition

  /** Enqueue x. */
  def enqueue(x: T) = lock.mutex{
    queue.enqueue(x)
    notEmpty.signal()
  } 

  /** Dequeue a value.  Blocks until the queue is non-empty. */
  def dequeue(): T = lock.mutex{
    notEmpty.await(queue.nonEmpty) // wait for a signal
    queue.dequeue()
  }

}
