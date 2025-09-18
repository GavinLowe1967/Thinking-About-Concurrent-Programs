package tacp.datatypes

import ox.scl._

import scala.collection.mutable.Queue

/** A total queue implemented using a Lock. */
class LockTotalQueue[T] extends TotalQueue[T]{
  /** The current state of the queue. */
  private val queue = new Queue[T]

  /** A Lock to protect queue. */
  private val lock = new Lock

  /** Enqueue x. */
  def enqueue(x: T) = lock.mutex{ queue.enqueue(x) }

  /** Try to dequeue.  Return None if the queue is empty. */
  def dequeue(): Option[T] = lock.mutex{ 
    if(queue.isEmpty) None else Some(queue.dequeue()) 
  }

  /** Shutdown the queue. */
  def shutdown() = {} // Nothing to do.
}
