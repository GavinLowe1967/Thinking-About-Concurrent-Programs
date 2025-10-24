package tacp.datatypes

import ox.scl._

import scala.collection.mutable.Queue

/** A total queue implemented using a JVM monitor. */
class MonitorTotalQueue[T] extends TotalQueue[T]{
  /** The current state of the queue. */
  private val queue = new Queue[T]

  /** Enqueue x. */
  def enqueue(x: T) = synchronized{ queue.enqueue(x) }

  /** Try to dequeue.  Return None if the queue is empty. */
  def dequeue(): Option[T] = synchronized{ 
    if(queue.isEmpty) None else Some(queue.dequeue()) 
  }
}
 
