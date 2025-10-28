package tacp.datatypes

import ox.scl._

import scala.collection.mutable.Queue

/** A partial queue implemented using a JVM monitor. */
class JVMMonitorPartialQueue[T] extends PartialQueue[T]{
  /** The current state of the queue. */
  private val queue = new Queue[T]

  /** Enqueue x. */
  def enqueue(x: T) = synchronized{ 
    queue.enqueue(x) 
    notify() // Signal to a waiting dequeue.
  }

  /** Try to dequeue, blocking while the queue is empty.  */
  def dequeue(): T = synchronized{ 
    while(queue.isEmpty) wait() // Wait for a value.
    queue.dequeue() 
  }
}
 
