package tacp.datatypes

import ox.scl._

import scala.collection.mutable.Queue

/** A total queue implemented using a JVM monitor. */
class MonitorPartialQueue[T] extends PartialQueue[T]{
  /** The current state of the queue. */
  private val queue = new Queue[T]

  /** Enqueue x. */
  def enqueue(x: T) = synchronized{ 
    queue.enqueue(x) 
    notify()
  }

  /** Try to dequeue.  Return None if the queue is empty. */
  def dequeue(): T = synchronized{ 
    while(queue.isEmpty) wait()
    queue.dequeue() 
  }

  // def shutdown() = {}
}
 
