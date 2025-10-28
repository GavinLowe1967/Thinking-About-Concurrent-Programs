package tacp.datatypes

import ox.scl._

import scala.collection.mutable.Queue

/** A bounded partial queue implemented using a JVM monitor. */
class JVMMonitorBoundedPartialQueue[T](bound: Int) extends PartialQueue[T]{
  /** The current state of the queue. */
  private val queue = new Queue[T]

  /** Enqueue x. */
  def enqueue(x: T) = synchronized{ 
    while(queue.length == bound) wait() // Wait for a space (1).
    queue.enqueue(x)
    // if(queue.length == 1) 
    notifyAll() // Signal to a waiting dequeue at (2).
  }

  /** Try to dequeue, blocking while the queue is empty.  */
  def dequeue(): T = synchronized{ 
    while(queue.isEmpty) wait() // Wait for a value (2).
    val result = queue.dequeue() 
    //if(queue.length == bound-1) 
    notifyAll() // Signal to a waiting enqueue at (1).
    result
  }
}
 
// Note: it's not clear that the commented-out tests before the notifyAlls are
// useful: they will only avoid a notifyAll when there are no waiting threads,
// which is presumably not expensive.
