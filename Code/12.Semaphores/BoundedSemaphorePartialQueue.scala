package tacp.datatypes

import ox.scl._

/** A queue implemented using a counting semaphore. */
class BoundedSemaphorePartialQueue[T](n: Int) extends PartialQueue[T]{
  /** The queue itself. */
  private val queue = new scala.collection.mutable.Queue[T]

  /** Semaphore for enqueuing.  The state of the semaphore equals the number of
    * free spaces. */
  private val spaces = new CountingSemaphore(n)

  /** Semaphore for dequeueing.  The state of the semaphore equals
    * queue.length. */
  private val size = new CountingSemaphore(0)

  /** Semaphore to provide mutual exclusion. */
  private val mutex = new MutexSemaphore

  def enqueue(v: T) = {
    spaces.down(); mutex.down()
    queue.enqueue(v)
    size.up(); mutex.up()
  }

  def dequeue(): T = {
    size.down(); mutex.down()
    val result = queue.dequeue()
    mutex.up(); spaces.up()
    result
  }
}
 
