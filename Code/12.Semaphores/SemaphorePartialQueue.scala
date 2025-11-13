package tacp.datatypes

import ox.scl._
import scala.collection.mutable.Queue

import tacp.datatypes.PartialQueue

class SemaphorePartialQueue[T] extends PartialQueue[T]{
  /** The queue itself. */
  private val queue = new Queue[T]

  /** Number of waiting dequeues. */
  private var waitingDequeues = 0

  /** Semaphore to provide mutual exclusion. */
  private val mutex = new MutexSemaphore
  /* If mutex is not fair, then sometimes the queue is "hogged" by enqueues.
   * This causes problems with linearizability testing.  Even with this
   * fairness, it's best to keep tests short, e.g. with --iters 30. */

  /** Semaphore on which a dequeue waits until the queue is non-empty. */
  private val nonEmptyS = new SignallingSemaphore

  /** Enqueue x. */
  def enqueue(x: T) = {
    mutex.down()
    queue.enqueue(x)
    if(waitingDequeues > 0) nonEmptyS.up() // Pass the baton to a dequeue.
    else mutex.up()
  }

  /** Dequeue a value. */
  def dequeue(): T = {
    mutex.down()
    if(queue.isEmpty){  // Thread has to wait.
      waitingDequeues += 1; mutex.up()
      nonEmptyS.down() // Wait for signal.
      assert(queue.length == 1)
      waitingDequeues -= 1
    }
    val result = queue.dequeue(); mutex.up(); result
  }
}

// -------------------------------------------------------

/** A queue implemented using a counting semaphore. */
class CountingSemaphorePartialQueue[T] extends PartialQueue[T]{
  /** The queue itself. */
  private val queue = new scala.collection.mutable.Queue[T]

  /** Semaphore for mutual exclusion on queue. */
  private val mutex = new MutexSemaphore()

  /** Semaphore for dequeueing.  The state of the semaphore equals
    * queue.length. */
  private val size = new CountingSemaphore(0)

  def enqueue(v: T) = {
    mutex.down()
    queue.enqueue(v)
    size.up()
    mutex.up()
  }

  def dequeue(): T = {
    size.down()
    mutex.down()
    val result = queue.dequeue()
    mutex.up()
    result
  }
}


