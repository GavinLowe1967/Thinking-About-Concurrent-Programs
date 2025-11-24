package tacp.semaphores

import ox.scl._

import tacp.monitors.ReadersWritersLock


/** A solution to the readers and writers problem using semaphores, where
  * waiting writers are queued. */
class SemaphoreReadersWritersQueueLock extends ReadersWritersLock{
  /** Number of readers currently in the critical region. */
  private var readers = 0

  /** Number of writers currently in the critical region. */
  private var writers = 0

  /* Invariant: (readers = 0 || writers = 0) && writers <= 1. */

  /** Number of readers waiting to enter. */
  private var readersWaiting = 0

  private val mutex = new MutexSemaphore

  /** Semaphore to signal to a waiting reader that it can now enter.  A signal
    * indicates that writers = 0. */
  private val readerEnterS = new SignallingSemaphore

  /** Semaphores for signalling to waiting writers.  A signal indicates that
    * readers = 0 and writers = 0. */ 
  private val queue = new scala.collection.mutable.Queue[Semaphore]

  /* Invariant: if readers > 0, readersWaiting > 0, and mutex is up, then
   * queue.nonEmpty. */

  def readerEnter() = {
    mutex.down()
    if(writers > 0 || queue.nonEmpty){ // Have to wait
      readersWaiting += 1
      mutex.up(); readerEnterS.down() // Wait for signal (1).
      assert(writers == 0)
      readersWaiting -= 1
    }
    readers += 1
    if(readersWaiting > 0) readerEnterS.up() // Signal to next reader at (1).
    else mutex.up()
  }

  def readerExit() = {
    mutex.down()
    assert(writers == 0); readers -= 1 
    if(readers == 0 && queue.nonEmpty) 
      queue.dequeue().up() // Signal to writer at (2).
    else{ assert(readersWaiting == 0 || queue.nonEmpty);  mutex.up() } 
  }

  def writerEnter() = {
    mutex.down()
    if(readers > 0 || writers > 0){ // Have to wait
      val sem = new SignallingSemaphore; queue.enqueue(sem)
      mutex.up(); sem.down() // Wait for signal (2).
      assert(readers == 0 && writers == 0)
    }
    writers += 1
    mutex.up()
  }

  def writerExit() = {
    mutex.down()
    assert(writers == 1 && readers == 0); writers -= 1 
    if(readersWaiting > 0) readerEnterS.up() // Signal to reader at (1).
    else if(queue.nonEmpty) queue.dequeue().up() // Signal to writer at (2).
    else mutex.up()
  }
}
