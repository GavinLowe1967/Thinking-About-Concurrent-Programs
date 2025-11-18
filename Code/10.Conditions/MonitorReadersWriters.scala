package tacp.monitors
 
import ox.scl._

/** A straightforward solution to the readers and writers problem using an SCL
  * monitor.  This version allows writers to be starved. */
class MonitorReadersWriters0 extends ReadersWritersLock{
  /** Number of readers currently in the critical region. */
  private var readers = 0

  /** Number of writers currently in the critical region. */
  private var writers = 0

  /* Invariant: (readers = 0 || writers = 0) && writers <= 1. */

  private val lock = new Lock

  private val cond = lock.newCondition

  def readerEnter() = lock.mutex{
    cond.await(writers == 0)
    readers += 1
  }

  def readerExit() = lock.mutex{
    assert(writers == 0); readers -= 1; cond.signalAll()
  }

  def writerEnter() = lock.mutex{
    cond.await(readers == 0 && writers == 0)
    writers += 1
  }

  def writerExit() = lock.mutex{
    assert(writers == 1 && readers == 0); writers -= 1; cond.signalAll()
  }
}

// =======================================================

/** A solution to the readers and writers problem using an SCL monitor. */
class MonitorReadersWriters extends ReadersWritersLock{
  /** Number of readers currently in the critical region. */
  private var readers = 0

  /** Number of writers currently in the critical region. */
  private var writers = 0

  /* Invariant: (readers = 0 || writers = 0) && writers <= 1. */

  /** Number of readers waiting to enter. */
  private var readersWaiting = 0

  /** Number of writers waiting to enter. */
  private var writersWaiting = 0

  private val lock = new Lock

  /** Condition to signal to a waiting reader that it can now enter. */
  private val readerEnterC = lock.newCondition

  /** Condition to signal to a waiting writer that it can now enter. */
  private val writerEnterC = lock.newCondition

  def readerEnter() = lock.mutex{
    if(writers > 0 || writersWaiting > 0){ // Have to wait
      readersWaiting += 1
      readerEnterC.await(writers == 0)
      readersWaiting -= 1
    }
    readers += 1
  }

  def readerExit() = lock.mutex{
    assert(writers == 0); readers -= 1 
    if(readers == 0 && writersWaiting > 0) writerEnterC.signal()
  }

  def writerEnter() = lock.mutex{
    if(readers > 0 || writers > 0){ // Have to wait
      writersWaiting += 1
      writerEnterC.await(readers == 0 && writers == 0)
      writersWaiting -= 1
    }
    writers += 1
  }

  def writerExit() = lock.mutex{
    assert(writers == 1 && readers == 0); writers -= 1 
    if(readersWaiting > 0) readerEnterC.signalAll()
    else if(writersWaiting > 0) writerEnterC.signal()
  }
}
