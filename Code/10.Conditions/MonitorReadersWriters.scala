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
  /* This version aims to be fair.  A writer will not enter if a reader is
   * waiting; and a reader will not enter when a writer is waiting.  We
   * therefore need to keep track of the number of readers and writers that
   * are currently waiting.  However, when a writer exits, we want all of the
   * currently waiting readers to enter.  We therefore keep track of the
   * number of readers that have received a signal, but not yet actually
   * entered (as they are waiting to obtain the lock).  We don't allow a
   * writer to enter while this number is non-zero.  Similarly, we force a
   * writer to wait when writersWaiting > 0, in case some other waiting writer
   * has received a signal and is about to run.

   * Below, we can't use an await with a guard, as that would break the logic.
   * For example, using "readerEnterC.await(writers == 0)" would mean that the
   * reader wouldn't wait when writers = 0 and writersWaiting > 0.  Using
   * "readerEnterC.await(writers == 0 && writersWaiting == 0)", and similar in
   * writerEnter would lead to deadlock when both readers and writers are
   * waiting.   */

  /** Number of readers currently in the critical region. */
  private var readers = 0

  /** Number of writers currently in the critical region. */
  private var writers = 0

  /* Invariant: (readers = 0 || writers = 0) && writers <= 1. */

  /** Number of readers waiting to enter. */
  private var readersWaiting = 0

  /** Number of writers waiting to enter. */
  private var writersWaiting = 0

  /** Number of readers that have received a signal, but have not yet
    * entered. */
  private var readersToEnter = 0

  private val lock = new Lock

  /** Condition to signal to a waiting reader that it can now enter. */
  private val readerEnterC = lock.newCondition

  /** Condition to signal to a waiting writer that it can now enter. */
  private val writerEnterC = lock.newCondition

  def readerEnter() = lock.mutex{
    if(writers > 0 || writersWaiting > 0){ // Have to wait
      readersWaiting += 1
      readerEnterC.await()
      assert(writers == 0); readersToEnter -= 1
    }
    readers += 1
  }

  def readerExit() = lock.mutex{
    assert(writers == 0); readers -= 1 
    if(readers == 0 && writersWaiting > 0 && readersToEnter == 0)
      writerEnterC.signal()
  }

  def writerEnter() = lock.mutex{
    if(readers > 0 || writers > 0 || 
        readersWaiting > 0 || readersToEnter > 0 || writersWaiting > 0){
      // Have to wait
      writersWaiting += 1
      writerEnterC.await() // Wait for signal. 
      assert(readers == 0 && writers == 0)
      writersWaiting -= 1
    }
    writers += 1
  }

  def writerExit() = lock.mutex{
    assert(writers == 1 && readers == 0); writers -= 1 
    if(readersWaiting > 0){
      assert(readersToEnter == 0)
      readersToEnter = readersWaiting; readersWaiting = 0
      readerEnterC.signalAll() // Signal to waiting readers.
    }
    else if(writersWaiting > 0) writerEnterC.signal()
  }
}

//=======================================================

/*
/** A solution to the readers and writers problem using an SCL monitor. */
class MonitorReadersWriters2 extends ReadersWritersLock{
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

  /** Which type should get priority?  0 = readers, 1 = writers. */
  //private var priority = 0

  def readerEnter() = lock.mutex{
    if(writers > 0 || writersWaiting > 0){ // Have to wait
      readersWaiting += 1
      readerEnterC.await()
      readersWaiting -= 1
    }
    readers += 1
  }

  // private def signal() = 
  //   if(readersWaiting > 

  def readerExit() = lock.mutex{
    assert(writers == 0); readers -= 1 
    if(readers == 0 && writersWaiting > 0){ writers = 1; writerEnterC.signal() }
  }

  def writerEnter() = lock.mutex{
    if(readers > 0 || writers > 0){ // Have to wait
      writersWaiting += 1
      writerEnterC.await()
      assert(readers == 0 && writers == 1, s"$readers $writers")
      writersWaiting -= 1
    }
    else writers += 1
  }

  def writerExit() = lock.mutex{
    assert(writers == 1 && readers == 0); writers -= 1 
    if(readersWaiting > 0) readerEnterC.signalAll()
    else if(writersWaiting > 0){ writers = 1; writerEnterC.signal() }
  }
}
 */
