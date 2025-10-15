package tacp.monitors

import ox.scl._

class MonitorExchanger[A] extends tacp.clientServer.Exchanger1T[A]{  
  /* stage stores an indication of what stage we are at in an exchange, one of
   * the following values. */
  private val Empty = 0 // no current thread (slot is invalid)
  private val Filled = 1 // the value in slot has been deposited by the first
                         // thread.
  private val Exchanging = 2 // an exchange is underway: the second thread has
                             // returned; the first thread should take the
                             // value in slot.
  private var stage = Empty

  /** A piece of data currently being exchanged, when stage != Empty. */
  private var slot = null.asInstanceOf[A]

  private val lock = new Lock

  /** Condition to signal to a thread waiting to exchange. */
  private val canStart = lock.newCondition

  /** Condition for second thread to signal to first thread that it has swapped
    * values. */
  private val swapped = lock.newCondition

  def exchange(x: A): A = lock.mutex{
    canStart.await(stage != Exchanging) // Wait for previous exchange to
                                        // finish (1).
    if(stage == Empty){
      slot = x; stage = Filled // Deposit my value. 
      canStart.signal() // Signal to second thread at (1).
      swapped.await(); assert(stage == Exchanging) // Wait for second thread (2).
      stage = Empty; canStart.signal() // Signal to next first thread at (1).
      slot
    }
    else{
      assert(stage == Filled)
      val result = slot; slot = x // Swap first thread's value with mine.
      stage = Exchanging; swapped.signal() // Signal to first thread at (2)
      result
    }
  }
}
