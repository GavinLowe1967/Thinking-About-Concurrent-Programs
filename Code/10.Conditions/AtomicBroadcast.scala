package tacp.monitors

import ox.scl._

class AtomicBroadcast[A](n: Int) extends tacp.clientServer.AtomicBroadcastT[A]{
  require(n > 0)

  /** Number of receivers waiting (when slotFilled = false), or the number still
    * to leave (when slotFilled = true). */
  private var waiting = 0

  private var slot: A = _

  /** Is the value in slot valid?  This remains true until the last receiver has
    * read slot. */
  private var slotFilled = false

  private val lock = new Lock

  /** A Condition to signal to the sender that all the readers are waiting, and
    * so it can write its value and return. */
  private val canWrite = lock.newCondition

  /** A condition to signal to receivers that the previous synchronisation is
    * complete, so they can enter the main part of the operation. */
  private val canEnter = lock.newCondition

  /** A condition to signal to receivers that they can read the sender's
    * value. */
  private val canRead = lock.newCondition

  def send(x: A) = lock.mutex{
    canWrite.await(slotFilled == false && waiting == n) 
                                        // Wait for the receivers. (2)
    slot = x; slotFilled = true
    canRead.signalAll() // Signal to the receivers at (3).
  }

  def receive(): A = lock.mutex{
    canEnter.await(slotFilled == false) // Wait for previous round to finish. (1)
    waiting += 1
    if(waiting == n) 
      canWrite.signal() // Last receiver signals to the sender at (2).
    canRead.await() // Wait for the sender. (3)
    waiting -= 1
    if(waiting == 0){ 
      slotFilled = false 
      canEnter.signalAll() // Signal to receivers on the next round at (1).
    }
    slot
  }
}

