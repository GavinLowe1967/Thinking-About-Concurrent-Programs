package tacp.jvmMonitors

import ox.scl._

/** A shared synchronous channel passing data of type A, implemented using a
  * JVM monitor. */
class JVMMonitorSyncChan[A] extends tacp.monitors.SyncChanT[A]{
  /** The current or previous value. */
  private var value = null.asInstanceOf[A]

  private val Empty = 0; private val Filled = 1; private val Read = 2

  /** The current stage of the exchange; is value (logically) empty, has it been
    * filled, or has it been read by the receiver? */
  private var stage = Empty

  def send(x: A) = synchronized{
    while(stage != Empty) wait() // Wait for previous value to be consumed (1).
    value = x; stage = Filled   // Deposit my value.
    notifyAll() // Signal to receiver at (2).
    while(stage != Read) wait() // Wait for receiver (3).
    stage = Empty; notifyAll() // Signal to sender on next round at (1).
  }

  def receive(): A = synchronized{
    while(stage != Filled) wait() // Wait for sender (2).
    stage = Read; notifyAll()     // Notify current sender at (3).
    value
  }
}
