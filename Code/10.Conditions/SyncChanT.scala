package tacp.monitors

import ox.scl._

/** Trait for a synchronous channel. */
trait SyncChanT[A]{
  /** Send x, synchronously. */
  def send(x: A): Unit

  /** Receive a value. */
  def receive(): A
}
