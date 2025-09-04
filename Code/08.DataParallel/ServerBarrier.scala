package tacp.dataParallel

import ox.scl._

trait BarrierT{
  def sync(me: Int): Unit
}

/** A simple implementation of a barrier using a server. */
class ServerBarrier(p: Int) extends BarrierT{
  private val arrive, leave = new SyncChan[Unit]
  /* Note: we can't use buffered channels here: if we did, a fast thread might
   * call sync on the next round, send on arrive, and receive a value from
   * leave intended for the previous round. */

  def sync(me: Int) = { arrive!(); leave?() }

  private def server = thread{
    repeat{
      for(i <- 0 until p) arrive?()
      for(i <- 0 until p) leave!()
    }
  }

  fork(server)

  def shutdown() = { arrive.close(); leave.close() }
}
