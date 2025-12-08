package tacp.dataParallel

import ox.scl._

abstract class CombiningBarrierT[A](p: Int, f: (A,A) => A){
  def sync(me: Int, x: A): A
}

// =======================================================

/** A simple implementation of a combining barrier using a server. */
class ServerCombiningBarrier[A](p: Int, f: (A,A) => A)
    extends CombiningBarrierT[A](p, f){
  private val arrive, leave = new SyncChan[A]

  def sync(me: Int, x: A) = { arrive!x; leave?() }

  private def server = thread{
    repeat{
      var y = arrive?()
      for(i <- 1 until p){ val x = arrive?(); y = f(y,x) }
      for(i <- 0 until p) leave!y
    }
  }

  fork(server)

  def shutdown() = { arrive.close(); leave.close() }
}

