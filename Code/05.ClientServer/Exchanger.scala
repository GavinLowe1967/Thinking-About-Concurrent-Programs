package tacp.clientServer

import ox.scl._

/** The trait for an exchanger. */
trait ExchangerT[A]{
  /** Exchange x with another thread. */
  def exchange(x: A): A

  /** Shutdown the object. */
  def shutdown(): Unit
}

trait Exchanger1T[A] extends ExchangerT[A]{
  def shutdown() = {}
}


class Exchanger[A] extends ExchangerT[A]{
  /** Reply channels, for the server to return results to clients. */
  private type ReplyChan = OnePlaceBuffChan[A]

  /** Channel from clients to the server. */
  private val toServer = new SyncChan[(A, ReplyChan)]

  /** Exchange x with another thread. */
  def exchange(x: A): A = {
    val c = new ReplyChan; toServer!(x, c); c?()
  }

  /** The server thread. */
  private def server = thread("Exchanger"){
    repeat{
      val (x1, c1) = toServer?(); val (x2, c2) = toServer?()
      c1!x2; c2!x1
    }
  }

  fork(server)

  /** Shut down the server. */
  def shutdown() = toServer.endOfStream()
}
 
