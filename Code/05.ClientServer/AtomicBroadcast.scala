package tacp.clientServer

import ox.scl._

/** Trait for an atomic broadcast. */
trait AtomicBroadcastT[A]{
  /** Synchronously broadcast x to all the receivers. */
  def send(x: A): Unit 

  /** Synchronously receive from the sender. */
  def receive(): A 

  def shutdown() = {}
}

// =======================================================

/** A class to allow a sender to synchronously broadcast to `n` receivers. */
class AtomicBroadcast[A](n: Int) extends AtomicBroadcastT[A]{
  private val fromSender = new SyncChan[(A, Chan[Unit])]

  private val fromReceiver = new SyncChan[Chan[A]]

  def send(x: A): Unit = {
    val c = new OnePlaceBuffChan[Unit]; fromSender!(x,c); c?()
  }

  def receive(): A = {
    val c = new OnePlaceBuffChan[A]; fromReceiver!c; c?()
  }

  private def server = thread{
    val queue = new scala.collection.mutable.Queue[Chan[A]]
    repeat{
      val (x,cs) = fromSender?()
      for(_ <- 0 until n) queue.enqueue(fromReceiver?())
      // All channels received.
      cs!(); for(_ <- 0 until n) queue.dequeue()!x
    }
  }

  fork(server)

  override def shutdown() = { fromSender.close(); fromReceiver.close() }
}


