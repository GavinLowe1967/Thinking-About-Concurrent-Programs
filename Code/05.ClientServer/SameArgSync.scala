package tacp.clientServer

import ox.scl._

/** A synchronisation object that allows a thread to synchronise with another
  * thread that submits the same argument to `sync`. */
class SameArgSync{
  /** A reply channel. */
  type ReplyChan = Chan[Unit]

  /** Channel on which clients send messages to the server. */
  private val toServer = new SyncChan[(Int, ReplyChan)]

  /** Synchronise with another thread that submits `n`. */
  def sync(n: Int) = {
    val reply = new OnePlaceBuffChan[Unit]; toServer!((n, reply)); reply?()
  }

  private def server = thread("server"){
    // Map holding pending requests
    val map = new scala.collection.mutable.HashMap[Int, ReplyChan]()
    repeat{
      val (n, reply) = toServer?()
      map.get(n) match{
        case Some(r1) => map -= n; reply!(); r1!()
        case None => map += n -> reply
      }
    }
  }

  fork(server)

  def shutdown = toServer.close()
}

// =======================================================

import scala.util.Random

object SameArgSyncTest{
  val N = 10  // sync is called on values in the range [0..N). 
  val p = 10 // # workers in each test

  // Events to be stored in the log
  trait LogEvent
  case class CallEvent(n: Int) extends LogEvent
  case class ReturnEvent(n: Int) extends LogEvent

  /** Worker that calls `sync(n)`, logging the calls. */
  def worker(me: Int, sync: SameArgSync, log: Log[LogEvent], n: Int) = 
    thread("worker"+me){
      log.add(me, CallEvent(n)); sync.sync(n); log.add(me, ReturnEvent(n))
    }

  /** Do a single test. */
  def doTest() = {
    val sync = new SameArgSync; val log = new Log[LogEvent](p)
    assert(p%2 == 0); val p2 = p/2   
    val ns = Array.fill(p2)(Random.nextInt(N)) // `worker(i)` will use `ns(i%p2)`
    run(|| (for(i <- 0 until p) yield worker(i, sync, log, ns(i%p2))))
    checkLog(log.get)
    sync.shutdown
  }

  /** Check the contents of the log. */
  def checkLog(log: Array[LogEvent]) = {
    // Bit map indicating those n for which a sync(n) is pending
    val pending = new Array[Boolean](N)
    // Count of how many sync(n)s can return
    val canReturn = new Array[Int](N)
    for(i <- 0 until log.length){
      log(i) match{
        case CallEvent(n) => 
          if(pending(n)){ pending(n) = false; canReturn(n) += 2 }
          else pending(n) = true
        case ReturnEvent(n) =>
          assert(canReturn(n) > 0, log.take(i+1).mkString("\n"))
          canReturn(n) -= 1
      }
    }
    assert(pending.forall(_ == false) && canReturn.forall(_ == 0))
  }

  def main(args: Array[String]) = {
    for(i <- 0 until 5000){ doTest(); if(i%100 == 0) print(".") }
    println()
  }
}
