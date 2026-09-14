package tacp.clientServer

import ox.scl._

/** A timed exchanger, with a timeout time of `delay` nanoseconds. */
class TimedExchanger[A](delay: Int){
  /** Reply channels, for the server to return results to clients. */
  private type ReplyChan = OnePlaceBuffChan[Option[A]]

  /** Channel from clients to the server. */
  private val toServer = new SyncChan[(A, ReplyChan)]

  /** Exchange x with another thread. */
  def exchange(x: A): Option[A] = {
    val c = new ReplyChan; toServer!(x, c); c?()
  }

  /** The server thread. */
  private def server = thread("Exchanger"){
    repeat{
      val (x1, c1) = toServer?()
      toServer.receiveWithinNanos(delay) match{
        case Some((x2, c2)) => c1!Some(x2); c2!Some(x1)
        case None => c1!None
      }
    }
  }

  def shutdown() = toServer.endOfStream()

  fork(server)
}


// =======================================================

import scala.util.Random

/** A tester for a timed exchanger. */
object TimedExchangerTest{
  /** Do a single test. */
  def doTest() = {
    val n = scala.util.Random.nextInt(20)
    val results = new Array[Option[Int]](n)
    val exchanger = new TimedExchanger[Int](8000)
    def worker(me: Int) = thread(s"worker($me)"){ 
      val x = exchanger.exchange(me); results(me) = x 
    }
    run(|| (for(i <- 0 until n) yield worker(i)))
    for(i <- 0 until n) results(i) match{
      case Some(x) => assert(x != i && results(x) == Some(i)) 
      case None => {}
    }
    // println(results.count(_.isEmpty).toString+"/"+n)
    exchanger.shutdown()
  }

  def main(args: Array[String]) = {
    for(i <- 0 until 5000){ doTest(); if(i%50 == 0) print(".") }
    println()
  }
}
