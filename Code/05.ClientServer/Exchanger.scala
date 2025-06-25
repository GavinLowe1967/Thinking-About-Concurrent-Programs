package tacp.clientServer

import ox.scl._

class Exchanger[A]{
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

// =======================================================

/** A tester for an exchanger. */
object ExchangerTest{
  /** Do a single test. */
  def doTest = {
    val n = 2*scala.util.Random.nextInt(10); val results = new Array[Int](n)
    val exchanger = new Exchanger[Int]
    def worker(me: Int) = thread(s"worker($me)"){ 
      val x = exchanger.exchange(me); results(me) = x 
    }
    run(|| (for(i <- 0 until n) yield worker(i)))
    for(i <- 0 until n){ val x = results(i);  assert(results(x) == i) }
    exchanger.shutdown()
  }

  def main(args: Array[String]) = {
    for(i <- 0 until 1000){ doTest; if(i%50 == 0) print(".") }
    println()
  }
}
