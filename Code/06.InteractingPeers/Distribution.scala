package tacp.interactingPeers

import ox.scl._

/** Objects that allow `n` clients to send messages to each other.
  * Internally, the object uses a ring to distribute messages. */
class Distribution(n: Int){
  /** A token is a triple (s,r,m), indicating message m being sent from s to
    * r. */
  type Token = (Int, Int, String)
  /** Channels for the ring.  ring(i) goes from node (i-1) mod n to node i;
    * so node i inputs on ring(i) and outputs on chan((i+1)%n). */  
  private val ring = Array.fill(n)(new UnboundedBuffChan[Token])

  /** Input channels into the ring. */
  private val in = Array.fill(n)(new UnboundedBuffChan[(Int, String)])

  /** Output channels from the ring. */
  private val out = Array.fill(n)(new UnboundedBuffChan[(Int, String)])

  /** Client `s` sends message `m` for client `r`. */
  def send(s: Int, r: Int, m: String) = {
    require(0 <= s && s < n && 0 <= r && r < n); in(s)!((r,m))
  }

  /** Client `r` receives a message, together with the identity of the sending
    * client. */
  def receive(r: Int): (Int, String) = { require(0 <= r && r < n); out(r)?() }

  /** Ring node with identity `me`. */
  private def node(me: Int) = thread(s"node $me"){
    val left = ring(me); val right = ring((me+1)%n)
    /* Deal with token (s,r,m), */
    def dispatch(s: Int, r: Int, m: String) = {
      if(r == me) out(me)!(s,m) else right!(s,r,m)
    }
    serve(
      left =?=> { case (s,r,m) => dispatch(s,r,m) }
      | in(me) =?=> { case (r,m) => dispatch(me,r,m) }
    )
  }

  // Fork off the threads. */
  fork(|| (for(i <- 0 until n) yield node(i)))

  /** Shut down the system. */
  def shutdown() = {
    ring.foreach(_.close()); in.foreach(_.close()); out.foreach(_.close())
  }
}

// =======================================================

import scala.util.Random

/** Test for Distribution. */
object DistributionTest{
  /** Run a single test. */
  def doTest() = {
    // We use n workers.  Each sends k values to each worker (including
    // itself).  So each thread performs a total of iters sends and receives.
    // Each thread sends values [0..iters) in order, converted to Strings.
    val n = 5; val k = 4; val iters = n*k
    val dist = new Distribution(n)
    // If sends(s)(i) = r then sender(s)'s i'th send is to thread r with
    // value i.toString.
    val sends = new Array[Array[Int]](n)
    // If receives(r)(i) = (s,m), then receiver(r)'s i'th receive was (s,m).
    val receives = Array.ofDim[(Int,String)](n,iters)
    def sender(me: Int) = thread(s"sender $me"){
      // This worker's i'th send will be i.toString to sends(me)(i).
      sends(me) = Random.shuffle(List.tabulate(iters)(i => i%n)).toArray
      for(i <- 0 until iters) dist.send(me, sends(me)(i), i.toString)
    }
    def receiver(me: Int) = thread(s"receiver $me"){
      for(i <- 0 until iters) receives(me)(i) = dist.receive(me)
    }
    run(|| (for(i <- 0 until n) yield sender(i) || receiver(i)))
    dist.shutdown()
    // Check correctness: if r received (s,m), then s sent m to r.
    for(r <- 0 until n; (s,m) <- receives(r)) assert(sends(s)(m.toInt) == r) 
  }

  def main(args: Array[String]) = {
    for(i <- 0 until 10000){ doTest(); if(i%100 == 0) print(".") }
    println()
  }

}


