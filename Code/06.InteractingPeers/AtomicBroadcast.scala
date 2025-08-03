package tacp.interactingPeers

import ox.scl._

/** A synchronous atomic broadcast, to be used by `n` receivers. */
class AtomicBroadcast[A](n: Int){
  require(n >= 2)

  /* We arrange the sender and receiver into a binary heap, with the sender at
   * the root.  The channels below are for signalling up and down respectively;
   * each array is indexed by the identities of the children. */

  private val signal = Array.fill(n)(new SyncChan[Unit])

  private val go = Array.fill(n)(new SyncChan[A])

  /** Receive a value.  
    * @param me the identity of this receiver. */
  def receive(me: Int): A = {
    val child1 = 2*me+2; val child2 = 2*me+3
    if(child1 < n) signal(child1)?()
    if(child2 < n) signal(child2)?()
    signal(me)!()
    val x = go(me)?()
    if(child1 < n) go(child1)!x
    if(child2 < n) go(child2)!x
    x
  }

  /** Send `x` as an atomic broadcast. */
  def send(x: A): Unit = {
    signal(0)?(); signal(1)?()
    go(0)!x; go(1)!x
  }
}

// =======================================================

import tacp.clientServer.AtomicBroadcastTest.
  {BeginSend,EndSend,BeginReceive,EndReceive,Log1,checkLog}
import scala.util.Random

object AtomicBroadcastTest{

  val iters = 5

  val Max = 20

  def sender(me: Int, ab: AtomicBroadcast[Int], log: Log1) = thread("sender"){
    for(i <- 0 until iters){
      val x = Random.nextInt(Max); log.add(me, BeginSend(x))
      ab.send(x); log.add(me, EndSend)
    }
  }

  def receiver(me: Int, ab: AtomicBroadcast[Int], log: Log1)
  = thread(s"receiver $me"){
    for(i <- 0 until iters){
      log.add(me, BeginReceive); val x = ab.receive(me); 
      log.add(me, EndReceive(x))
    }
  }

  def doTest = {
    val n = 2+Random.nextInt(20); val ab = new AtomicBroadcast[Int](n)
    val log = new Log1(n+1)
    val receivers = || (for(i <- 0 until n) yield receiver(i, ab, log))
    run(sender(n, ab, log) || receivers)
    checkLog(n, log.get)
  }

  def main(args: Array[String]) = {
    for(i <- 0 until 5000){ doTest; if(i%50 == 0) print(".") }
    println()
  }

}
