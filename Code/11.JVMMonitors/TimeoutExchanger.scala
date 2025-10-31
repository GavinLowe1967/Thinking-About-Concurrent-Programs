package tacp.jvmMonitors

import ox.scl._

/** An object that allows threads to try to exchange data of type A.  An
  * attempt to exchange will timeout after (approximately) delay ns, returning
  * a result of None.   */
class TimeoutExchanger[A](delay: Int){
  private var slot: A = _

  /* stage stores an indication of what stage we are at in an exchange, one of
   * the following values. */
  private val Empty = 0 // No current thread (slot is invalid).
  private val Filled = 1 // The value in slot has been deposited by the first
                         // thread.
  private val Exchanging = 2 // An exchange is underway: the second thread has
                             // returned; the first thread should take the
                             // value in slot.
  private var stage: Int = Empty

  def exchange(x: A): Option[A] = synchronized{
    if(stage == Empty){              // Deposit my value and wait.
      slot = x; stage = Filled; wait(0, delay)
      if(stage == Exchanging){ stage = Empty; Some(slot) } // Success.
      else{ assert(stage == Filled); stage = Empty; None } // No joy.
    }
    else if(stage == Filled){
      // Take value in slot; deposit my value there.
      val result = slot; slot = x; stage = Exchanging; notify(); Some(result)
    }
    else{ assert(stage == Exchanging); None } // Another exchange is under way.
  }
}

// =======================================================

import scala.util.Random

/** A tester for an exchanger. */
object ExchangerTest{

  /** Do a single test. */
  def doTest() = {
    val n = 1+Random.nextInt(10); val delay = 1+Random.nextInt(4)
    val results = Array.fill[Option[Int]](n)(None)
    val exchanger = new TimeoutExchanger[Int](delay)
    def worker(me: Int) = thread(s"worker($me)"){ 
      Thread.sleep(0,Random.nextInt(8))
      val x = exchanger.exchange(me); results(me) = x 
    }
    run(|| (for(i <- 0 until n) yield worker(i)))
    var count = 0
    for(i <- 0 until n) results(i) match{
      case Some(x) =>  assert(x != i && results(x) == Some(i)); count += 1
      case None => {}
    }
    //println(s"$n $count") 
  }
  // Note: the delay before calling exchange and the delay within the
  // exchanger are chosen to ensure a reasonable proportion of exchanges fail.

  def main(args: Array[String]) = {
    for(i <- 0 until 5000){ doTest(); if(i%100 == 0) print(".") }
    println()
  }
}
