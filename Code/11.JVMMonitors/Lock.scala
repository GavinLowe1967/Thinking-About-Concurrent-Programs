package tacp.jvmMonitors

import ox.scl._

/** A simple lock implemented using a JVM monitor. */
class Lock{
  /** Does a thread currently hold the lock? */
  private var locked = false

  /** Acquire the lock. */
  def acquire() = synchronized{
    while(locked) wait() // Wait for current thread to release the lock. 
    locked = true
  }

  /** Release the lock.*/
  def release() = synchronized{
    locked = false
    notify() // Signal to an acquire.
  }
}

// =======================================================

/** A simple test on Lock. */
object LockTest{
  /* We run an even number of threads, half of which increment `c` `iters`
   * times, and the remainder decrement `c` `iters` times, in each case
   * protected by `lock`.  At the end, we check `c` = 0.  */

  private var c = 0

  private val iters = 500

  private val lock = new Lock

  private def inc() = { lock.acquire(); c += 1; lock.release() }

  private def dec() = { lock.acquire(); c -= 1; lock.release() }

  private def worker(me: Int) = thread(s"worker($me)"){
    for(_ <- 0 until iters) if(me%2 == 0) inc() else dec()
  }

  def doTest() = {
    run(|| (for(id <- 0 until 8) yield worker(id)))
    assert(c == 0)
  }

  def main(args: Array[String]) = {
    for(i <- 0 until 10000){ doTest(); if(i%500 == 0) print(".") }
    println()
  }

}

// =======================================================

/** Linearizability testing of the lock. */
object LockLinTest{
  /** The sequential specification datatype: true represents that the lock is
    * held by a thread. */
  type SeqLock = Boolean

  def seqAcquire(l: SeqLock): (Unit, SeqLock) = { require(!l); ((), true) }

  def seqRelease(l: SeqLock): (Unit, SeqLock) = { assert(l); ((), false) }

  val iters = 100

  def worker(me: Int, log: LinearizabilityLog[SeqLock, Lock]) = {
    for(i <- 0 until iters){
      log(_.acquire(), "acquire", seqAcquire)
      log(_.release(), "release", seqRelease)
    }
  }

  def doTest() = {
    val tester = LinearizabilityTester[SeqLock,Lock](false, new Lock, 8, worker)
    if(tester() <= 0) sys.exit()
  }

  def main(args: Array[String]) = {
    for(i <- 0 until 5000){ doTest(); if(i%200 == 0) print(".") }
    println()
  }

}
