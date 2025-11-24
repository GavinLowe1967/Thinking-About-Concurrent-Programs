package tacp.jvmMonitors

import ox.scl._

trait LockT{
  /** Acquire the lock. */
  def acquire(): Unit 

  /** Release the lock.*/
  def release(): Unit

  /** Execute comp under mutual exclusion. */
  def mutex[A](comp: => A): A = {
    acquire()
    try{ comp } finally{ release() }
  }
}
 
/** A simple lock implemented using a JVM monitor. */
class Lock extends LockT{
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

  /** Get a Condition associated with this. */
  def newCondition = new Condition(this)
}

// =====

/** A Condition, associated with lock.  All operations should be called only
  * by a thread holding lock. */
class Condition(lock: Lock){
  /** An object that allows a single thread to wait to receive a signal. */
  private class Signal{
    /** Has there been a signal? */
    private var done = false

    /** Wait for a signal. */
    def await() = synchronized{ while(!done) wait() }

    /** Signal to the waiting thread. */
    def signal() = synchronized{ done = true; notify() }
  }

  /** Queue of waiting Signal objects, protected by lock. */
  private val queue = new scala.collection.mutable.Queue[Signal]

  /** Wait for a signal.  Precondition: this thread holds lock. */
  def await(): Unit = {
    val mySignal = new Signal; queue.enqueue(mySignal); lock.release()
    mySignal.await(); lock.acquire()
  }

  /** Wait for test to be true.  Precondition: this thread holds lock. */
  def await(test: => Boolean): Unit = while(!test) await()

  /** Signal to a waiting thread.  Precondition: this thread holds lock. */
  def signal() = {
    if(queue.nonEmpty){ val sig = queue.dequeue(); sig.signal() }
  }

  /** Signal to all waiting threads.  Precondition: this thread holds lock. */
  def signalAll() = {
    while(queue.nonEmpty){ val sig = queue.dequeue(); sig.signal() }
  }
}






// =======================================================

// Note: there is a linearizability tester for locks in the Tests directory. 

/** A simple test on Lock. */
object SimpleLockTest{
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
