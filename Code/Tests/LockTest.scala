package tacp.tests

import ox.scl._
import tacp.jvmMonitors.LockT

/** Linearizability testing of lock. */
object LockTest{
  /** The sequential specification datatype: true represents that the lock is
    * held by a thread. */
  type SeqLock = Boolean

  def seqAcquire(l: SeqLock): (Unit, SeqLock) = { require(!l); ((), true) }

  def seqRelease(l: SeqLock): (Unit, SeqLock) = { assert(l); ((), false) }

  val iters = 100

  def worker(me: Int, log: LinearizabilityLog[SeqLock, LockT]) = {
    for(i <- 0 until iters){
      log(_.acquire(), "acquire", seqAcquire)
      log(_.release(), "release", seqRelease)
    }
  }

  val JVMMon = 0; val Semaphore = 1

  def doTest(choice: Int) = {
    val lock: LockT = 
      if(choice == JVMMon) new tacp.jvmMonitors.Lock
      else{ assert(choice == Semaphore); new tacp.semaphores.QueueLock }
    val tester = LinearizabilityTester[SeqLock,LockT](false, lock, 8, worker)
    if(tester() <= 0) sys.exit()
  }

  def main(args: Array[String]) = {
    var choice = JVMMon; var i = 0
    while(i < args.length) args(i) match{
      case "--semaphore" => choice = Semaphore; i += 1
    }

    for(i <- 0 until 5000){ doTest(choice); if(i%200 == 0) print(".") }
    println()
  }

}
