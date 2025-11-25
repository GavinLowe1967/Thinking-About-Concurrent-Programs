package tacp.tests

import ox.scl._

import tacp.monitors.{DisjointLock,MonitorDisjointLock}

object DisjointLockTest{
  var iters = 200  // Number of iterations by each worker.
  val p = 6 // Number of workers.


  /** The type of objects being tested. */
  type DL = DisjointLock

  /** Sequential specification object.  The state (a,b) represents that a
    * A-threads and b B-threads are using the resource.  
    * Invariant: a = 0 or b = 0. */
  type S = (Int, Int)

  /* Sequential operations. */

  def seqAEnter(s: S): (Unit, S) = {
    val (a,b) = s; require(b == 0); ((), (a+1,b))
  }
  def seqAExit(s: S): (Unit, S) = {
    val (a,b) = s; assert(a > 0 && b == 0); ((), (a-1,b))
  }
  def seqBEnter(s: S): (Unit, S) = {
    val (a,b) = s; require(a == 0); ((), (a,b+1))
  }
  def seqBExit(s: S): (Unit, S) = {
    val (a,b) = s; assert(b > 0 && a == 0); ((), (a,b-1))
  }

  def worker(me: Int, log: LinearizabilityLog[S, DL]) = {
    val random = new scala.util.Random
    // Pause for a short while.
    def pause() = while(random.nextInt(1000) > 0){} 
    for(i <- 0 until iters){
      pause()
      if(random.nextInt(2) == 0){
        log(_.aEnter(), "aEnter", seqAEnter)
        pause()
        log(_.aExit(), "aExit", seqAExit)
      }
      else{
        log(_.bEnter(), "bEnter", seqBEnter)
        pause()
        log(_.bExit(), "bExit", seqBExit)
      }
    }
  }

  def doTest() = {
    val dl: DL = new MonitorDisjointLock
    val tester = LinearizabilityTester[S, DL]((0,0), dl, p, worker _)
    assert(tester() > 0)
  }

  def main(args: Array[String]) = {
    for(i <- 0 until 5000){ doTest(); if(i%50 == 0) print(".") }
    println()
  }

}
