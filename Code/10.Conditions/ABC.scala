package tacp.monitors

import ox.scl._

trait ABC[A,B,C]{
  def ASync(me: A): (B,C)
  def BSync(me: B): (A,C)
  def CSync(me: C): (A,B)
}

// =======================================================

class ABCSyncMonitor[A,B,C] extends ABC[A,B,C]{
  private val lock = new Lock

  /** The identities of the current (or previous) threads. */
  private var a: A = _; private var b: B = _; private var c: C = _

  /** Are the values in a, b, c valid? */
  private var aFull, bFull, cFull = false

  /** Conditions on which threads wait for permission to write their values. */
  private val aStart, bStart, cStart = lock.newCondition

  /** Conditions on which threads wait to be able to read their results. */
  private val aSignal, bSignal = lock.newCondition

  def ASync(me: A) = lock.mutex{
    aStart.await(!aFull)          // (A1)
    assert(!bFull && !cFull); a = me; aFull = true
    bStart.signal()                // signal to b at (B1)
    aSignal.await()                // (A2)
    assert(aFull && bFull && cFull)
    bSignal.signal()                 // signal to b at (B2)
    (b,c)
  }

  def BSync(me: B) = lock.mutex{
    bStart.await(aFull && !bFull)         // (B1)
    assert(!cFull); b = me; bFull = true
    cStart.signal()                 // signal to c at (C1)
    bSignal.await()                // (B2)
    assert(aFull && bFull && cFull)
    aFull = false; bFull = false; cFull = false
    aStart.signal()               // signal to next A at (A1)
    (a,c)
  }

  def CSync(me: C) = lock.mutex{
    cStart.await(bFull && !cFull)         // (C1)
    assert(aFull); c = me; cFull = true
    aSignal.signal() // Signal to A at (A2)
    (a,b)
  }
}

// =======================================================

object ABCTest{
  val n = 6 // Number of threads in each role

  // Events for the log
  trait LogEvent
  case class AStart(a: Int) extends LogEvent
  case class BStart(b: Int) extends LogEvent
  case class CStart(c: Int) extends LogEvent
  case class AEnd(a: Int, b: Int, c: Int) extends LogEvent
  case class BEnd(a: Int, b: Int, c: Int) extends LogEvent
  case class CEnd(a: Int, b: Int, c: Int) extends LogEvent

  /* Each worker thread gets a distinct identity in the range [0..3n), with
   * [0..n) being A-threads, [n..2n) being B-threads, and [2n..3n) being
   * C-threads. */

  /** Check that es represents a valid log. */
  def checkLog(es: Array[LogEvent]) = {
    // started i indicates whether thread i has started.  results(i)
    // optionally indicates the result expected for thread i, and is set when
    // we first encounter one of the three End events for this
    // synchronisation.
    val started = new Array[Boolean](3*n)
    val results = Array.fill[Option[(Int,Int,Int)]](3*n)(None)
    // Deal with an *End(a,b,c) event in the log by thread i
    def processEnd(i: Int, a: Int, b: Int, c: Int) = {
      assert(started(a) && started(b) && started(c))
      results(i) match{
        case None => 
          for(j <- List(a,b,c); if j != i){ 
            assert(results(j) == None); results(j) = Some((a,b,c))
          }
        case Some((a1,b1,c1)) => assert(a1 == a && b1 == b && c1 == c)
      }
    }
    // Traverse es.
    var ix = 0
    while(ix < es.length){
      es(ix) match{
        case AStart(a) => started(a) = true
        case BStart(b) => started(b) = true
        case CStart(c) => started(c) = true
        case AEnd(a,b,c) => processEnd(a, a, b, c)
        case BEnd(a,b,c) => processEnd(b, a, b, c)
        case CEnd(a,b,c) => processEnd(c, a, b, c)
      } // end of es(i) match
      ix += 1
    }
    // Note: this ought to give more feedback in the case of a failure. 
  }

  def doTest = {
    val abc: ABC[Int,Int,Int] = new ABCSyncMonitor[Int,Int,Int]
    val log = new Log[LogEvent](3*n)
    def aThread(a: Int) = thread(s"A($a)"){
      log.add(a, AStart(a)); val (b,c) = abc.ASync(a); log.add(a, AEnd(a,b,c))
    }
    def bThread(b: Int) = thread(s"B($b)"){
      log.add(b, BStart(b)); val (a,c) = abc.BSync(b); log.add(b, BEnd(a,b,c))
    }
    def cThread(c: Int) = thread(s"C($c)"){
      log.add(c, CStart(c)); val (a,b) = abc.CSync(c); log.add(c, CEnd(a,b,c))
    }
    def worker(me: Int) = 
      if(me < n) aThread(me) else if(me < 2*n) bThread(me) else cThread(me)
    run(|| (for(i <- 0 until 3*n) yield worker(i)))
    checkLog(log.get)
  } // end of doTest

  def main(args: Array[String]) = {
    for(i <- 0 until 5000){ doTest; if(i%200 == 0) print(".") }
    println()
  }

}
