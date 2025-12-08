package tacp.dataParallel

import ox.scl._
import scala.util.Random

object CombiningBarrierTest{
  val iters = 100 // Number of iterations per test.
  val reps = 500

  // Events to place in Log.
  abstract class LogEvent
  // Thread id calls sync or returns from sync.
  case class Arrive(id: Int, x: Int) extends LogEvent
  case class Leave(id: Int, y: Int) extends LogEvent

  /** The function used to combine values.  We'll use addition. */
  def f(x1: Int, x2: Int) = x1+x2

  /** Run a single test. */
  def doTest(doLog: Boolean) = {
    val p = 1+Random.nextInt(20) // Number of threads.
    val barrier: CombiningBarrierT[Int] = 
      if(doLog) new CombiningBarrierLog[Int](p,f)
      else new ServerCombiningBarrier[Int](p,f)
    val log = new debug.Log[LogEvent](p)
    def worker(me: Int) = thread{
      for(_ <- 0 until iters){ 
        val x = Random.nextInt(20); log.add(me, Arrive(me,x))
        val y = barrier.sync(me, x); log.add(me, Leave(me,y))
      }
    }
    run(|| (for (i <- 0 until p) yield worker(i)))
    barrier match{
      case sb: ServerCombiningBarrier[Int] => sb.shutdown()
      case _ => {}
    }
    checkLog(log.get, p)
  }

  /** Check that es represents a valid history for p threads. */
  def checkLog(es: Array[LogEvent], p: Int) = {
    // We traverse the log, keeping track of which threads are currently
    // within a sync, respectively waiting to synchronise or leaving; we use a
    // bitmap for each of these sets.
    var waiting, leaving = new Array[Boolean](p)
    var numWaiting, numLeaving = 0 // # currently waiting, leaving
    // started is true if we have seen a value as part of the current round.
    // In that case, y is all such values combined together.  expected is the 
    // expected return value.
    var started = false; var y = -1; var expected = -1
    for(e <- es) e match{
      case Arrive(id, x) =>
        if(!started){ y = x; started = true } else y = f(y,x)
        assert(!waiting(id)); waiting(id) = true; numWaiting += 1
        if(numWaiting == p){ // All can now leave.
          assert(numLeaving == 0); leaving = waiting; numLeaving = p
          waiting = new Array[Boolean](p); numWaiting = 0
          expected = y; started = false
        }
      case Leave(id, y1) =>
        assert(leaving(id) && y1 == expected)
        leaving(id) = false; numLeaving -= 1
    }
  }

  def main(args: Array[String]) = {
    var doLog = false; var i = 0
    while(i < args.length) args(i) match{
      case "--doLog" => doLog = true; i += 1
    }

    for(r <- 0 until reps){ doTest(doLog); if(r%10 == 0) print(".") }
    println()
  }
}
