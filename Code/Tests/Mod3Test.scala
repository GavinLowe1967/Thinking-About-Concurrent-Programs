package tacp.tests

import ox.scl._
import tacp.jvmMonitors.{Mod3,Mod3M}
import tacp.semaphores.{Mod3S, Mod3SA}
import scala.collection.immutable.Set

object Mod3Test{
  var iters = 200  // Number of iterations by each worker.
  val numWorkers = 6 // Number of workers.

  /** The abstract specification object: the state represents those threads
    * currently using the resource. */ 
  type S = Set[Int]

  def seqEnter(id: Int)(current: S): (Unit, S) = {
    require(current.sum%3 == 0); ((), current+id)
  }

  def seqExit(id: Int)(current: S): (Unit, S) = {
    assert(current.contains(id)); ((), current-id)
  }

  // A worker thread
  def worker(me: Int, log: LinearizabilityLog[S,Mod3]) = {
    for(i <- 0 until iters){
      log(_.enter(me), "enter("+me+")", seqEnter(me))
      log(_.exit(me), "exit("+me+")", seqExit(me))
    }
  }

  val Monitor = 0; val Semaphore = 1; val SemaphoreA = 2

  def doTest(choice: Int) = {
    val mon: Mod3 = choice match{
      case Monitor => new Mod3M; case Semaphore => new tacp.semaphores.Mod3S
      case SemaphoreA => new Mod3SA
    }
    val tester = 
      LinearizabilityTester[S, Mod3](Set[Int](), mon, numWorkers, worker _)
    assert(tester() > 0)
  }

  // The main method
  def main(args: Array[String]) = {
    var choice = 0; var i = 0
    while(i < args.length) args(i) match{
      case "--semaphore" => choice = Semaphore; i += 1
      case "--semaphoreA" => choice = SemaphoreA; i += 1
    }
    for(i <- 0 until 5000){ doTest(choice); if(i%50 == 0) print(".") }
    println()
  }
}
