package tacp.tests

import ox.scl._
import tacp.jvmMonitors.{Mod3,Mod3M}
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

  def doTest() = {
    val mon: Mod3 = new Mod3M
    val tester = 
      LinearizabilityTester[S, Mod3](Set[Int](), mon, numWorkers, worker _)
    assert(tester() > 0)
  }

  // The main method
  def main(args: Array[String]) = {
    for(i <- 0 until 5000){ doTest(); if(i%50 == 0) print(".") }
    println()
  }
}
