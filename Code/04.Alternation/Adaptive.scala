package tacp.alternation

import ox.scl._

/** Calculating integral, using trapezium rule, adaptive quadrature, and bag
  * of tasks pattern. */
class Adaptive(
  f: Double => Double, a: Double, b: Double, Epsilon: Double, nWorkers: Int){
  require(a <= b)

  // Interval on which to work
  private type Task = (Double, Double)

  /** The bag, that keeps track of jobs pending. */
  object Bag{
    /** Channel to the workers, to distribute tasks.  */
    private val toWorkers = new SyncChan[Task]

    /** Channel from the workers, to return subtasks. */
    private val toBag = new SyncChan[Task]

    /** Channel to indicate that a worker has completed a task. */
    private val doneC = new SyncChan[Unit]

    /** Get a task. */
    def get(): Task = toWorkers?()

    /** Add t to the bag. */
    def add(t: Task) = toBag!t

    /** Indicate that a task is done. */
    def done() = doneC!()

    /** Server controlling the bag. */
    private def server = thread("bag"){
      val stack = new scala.collection.mutable.Stack[Task]
      stack.push((a,b))
      var busyWorkers = 0 // # workers with tasks
      serve(
        stack.nonEmpty && toWorkers =!=> { busyWorkers += 1; stack.pop() }
          | busyWorkers > 0 && toBag =?=> { t1 => stack.push(t1) }
          | busyWorkers > 0 && doneC =?=> { _ => busyWorkers -= 1 }
      )
      assert(busyWorkers == 0 && stack.isEmpty)
      toWorkers.endOfStream() 
    }

    fork(server)
  } // End of Bag.

  /** The adder object, responsible for adding workers' subresults. */
  object Adder{
    /** Channel from the workers to the adder thread, to add up subresults. */
    private val toAdder = new SyncChan[Double]

    /** Channel to get final result. */
    private val getC = new SyncChan[Double]

    /** Add x to the overall result. */
    def add(x: Double) = toAdder!x

    /** Get the final result. */
    def get = getC?()
    
    /** Server to receive results from workers and add up the results. */
    private def adder = thread("adder"){
      var result = 0.0
      for(_ <- 0 until nWorkers){ result += (toAdder?()) }
      getC!result
    }

    fork(adder)
  } // End of Adder.

  /** A worker, that receives arguments from the server, either estimates the
    * integral directly or returns new tasks to the bag. */
  private def worker = thread("worker"){
    // If a < b, then this worker is responsible for the task (a,b).  If a =
    // b, then the worker has no current task.
    var a = -1.0; var b = -1.0
    var mySum = 0.0 // Current sum this worker has calculated.
    repeat{
      if(a == b){ val p = Bag.get(); a = p._1; b = p._2; assert(a < b) }
      val mid = (a+b)/2.0; val fa = f(a); val fb = f(b); val fmid = f(mid)
      val larea = (fa+fmid)*(mid-a)/2; val rarea = (fmid+fb)*(b-mid)/2
      val area = (fa+fb)*(b-a)/2
      if (Math.abs(larea+rarea-area) < Epsilon){ 
        mySum += area; b = a; Bag.done()
      }
      else{ // Return (a,mid) to the bag, and carry on with (mid,b). 
        Bag.add((a,mid)); a = mid 
      }
    }
    Adder.add(mySum)
  }

  def apply(): Double = {
    run( || (for (i <- 0 until nWorkers) yield worker) )
    Adder.get
  }
}

