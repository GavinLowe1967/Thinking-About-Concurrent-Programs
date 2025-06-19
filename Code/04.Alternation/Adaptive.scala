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

// =======================================================

// import scala.util.Random

// /** A test for adaptive quadrature. 
//   * 
//   * Note: this assumes that Adaptive is on the current path. */
// object AdaptiveTest{
//   val Epsilon = 1E-6

//   /** We will test the Adaptive class by selecting random polynomials.  Each
//     * polynomial will be represented by an array of its coefficients.  The
//     * polynomial p represents the sum of p(i)*x^i, where i ranges over p's
//     * indices. */ 
//   type Polynomial = Array[Double]  

//   /* We'll create random polynomials, with degree uniform in [0..MaxDegree), and
//     * coefficients uniform in [-MaxCoeff..MaxCoeff). */
//   val random = scala.util.Random
//   val MaxDegree = 5
//   val MaxCoeff = 100

//   /** Random Double in [-max, max). */
//   def uniform(max: Double): Double = max*(2*random.nextDouble()-1)

//   /** Create a random polynomial. */
//   def mkPoly: Polynomial = 
//     Array.fill(1+random.nextInt(MaxDegree))(uniform(MaxCoeff))

//   /** Convert a poly to a string. */
//   def toString(poly: Polynomial): String =
//     (0 until poly.length).map(i => poly(i).toString+"x^"+i).mkString(" + ")

//   /** Evaluate poly at x. */
//   def evalPoly(poly: Polynomial)(x: Double): Double = {
//     // Use Horner's rule.
//     var result = 0.0
//     for(i <- poly.size-1 to 0 by -1) result = result*x + poly(i)
//     result
//   }

//   /** Estimate the integrap of f from a to b using adaptive quadrature. */
//   def estimate(f: Double => Double, a: Double, b: Double) : Double = {
//     val mid = (a+b)/2.0
//     val fa = f(a); val fb = f(b); val fmid = f(mid)
//     val lArea = (fa+fmid)*(mid-a)/2; val rArea = (fmid+fb)*(b-mid)/2
//     val area = (fa+fb)*(b-a)/2
//     if (Math.abs(lArea+rArea-area) < Epsilon) area
//     else estimate(f,a,mid) + estimate(f,mid,b)
//   }

//   /** Pick parameters for a test.
//     * @return a tuple (f, p, a, b, nWorkers) indicating that the integral of f
//     * from a to b should be estimated using nWorkers workers, and that f
//     * corresponds to p. */
//   def pickParams: (Double => Double, Polynomial, Double, Double, Int) = {
//     // Function to evaluate.
//     val p = mkPoly; val f = evalPoly(p)(_)
//     // Limits.
//     val a = uniform(10); val b = a+10*Random.nextDouble()
//     // Number of workers.
//     val nWorkers = 1+Random.nextInt(16)
//     (f, p, a, b, nWorkers)
//   }

//   /** Do a single test. */
//   def doTest = {
//     val (f, p, a, b, nWorkers) = pickParams
//     val seqResult = estimate(f, a, b)
//     val concResult = new Adaptive(f, a, b, Epsilon, nWorkers)()
//     assert(
//       seqResult != 0.0 && Math.abs((seqResult-concResult)/seqResult) < 1E-7 ||
//         Math.abs(seqResult-concResult) < 1E-10,
//       "failed\nf = "+toString(p)+"\n"+
//         "a = "+a+"; b = "+b+"; nWorkers = "+nWorkers+"\n"+
//         "seqResult = "+seqResult+"; concResult = "+concResult)
//   }

//   def main(args: Array[String]) = {
//     for(i <- 0 until 1000){ doTest; if(i%10 == 0) print(".") }
//     println()
//   }
// }


  
