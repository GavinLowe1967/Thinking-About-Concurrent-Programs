package tacp.alternation

import ox.scl._
import scala.util.Random

/** A test for adaptive quadrature. 
  * 
  * Note: this assumes that a class Adaptive is on the current path, with
  * signature:
  *  
  * class Adaptive(
  *   f: Double => Double, a: Double, b: Double, Epsilon: Double, nWorkers: Int){
  *   require(a <= b)
  *   def apply(): Double = ...
  * }
  */
object AdaptiveTest{
  val Epsilon = 1E-6

  /** We will test the Adaptive class by selecting random polynomials.  Each
    * polynomial will be represented by an array of its coefficients.  The
    * polynomial p represents the sum of p(i)*x^i, where i ranges over p's
    * indices. */ 
  type Polynomial = Array[Double]  

  /* We'll create random polynomials, with degree uniform in [0..MaxDegree), and
    * coefficients uniform in [-MaxCoeff..MaxCoeff). */
  val random = scala.util.Random
  val MaxDegree = 5
  val MaxCoeff = 100

  /** Random Double in [-max, max). */
  def uniform(max: Double): Double = max*(2*random.nextDouble()-1)

  /** Create a random polynomial. */
  def mkPoly: Polynomial = 
    Array.fill(1+random.nextInt(MaxDegree))(uniform(MaxCoeff))

  /** Convert a poly to a string. */
  def toString(poly: Polynomial): String =
    (0 until poly.length).map(i => poly(i).toString+"x^"+i).mkString(" + ")

  /** Evaluate poly at x. */
  def evalPoly(poly: Polynomial)(x: Double): Double = {
    // Use Horner's rule.
    var result = 0.0
    for(i <- poly.size-1 to 0 by -1) result = result*x + poly(i)
    result
  }

  /** Estimate the integrap of f from a to b using adaptive quadrature. */
  def estimate(f: Double => Double, a: Double, b: Double) : Double = {
    val mid = (a+b)/2.0
    val fa = f(a); val fb = f(b); val fmid = f(mid)
    val lArea = (fa+fmid)*(mid-a)/2; val rArea = (fmid+fb)*(b-mid)/2
    val area = (fa+fb)*(b-a)/2
    if (Math.abs(lArea+rArea-area) < Epsilon) area
    else estimate(f,a,mid) + estimate(f,mid,b)
  }

  /** Pick parameters for a test.
    * @return a tuple (f, p, a, b, nWorkers) indicating that the integral of f
    * from a to b should be estimated using nWorkers workers, and that f
    * corresponds to p. */
  def pickParams: (Double => Double, Polynomial, Double, Double, Int) = {
    // Function to evaluate.
    val p = mkPoly; val f = evalPoly(p)(_)
    // Limits.
    val a = uniform(10); val b = a+10*Random.nextDouble()
    // Number of workers.
    val nWorkers = 1+Random.nextInt(16)
    (f, p, a, b, nWorkers)
  }

  /** Do a single test. */
  def doTest = {
    val (f, p, a, b, nWorkers) = pickParams
    val seqResult = estimate(f, a, b)
    val concResult = new Adaptive(f, a, b, Epsilon, nWorkers)()
    assert(
      seqResult != 0.0 && Math.abs((seqResult-concResult)/seqResult) < 1E-7 ||
        Math.abs(seqResult-concResult) < 1E-10,
      "failed\nf = "+toString(p)+"\n"+
        "a = "+a+"; b = "+b+"; nWorkers = "+nWorkers+"\n"+
        "seqResult = "+seqResult+"; concResult = "+concResult)
  }

  def main(args: Array[String]) = {
    for(i <- 0 until 1000){ doTest; if(i%10 == 0) print(".") }
    println()
  }
}


  
