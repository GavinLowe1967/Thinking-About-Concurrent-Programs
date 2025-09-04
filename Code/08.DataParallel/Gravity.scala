package tacp.dataParallel

import ox.scl._

/* Simulation of a number of astronomical bodies acting upon each other under
 * gravity. */

object Gravity{
  /** Gravitational constant. */
  val G = 6.6743E-11

  /** The time for one step of the simulation, slightly over one day. */
  val Timestep = 100_000
}


// =======================================================

/** A vector in 3-dimensional space. */
case class Vector(x: Double, y: Double, z: Double){
  /** The length of this vector. */
  def length = Math.sqrt(x*x + y*y + z*z)

  /** This plus v. */
  def + (v: Vector) = Vector(x+v.x, y+v.y, z+v.z)

  /** The minus v. */
  def - (v: Vector) = Vector(x-v.x, y-v.y, z-v.z)

  /** This times s. */
  def * (s: Double) = Vector(x*s, y*s, z*s)

  /** Format this as a String, printing numbers to p decimal places. */
  def toString(p: Int) = {
    def f(xx: Double) = s"%.${p}f".format(xx)
    s"(${f(x)}, ${f(y)}, ${f(z)})"
  }
 
}

object Vector{
  val Zero = Vector(0.0, 0.0, 0.0)
}

// =======================================================

/** A single astronomical body. */
class AstronomicalBody(val mass: Double, position0: Vector, velocity0: Vector){
  /** The body's current position. */
  private var position = position0

  /** The body's current velocity. */
  private var velocity = velocity0

  /** Update the velocity of this based on the gravitational attraction from
    * other. */
  def updateVel(other: AstronomicalBody) = {
    val towards = other.position-position // Vector to other.
    val d = towards.length // Distance to other.
    val force = Gravity.G*mass*other.mass/(d*d) // Force on this.
    val dv = towards*(force/mass/d*Gravity.Timestep) // Change in velocity.
    velocity += dv
  }

  /** Move this for one timestep.  Return the new position. */
  def move() = { position += velocity*Gravity.Timestep; position }

  override def clone = new AstronomicalBody(mass, position0, velocity0)
}


// =======================================================

/** A sequential simulation for bodies. */
class SequentialSimulation(bodies: Array[AstronomicalBody]){
  private val n = bodies.length

  /** Simulate for steps steps, returning an array of the bodies' positions at
    * each timestep. */
  def apply(steps: Int): Array[Array[Vector]] = {
    val result = Array.ofDim[Vector](steps,bodies.length)
    for(step <- 0 until steps){
      for(i <- 0 until n; j <- 0 until n; if i != j) 
        bodies(i).updateVel(bodies(j))
      for(i <- 0 until n) result(step)(i) = bodies(i).move()
    }
    result
  }
}

// =======================================================

/** A concurrent simulation for bodies. */
class ConcurrentSimulation(bodies: Array[AstronomicalBody]){
  private val n = bodies.length

  /** Simulate for steps steps, using numWorkers workers, returning an array of
    * the bodies' positions at each timestep. */
  def apply(steps: Int, numWorkers: Int): Array[Array[Vector]] = {
    // Array to hold the results.
    val result = Array.ofDim[Vector](steps,bodies.length)

    // The barrier for coordinating workers.
    val barrier = new Barrier(numWorkers)

    // A single worker.
    def worker(me: Int) = thread(s"worker($me)"){
      // This worker is responsible for bodies[start..end). 
      val start = me*n/numWorkers; val end = (me+1)*n/numWorkers
      for(step <- 0 until steps){
        for(i <- start until end; j <- 0 until n; if i != j)
          bodies(i).updateVel(bodies(j))
        barrier.sync(me)
        for(i <- start until end) result(step)(i) = bodies(i).move()
        barrier.sync(me)
      }
    }

    // Run the workers. 
    run(|| (for(i <- 0 until numWorkers) yield worker(i)))
    result
  }
}

// =======================================================

import scala.util.Random

object GravityTest{
  /** A pair of bodies, roughly corresponding to the sun and earth. */
  def twoBodies = Array(
    new AstronomicalBody(1.989E30, Vector.Zero, Vector.Zero),
    new AstronomicalBody(
      5.9722E24, Vector(1.5E11, 0.0, 0.0), Vector(0.0, 2.9785E4, 0.0))
  )

  def twoBodySimulation = {
    val steps = 316 // approx one year
    val sResults = new SequentialSimulation(twoBodies)(steps)
    val cResults = new ConcurrentSimulation(twoBodies)(steps, 2)
    showResults(sResults); println(); showResults(cResults)
    for(i <- 0 until steps; j <- 0 until 2){
      val s = sResults(i)(j); val c = cResults(i)(j)
      assert(s == c, s"$i $j $s $c")
    }
  }

  /** Display results, in units of millions of km. */
  def showResults(results: Array[Array[Vector]]) = 
    for(i <- 0 until results.length){
      for(v <- results(i)){ print((v*1E-9).toString(2)); print("\t") }
      println()
    }

  /** Perform a single test, running sequential and concurrent simulations on
    * the same data, and comparing the results. */
  def doTest = {
    val steps = 1000
    val n = 2+Random.nextInt(50); val numWorkers = 2+Random.nextInt(7)
    // A random number in [0,max).
    def rand(max: Double) = max*Random.nextDouble()
    // Make a random AstronomicalBody
    def newBody() = new AstronomicalBody(
      rand(1E31), // mass
      Vector(rand(1E15), rand(1E15), rand(1E15)), // position
      Vector(rand(1E7), rand(1E7), rand(1E7)) // velocity
    )
    val bodies = Array.fill(n)(newBody()); val bodies1 = bodies.map(_.clone)
    val sResults = new SequentialSimulation(bodies)(steps)
    val cResults = new ConcurrentSimulation(bodies1)(steps, numWorkers)
    for(i <- 0 until steps; j <- 0 until n){
      val s = sResults(i)(j); val c = cResults(i)(j)
      assert(s == c, s"$i $j $s $c")
    }
  }

  def main(args: Array[String]) = {
    if(false) twoBodySimulation
    else{ 
      for(i <- 0 until 100){ doTest; print(".") }
      println()
    }
  }
}
