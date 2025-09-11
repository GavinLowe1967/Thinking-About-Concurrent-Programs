package tacp.dataParallel

import ox.scl._

object Jacobi{
  type Matrix = Array[Array[Double]]
  type Vector = Array[Double]
  val Epsilon = 0.000001 // tolerance
}

import Jacobi._

/** Trait representing objects that perform Jacobian iteration. */
trait Jacobi{
  /** Find x such that a x is approximately b, performing Jacobi iteration until
    * successive iterations vary by at most Epsilon.
    * Pre: a is of size n by n, and b is of size n, for some n. */
  def solve(a: Matrix, b: Vector): Vector

  /** Calculate new value for x(i) based on the old value. */ 
  @inline protected def update(a: Matrix, b: Vector, x: Vector, i: Int, n: Int)
      : Double = {
    var sum = 0.0
    for(j <- 0 until n; if j != i) sum += a(i)(j)*x(j)
    (b(i)-sum) / a(i)(i)
  }
}

// -------------------------------------------------------

/** A sequential implementation of Jacobian iteration. */
object SeqJacobi extends Jacobi{
  def solve(a: Matrix, b: Vector): Vector = {
    val n = a.length
    require(a.forall(_.length == n) && b.length == n)
    var oldX, newX = new Vector(n)
    var done = false

    while(!done){
      done = true
      for(i <- 0 until n){
        newX(i) = update(a, b, oldX, i, n)
	done &&= Math.abs(oldX(i)-newX(i)) < Epsilon
      }
      if(!done){ val t = oldX; oldX = newX; newX = t } // Swap arrays.
    }
    newX
  }
}

// -------------------------------------------------------

/** A concurrent implementation of Jacobian iteration, using shared variables. */
class ConcJacobi(p: Int) extends Jacobi{
  private val combBarrier = new AndBarrier(p)

  def solve(a: Matrix, b: Vector): Vector = {
    val n = a.length
    require(a.forall(_.length == n) && b.length == n)
    /* The start of the segment calculated by thread id. */
    @inline def startFor(id: Int) = id*n/p
    val x0, x1 = new Vector(n)
    var result: Vector = null // Ends up storing final result.

    // Worker to handle rows [startFor(me) .. startFor(me+1)).
    def worker(me: Int) = thread(s"worker($me)"){
      val start = startFor(me); val end = startFor(me+1)
      var oldX = x0; var newX = x1; var done = false
      while(!done){
        var myDone = true
        for(i <- start until end){
          newX(i) = update(a, b, oldX, i, n)
	  myDone &&= Math.abs(oldX(i)-newX(i)) < Epsilon
        }
        done = combBarrier.sync(me, myDone)
        if(!done){ val t = oldX; oldX = newX; newX = t } // Swap arrays.
      }
      // Worker 0 sets result to final result.
      if(start == 0) result = newX
    }

    // Run system.
    run(|| (for (i <- 0 until p) yield worker(i)))
    result
  }
}

// -------------------------------------------------------

/** A concurrent implementation of Jacobian iteration, using message
  * passing. */
class JacobiMP0(p: Int) extends Jacobi{
  private val barrier = new Barrier(p)
  // Messages are triples: worker identity, new segment of x, is that worker
  // willing to terminate?
  private type Msg = (Int, Vector, Boolean)
  // Channels to send messages to workers: indexed by receiver's identity;
  // buffered.
  private val toWorker = Array.fill(p)(new BuffChan[Msg](p-1))

  def solve(a: Matrix, b: Vector): Vector = {
    val n = a.length
    require(a.forall(_.length == n) && b.length == n)
    /* The start of the segment calculated by thread id. */
    @inline def startFor(id: Int) = id*n/p

    var result: Vector = null // will hold final result

    /* Worker to handle rows [startFor(me) .. startFor(me+1)). */
    def worker(me: Int) = thread(s"worker($me)"){
      val start = startFor(me); val end = startFor(me+1); val height = end-start
      var done = false; val x = new Vector(n)

      while(!done){
        done = true
        // newX(i) holds the new value of x(i+start)
        val newX = new Array[Double](height)
        // Update this section of x, storing results in newX
        for(i <- start until end){
          newX(i-start) = update(a, b, x, i, n)
          done &&= Math.abs(x(i)-newX(i-start)) < Epsilon
        }
        // Send this section to all other processes.
        for(w <- 1 until p) toWorker((me+w)%p)!(me, newX, done)
        // Copy newX into x.
        for(i <- 0 until height) x(start+i) = newX(i)
        // Receive from others.
        for(k <- 0 until p-1){
          val (him, hisX, hisDone) = toWorker(me)?(); val offset = startFor(him)
          for(i <- 0 until hisX.length) x(offset+i) = hisX(i)
          done &&= hisDone
        }
        // Synchronise for end of round.
        barrier.sync(me)
      }
      if(me == 0) result = x // Copy final result.
    } // end of worker

    // Run system
    run(|| (for(i <- 0 until p) yield worker(i)))
    result
  }
}

// -------------------------------------------------------

/** A concurrent implementation of Jacobian iteration, using message
  * passing, and avoiding copying of individual elements. */
class JacobiMP(p: Int) extends Jacobi{
  private val barrier = new Barrier(p)
  // Messages are triples: worker identity, new segment of x, is that worker
  // willing to terminate?
  private type Msg = (Int, Vector, Boolean)
  // Channels to send messages to workers: indexed by receiver's identity;
  // buffered.
  private val toWorker = Array.fill(p)(new BuffChan[Msg](p-1))

  def solve(a: Matrix, b: Vector): Vector = {
    val n = a.length
    require(a.forall(_.length == n) && b.length == n)
    /* The start of the segment calculated by thread id. */
    @inline def startFor(id: Int) = id*n/p
    // The size of the segment calculated by each thread. */
    val sizeFor = Array.tabulate(p)(id => startFor(id+1)-startFor(id))
    var result: Vector = null // Will hold final result.

    // Worker to handle rows [me*height .. (me+1)*height).
    def worker(me: Int) = thread{
      val start = startFor(me); val end = startFor(me+1); val height = end-start
      var done = false
      val xs = Array.tabulate(p)(i => new Vector(sizeFor(i)))
      // xs represents the vector x = xs.flatten

      while(!done){
        done = true
        // newX(i) holds the new value of x(i+start) = xs(me)(i).
        val newX = new Array[Double](height)
        // Update this section of x, storing results in newX.
        for(i1 <- 0 until height){
          val i = start+i1
          var sum = 0.0
          for(k <- 0 until p; j1 <- 0 until sizeFor(k)){
            val j = startFor(k)+j1; if(j != i) sum += a(i)(j)*xs(k)(j1)
          }
          newX(i1) = (b(i)-sum) / a(i)(i)
          done &&= Math.abs(xs(me)(i1)-newX(i1)) < Epsilon
        }
        // Send this section to all other processes.
        for(w <- 1 until p) toWorker((me+w)%p)!(me, newX, done)
        // Copy newX into x.
        xs(me) = newX
        // Receive from others.
        for(k <- 0 until p-1){
          val (him, hisX, hisDone) = toWorker(me)?()
          xs(him) = hisX; done &&= hisDone
        }
        // Synchronise for end of round.
        barrier.sync(me)
      }
      if(me == 0) result = xs.flatten // Copy final result.
    } // End of worker.

    // Run system.
    run(|| (for(i <- 0 until p) yield worker(i)))
    result
  }
}
