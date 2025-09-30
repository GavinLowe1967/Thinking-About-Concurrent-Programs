package tacp.locks

import ox.scl._

/** Trait for an array of `n`thread-safe counters. */
abstract class CounterArray(n: Int){
  /** The counters. */
  protected val a = new Array[Int](n)

  /** Increment counter i. */
  def inc(i: Int): Unit

  /** Decrement counter i. */
  def dec(i: Int): Unit

  /** Get the value of counter i. */
  def get(i: Int): Int
}

/** An implementation using coarse-grained locking. */
class CoarseCounterArray(n: Int) extends CounterArray(n){
  /** The counters. */
  //private val a = new Array[Int](n)

  /** Lock that protects all the counters. */
  private val lock = new Lock

  def inc(i: Int) = lock.mutex{ a(i) += 1 }

  def dec(i: Int) = lock.mutex{ a(i) -= 1 }

  def get(i: Int) = lock.mutex{ a(i) }
}


/** An implementation using fine-grained locking. */
class FineGrainedCounterArray(n: Int) extends CounterArray(n){
  /** The counters. */
  //private val a = new Array[Int](n)

  /** Locks.  locks(i) protects a(i). */
  private val locks = Array.fill(n)(new Lock)

  def inc(i: Int) = locks(i).mutex{ a(i) += 1 }

  def dec(i: Int) = locks(i).mutex{ a(i) -= 1 }

  def get(i: Int) = locks(i).mutex{ a(i) }
}


/** An implementation using striped locking. */
class StripedCounterArray(n: Int, stripes: Int) extends CounterArray(n){
  require(stripes > 0)

  /** The counters. */
  //private val a = new Array[Int](n)

  /** Locks.  locks(j) protects all a(i) such that i%stripes == j. */
  private val locks = Array.fill(stripes)(new Lock)

  def inc(i: Int) = locks(i%stripes).mutex{ a(i) += 1 }

  def dec(i: Int) = locks(i%stripes).mutex{ a(i) -= 1 }

  def get(i: Int) = locks(i%stripes).mutex{ a(i) }
}

// =======================================================

import scala.util.Random

/** A test for the above implementations. */
object CounterArrayTest{
  val iters = 10 // # incs and decs by each thread.

  val Coarse = 0; val Fine = 1; val Striped = 2

  def doTest(switch: Int) = {
    val n = 1+Random.nextInt(20); val numWorkers = 2+Random.nextInt(10)
    val ca: CounterArray = 
      if(switch == Coarse) new CoarseCounterArray(n) 
      else if(switch == Fine) new FineGrainedCounterArray(n)
      else{ 
        val stripes = 1+Random.nextInt(n); new StripedCounterArray(n, stripes)
      }
    def worker = thread("worker"){
      // Generate random permutation of [0..n).
      val random = new Random; val indices = (0 until n).toArray
      for(i <- 0 until n-1){
        // Swap indices(i) and random element of indices[i..n).
        val j = i+random.nextInt(n-i); val t = (j)
        indices(j) = indices(i); indices(i) = t
      }
      // Perform incs and decs in order of indices
      for(_ <- 0 until iters){
        for(i <- indices) ca.inc(i)
        for(i <- indices) ca.dec(i)
      }
    } // end of worker
    run(|| (for (_ <- 0 until numWorkers) yield worker))
    for(i <- 0 until n) assert(ca.get(i) == 0)
  }

  def main(args: Array[String]) = {
    var switch = Coarse; var i = 0
    while(i < args.length) args(i) match{
      case "--fine" => switch = Fine; i += 1
      case "--striped" => switch = Striped; i += 1
      case "--coarse" => switch = Coarse; i += 1
    }

    for(i <- 0 until 10000){ doTest(switch); if(i%200 == 0) print(".") }
    println()
  }
}
