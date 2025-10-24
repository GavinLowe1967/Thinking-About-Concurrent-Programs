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
