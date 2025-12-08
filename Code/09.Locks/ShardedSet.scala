package tacp.datatypes

import ox.scl._

/** Trait for a set containing elements of type A. */
trait Set[A]{
  /** Does this contain x? */
  def contains(x: A): Boolean

  /** Add x to this.  Return true if x was not already in the set. */
  def add(x: A): Boolean 

  /** Remove x from this.  Return true if x was previously in the set. */
  def remove(x: A): Boolean
}

// =======================================================

/** A sharded set containing elements of type A, using `shards` shards. */
class ShardedSet[A](shards: Int) extends Sharding[A](shards) with Set[A]{

  /** The shards.  This ShardedSet object represents the union of sets. */ 
  protected val sets = 
    Array.fill(shards)(new scala.collection.mutable.HashSet[A])

  /** Locks to protect sets: locks(i) protects sets(i). */
  private val locks = Array.fill(shards)(new Lock)

  /** Does this contain x? */
  def contains(x: A): Boolean = {
    val s = shardFor(x); locks(s).mutex{ sets(s).contains(x) }
  }

  /** Add x to this.  Return true if x was not already in the set. */
  def add(x: A): Boolean = {
    val s = shardFor(x); locks(s).mutex{ sets(s).add(x) }
  }

  /** Remove x from this.  Return true if x was previously in the set. */
  def remove(x: A): Boolean = {
    val s = shardFor(x); locks(s).mutex{ sets(s).remove(x) }
  }
}

