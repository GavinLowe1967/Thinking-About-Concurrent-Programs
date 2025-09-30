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

/** Useful functions supporting sharding. */
object Sharding{
  /** Improve a hash code. */
  def improve(hcode: Int): Int = {
    var h = hcode + ~(hcode << 9)
    h = h ^ (h >>> 14); h = h + (h << 4); h ^ (h >>> 10)
  }

  /** The log of shards.  Check this is a power of 2. */
  def logShards(shards: Int) = {
    var s = shards; var i = 0
    while(s > 1){ s = s >> 1; i += 1 }
    require(shards == 1 << i, 
      s"The number of shards should be a power of 2, received $shards.")
    i
  }
}

// =======================================================

/** A sharded set containing elements of type A, using `shards` shards. */
class ShardedSet[A](shards: Int) extends Set[A]{
  require(shards > 1)

  /** The amount to shift hash codes to obtain the index of the relevant
    * shard. */
  private val shift = 32-Sharding.logShards(shards)

  /** The shard in which x is stored. */ 
  private def shardFor(x: A) = Sharding.improve(x.hashCode) >>> shift

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

