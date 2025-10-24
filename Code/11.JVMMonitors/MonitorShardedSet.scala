package tacp.datatypes

import ox.scl._

/** A sharded set containing elements of type A, using `shards` shards. */
class MonitorShardedSet[A](shards: Int) extends Sharding[A](shards) with Set[A]{
  /** The shards.  This ShardedSet object represents the union of sets. */ 
  protected val sets = 
    Array.fill(shards)(new scala.collection.mutable.HashSet[A])

  /** Does this contain x? */
  def contains(x: A): Boolean = {
    val s = shardFor(x); sets(s).synchronized{ sets(s).contains(x) }
  }

  /** Add x to this.  Return true if x was not already in the set. */
  def add(x: A): Boolean = {
    val s = shardFor(x); sets(s).synchronized{ sets(s).add(x) }
  }

  /** Remove x from this.  Return true if x was previously in the set. */
  def remove(x: A): Boolean = {
    val s = shardFor(x); sets(s).synchronized{ sets(s).remove(x) }
  }
}

 
 
