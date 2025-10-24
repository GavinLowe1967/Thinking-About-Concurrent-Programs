package tacp.datatypes

import ox.scl._

trait Map[K,V]{
  /** Optionally get the value associated with k. */
  def get(k: K): Option[V] 

  /** Add the association k -> v.  Optionally return the value previously
    * associated with k. */
  def put(k: K, v: V): Option[V] 

  /** Remove any association for k.  Optionally return the value previously
    * associated with k. */
  def remove(k: K): Option[V] 
}

// =======================================================

/** A sharded map from K to V, using `shards` shards. */
class ShardedMap[K,V](shards: Int) extends Sharding[K](shards) with Map[K,V]{
  /** The shards.  This ShardedMap object represents the union of maps. */ 
  private val maps = 
    Array.fill(shards)(new scala.collection.mutable.HashMap[K, V])

  /** Locks to protect sets: locks(i) protects sets(i). */
  private val locks = Array.fill(shards)(new Lock)

  /** Add the association k -> v.  Optionally return the value previously
    * associated with k. */
  def put(k: K, v: V): Option[V] = {
    val s = shardFor(k); locks(s).mutex{ maps(s).put(k, v) }
  }

  /** Optionally get the value associated with k. */
  def get(k: K): Option[V] = {
    val s = shardFor(k); locks(s).mutex{ maps(s).get(k) }
  }

  /** Remove any association for k.  Optionally return the value previously
    * associated with k. */
  def remove(k: K): Option[V] = {
    val s = shardFor(k); locks(s).mutex{ maps(s).remove(k) }
  }
}


