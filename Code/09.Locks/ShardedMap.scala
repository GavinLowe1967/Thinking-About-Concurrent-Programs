package tacp.datatypes

import ox.scl._

import Sharding._

/** A sharded map from K to V, using `shards` shards. */
class ShardedMap[K,V](shards: Int){
  require(shards > 1)

  /** The amount to shift hash codes to obtain the index of the relevant
    * shard. */
  private val shift = 32-logShards(shards)

  /** The shard in which k is stored. */ 
  private def shardFor(k: K) = improve(k.hashCode) >>> shift

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

// =======================================================

import scala.util.Random

/** Linearisability tester for the ShardedMap. */
object ShardedMapTest{
  var iters = 200  // Number of iterations by each worker.
  val MaxVal = 200 // Maximum key or value placed in the set.

  // Sequential and concurrent datatypes.
  type SeqMap = scala.collection.immutable.HashMap[Int,Int]
  type ConcMap = ShardedMap[Int,Int]

  // Operations on the sequential datatype.
  def seqPut(k: Int, v: Int)(s: SeqMap): (Option[Int], SeqMap) = 
    (s.get(k), s + (k -> v))

  def seqGet(k: Int)(s: SeqMap): (Option[Int], SeqMap) = 
    (s.get(k), s)

  def seqRemove(k: Int)(s: SeqMap): (Option[Int], SeqMap) = 
    (s.get(k), s - k)

  /** A worker. */
  def worker(me: Int, log: LinearizabilityLog[SeqMap, ConcMap]) = {
    val random = new Random(scala.util.Random.nextInt()+me*45207)
    for(i <- 0 until iters){
      val k = random.nextInt(MaxVal)
      random.nextInt(3) match{
        case 0 => 
          val v = random.nextInt(MaxVal)
          log(_.put(k,v), s"put($k.$v)", seqPut(k,v))
        case 1 => log(_.get(k), s"get($k)", seqGet(k))
        case 2 => log(_.remove(k), s"remove($k)", seqRemove(k))
      }
    }
  } 

  /** Perform a single test. */
  def doTest = {
    val s = 1 << (1+Random.nextInt(6)) // Number of shards.
    var p = 1+Random.nextInt(10) // Number of workers.
    val concMap = new ConcMap(s); val seqMap = new SeqMap
    val tester = 
      LinearizabilityTester[SeqMap,ConcMap](seqMap, concMap, p, worker)
    if(tester() <= 0) sys.exit()
  }

  def main(args: Array[String]) = {
    for(r <- 0 until 10000){ doTest; if(r%50 == 0) print(".") }
    println()
  }
}
