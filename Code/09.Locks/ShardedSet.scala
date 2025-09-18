package tacp.datatypes

import ox.scl._

trait Set[A]{
  /** Does this contain x? */
  def contains(x: A): Boolean

  /** Add x to this.  Return true if x was not already in the set. */
  def add(x: A): Boolean 

  /** Remove x from this.  Return true if x was previously in the set. */
  def remove(x: A): Boolean
}



/** Useful functions supporting sharding. */
object Sharding{
  /** Improve a hash code. */
  def improve(hcode: Int): Int = {
    var h: Int = hcode + ~(hcode << 9)
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

import Sharding._

/** A sharded set containing elements of type A, using `shards` shards. */
class ShardedSet[A](shards: Int) extends Set[A]{
  require(shards > 1)

  /** The amount to shift hash codes to obtain the index of the relevant
    * shard. */
  private val shift = 32-logShards(shards)

  /** The shard in which x is stored. */ 
  private def shardFor(x: A) = improve(x.hashCode) >>> shift

  /** The shards.  This ShardedSet object represents the union of sets. */ 
  private val sets = Array.fill(shards)(new scala.collection.mutable.HashSet[A])

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

// =======================================================

import scala.util.Random

/** Linearisability tester for the ShardedSet. */
object ShardedSetTest{
  var iters = 200  // Number of iterations by each worker.
  val MaxVal = 200 // Maximum value placed in the set.

  // Sequential and concurrent datatypes.
  type SeqSet = scala.collection.immutable.HashSet[Int]
  type ConcSet = ShardedSet[Int]

  // Operations on the sequential datatype.
  def seqAdd(x: Int)(s: SeqSet): (Boolean, SeqSet) = 
    if(s.contains(x)) (false, s) else (true, s+x)

  def seqContains(x: Int)(s: SeqSet): (Boolean, SeqSet) = 
    (s.contains(x), s)

  def seqRemove(x: Int)(s: SeqSet): (Boolean, SeqSet) = 
    if(s.contains(x)) (true, s-x) else (false, s)

  /** A worker. */
  def worker(me: Int, log: LinearizabilityLog[SeqSet, ConcSet]) = {
    val random = new Random(scala.util.Random.nextInt()+me*45207)
    for(i <- 0 until iters){
      val x = random.nextInt(MaxVal)
      random.nextInt(3) match{
        case 0 => log(_.add(x), s"add($x)", seqAdd(x))
        case 1 => log(_.contains(x), s"contains($x)", seqContains(x))
        case 2 => log(_.remove(x), s"remove($x)", seqRemove(x))
      }
    }
  } 

  /** Perform a single test. */
  def doTest = {
    val s = 1 << (1+Random.nextInt(6)) // Number of shards. 
    var p = 1+Random.nextInt(10) // Number of workers.
    val concSet = new ConcSet(s); val seqSet = new SeqSet
    val tester = 
      LinearizabilityTester[SeqSet,ConcSet](seqSet, concSet, p, worker)
    if(tester() <= 0) sys.exit()
  }

  def main(args: Array[String]) = {
    for(r <- 0 until 10000){ doTest; if(r%50 == 0) print(".") }
    println()
  }

}
