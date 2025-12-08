package tacp.datatypes

import ox.scl._

/** Class providing support for sharding. */
abstract class Sharding[A](shards: Int){
  require(shards > 1)

  /** Improve a hash code. */
  private def improve(hcode: Int): Int = {
    var h = hcode + ~(hcode << 9)
    h = h ^ (h >>> 14); h = h + (h << 4); h ^ (h >>> 10)
  }

  /** The log of shards.  Check this is a power of 2. */
  private def logShards = {
    var s = shards; var i = 0
    while(s > 1){ s = s >> 1; i += 1 }
    require(shards == 1 << i, 
      s"The number of shards should be a power of 2, received $shards.")
    i
  }

  /** The amount to shift hash codes to obtain the index of the relevant
    * shard. */
  private val shift = 32-logShards

  /** The shard in which x is stored. */ 
  protected def shardFor(x: A) = improve(x.hashCode) >>> shift
}
