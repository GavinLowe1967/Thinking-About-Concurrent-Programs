package tacp.datatypes

import ox.scl._

import scala.util.Random

/** Linearisability tester for the ShardedMap. */
object MapTest{
  var iters = 200  // Number of iterations by each worker.
  val MaxVal = 200 // Maximum key or value placed in the set.

  // Sequential and concurrent datatypes.
  type SeqMap = scala.collection.immutable.HashMap[Int,Int]
  type ConcMap = Map[Int,Int]

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
  private var linkedList = false


  /** Perform a single test. */
  def doTest = {
    val s = 1 << (1+Random.nextInt(6)) // Number of shards.
    var p = 1+Random.nextInt(10) // Number of workers.
    val concMap: ConcMap = 
      if(linkedList) new LinkedListMap else  new ShardedMap(s);
    val seqMap = new SeqMap
    val tester = 
      LinearizabilityTester[SeqMap,ConcMap](seqMap, concMap, p, worker)
    if(tester() <= 0) sys.exit()
  }

  def main(args: Array[String]) = {
    var i = 0
    while(i < args.length) args(i) match{
      case "--linkedList" => linkedList = true; i += 1
    }

    for(r <- 0 until 10000){ doTest; if(r%50 == 0) print(".") }
    println()
  }
}
