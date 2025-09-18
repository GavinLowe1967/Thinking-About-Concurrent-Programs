package tacp.datatypes

import ox.scl._

import scala.util.Random

/** Linearisability tester for the ShardedSet. */
object SetTest{
  var iters = 200  // Number of iterations by each worker.
  val MaxVal = 200 // Maximum value placed in the set.

  // Sequential and concurrent datatypes.
  type SeqSet = scala.collection.immutable.HashSet[Int]
  type ConcSet = Set[Int]

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

  private var linkedList = false

  /** Perform a single test. */
  def doTest = {
    val s = 1 << (1+Random.nextInt(6)) // Number of shards. 
    var p = 1+Random.nextInt(10) // Number of workers.
    val concSet: Set[Int] = 
      if(linkedList) new LinkedListSet[Int] else new ShardedSet[Int](s)
    val seqSet = new SeqSet
    val tester = 
      LinearizabilityTester[SeqSet,ConcSet](seqSet, concSet, p, worker)
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
