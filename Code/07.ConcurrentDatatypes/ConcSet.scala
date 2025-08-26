package tacp.datatypes

import ox.scl._

trait ConcSet[A]{
  /** Add x to this set.  Return true if x was not previously in the set. */
  def add(x: A): Boolean 

  def shutdown(): Unit
}

// =======================================================

/** A concurrent set with an add operation. */
class ServerConcSet[A] extends ConcSet[A]{
  private type ReplyChan = OnePlaceBuffChan[Boolean]

  /** Channel from clients to the server. */
  private val addC = new SyncChan[(A, ReplyChan)]

  /** Add x to this set.  Return true if x was not previously in the set. */
  def add(x: A): Boolean = {
    val c = new ReplyChan; addC!(x,c); c?()
  }

  private def server = thread{
    val set = new scala.collection.mutable.HashSet[A]
    repeat{ val (x,c) = addC?(); val res = set.add(x); c!res }
  }

  fork(server)

  /** Shut down the object. */
  def shutdown() = addC.close()
}

// =======================================================

/** A concurrent set with an add operation.  Implementation using a monitor. */
class MonitorConcSet[A] extends ConcSet[A]{
  /** The underlying set. */
  private val set = new scala.collection.mutable.HashSet[A]

  /** Add x to this set.  Return true if x was not previously in the set. */
  def add(x: A): Boolean = synchronized{ set.add(x) }

  def shutdown() = {}
}

// =======================================================

/** A tester for ServerConcSet. */
object SetTest{
  // # iterations by each worker
  var iters = 200

  // max value added to the set
  var maxValue = 20

  // # runs
  var reps = 1000

  // Sequential specification type
  type S = scala.collection.immutable.HashSet[Int]

  // Type of concurrent object to be tested.
  type C = ConcSet[Int]

  // Sequential add operation
  def seqAdd(x: Int)(set: S): (Boolean, S) = 
    if(set.contains(x)) (false, set) else (true, set+x)

  // A worker thread
  def worker(me: Int, log: LinearizabilityLog[S,C]) = {
    val random = new scala.util.Random
    for(i <- 0 until iters){
      val x = random.nextInt(maxValue)
      log(_.add(x), s"add($x)", seqAdd(x))
    }
  }

  def doTest = {
    val concSet = new ServerConcSet[Int]; val seqSet = new S
    val tester = LinearizabilityTester[S, C](seqSet, concSet, 4, worker)
    if(tester() <= 0) sys.exit()
    concSet.shutdown()
  }

  // The main method
  def main(args: Array[String]) = {
    for(i <- 0 until reps){ doTest; if(i%10 == 0) print(".") }
    println()
  }
}
