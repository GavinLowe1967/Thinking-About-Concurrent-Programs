package tacp.datatypes

import ox.scl._

/** A concurrent iterator producing values of type `A`. */
trait ConcurrentIterator[A]{
  /** Optionally get the next element of the iterator, or |None| if there are no
    * more elements. */ 
  def getNext(): Option[A]
}

// =======================================================

/** A concurrent iterator over the elements of `a`. */
class ArrayIterator[A](a: Array[A]) extends ConcurrentIterator[A]{
  /** Lock to protect `i`. */
  private val lock = new Lock

  private val n = a.length

  /** The index of the next value to return, or `n` if the iteration is
    * complete. */
  private var i = 0

  def getNext(): Option[A] = lock.mutex{
    if(i < n){ i += 1; Some(a(i-1)) } else None
  }
}

// =======================================================

import scala.util.Random

import tacp.trapezium.CollectorLock

/** A test of ArrayIterator. */
object ArrayIteratorTest{
  /* Each test sums a random array concurrently, making use of an ArrayIterator
   * and a Collector, and compares with a sum calculated sequentially. */

  /** Perform a single test. */
  def doTest = {
    val a = Array.fill(Random.nextInt(1000))(Random.nextInt(100))
    val iter = new ArrayIterator(a); val collector = new CollectorLock
    def worker = thread{
      var mySum = 0; var done = false
      while(!done) iter.getNext() match{
        case Some(n) => mySum += n; case None => done = true
      }
      collector.add(mySum)
    }
    run(|| (for(i <- 0 until 8) yield worker))
    assert(collector.get == a.sum)
  }

  def main(args: Array[String]) = {
    for(i <- 0 until 1000){ doTest; if(i%100 == 0) print(".") }
    println()
  }
}

// =======================================================

/** A concurrent iterator that produces iterators over blocks of `a` of size
  * `blockSize`. */
class ArrayBlockIterator[A](a: Array[A], blockSize: Int)
    extends ConcurrentIterator[Iterator[A]]{

  /** A sequential iterator over £a[start..end)£. */
  class BlockIterator[A](a: Array[A], start: Int, end: Int) extends Iterator[A]{
    /** Index of the next element. */
    private var index = start

    def hasNext = index < end

    def next() = { require(index < end); val x = a(index); index += 1; x }
  }

  /** Index of the start of the next block. */
  private var index = 0

  /** Lock to protect `index`. */
  private val lock = new Lock

  def getNext() = lock.mutex{
    if(index < a.length){
      val start = index; index = (index+blockSize) min a.length
      Some(new BlockIterator(a, start, index))
    }
    else None
  }
}

// =======================================================

/** A test of ArrayBlockIterator. */
object ArrayBlockIteratorTest{
  /* Each test sums a random array concurrently, making use of an ArrayIterator
   * and a Collector, and compares with a sum calculated sequentially. */

  /** Perform a single test. */
  def doTest = {
    val a = Array.fill(Random.nextInt(1000))(Random.nextInt(100))
    val bIter = new ArrayBlockIterator(a,12); val collector = new CollectorLock
    def worker = thread{
      var mySum = 0; var done = false
      while(!done) bIter.getNext() match{
        case Some(iter) => while(iter.hasNext) mySum += iter.next() 
        case None => done = true
      }
      collector.add(mySum)
    }
    run(|| (for(i <- 0 until 8) yield worker))
    assert(collector.get == a.sum)
  }

  def main(args: Array[String]) = {
    for(i <- 0 until 1000){ doTest; if(i%100 == 0) print(".") }
    println()
  }
}

// =======================================================

/** A ShardedSet containing elements of type A, together with a mechanism to
  * iterate concurrently over the set. */
class IterableShardedSet[A](shards: Int) extends ShardedSet[A](shards){
  /** A ConcurrentIterator that produces Iterators, which together iterate over
    * all elements of the set.  Each Iterator is over one of the shards.  */
  def iteratorIterator = new ConcurrentIterator[Iterator[A]]{
    /** The index of the next shard over which to provide an Iterator. */
    private var index = 0

    /** Lock to protect index. */
    private val lock = new Lock

    /** Try to get the next Iterator. */
    def getNext(): Option[Iterator[A]] = lock.mutex{
      if(index < shards){ index += 1; Some(sets(index-1).iterator) } else None
    }
  }
}

// =======================================================

/** Test on IterableShardedSet. */
object IterableShardedSetTest{
  val numWorkers = 8

  def doTest = {
    // Add random distinct numbers to a set. 
    val set = new IterableShardedSet[Int](128); var sum = 0; var next = 0
    for(i <- 0 until 10000){
      next += 1+Random.nextInt(10); sum += next; set.add(next)
    }
    // Workers collectively sum set, obtaining iterators, and passing subsums
    // to a Collector.
    val cIter = set.iteratorIterator; val collector = new CollectorLock
    def worker = thread{
      var mySum = 0; var done = false
      while(!done) cIter.getNext() match{
        case Some(iter) =>  while(iter.hasNext) mySum += iter.next() 
        case None => done = true
      }
      collector.add(mySum)
    }
    run(|| (for(i <- 0 until numWorkers) yield worker))
    assert(collector.get == sum)
  }

  def main(args: Array[String]) = {
    for(i <- 0 until 5000){ doTest; if(i%100 == 0) print(".") }
    println()
  }

}
