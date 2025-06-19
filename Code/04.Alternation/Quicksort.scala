package tacp.alternation

import ox.scl._

/** Trait implemented by different implementations of Quicksort, providing the
  * partition function. */
trait QuicksortT{
  protected val a: Array[Int]

  /** Partition a[l..r), returning the index of the pivot.  Return i such that
    * a[l..i) <= a(i) < a[i+1..r) and l <= i < r.  Permute only the segment
    * a[l..r). */
  protected def partition(l: Int, r: Int): Int = {
    assert(r-l > 1)
    val x = a(l); var i = l+1; var j = r
    // Invariant a[l+1..i) <= x = a(l) < a[j..r) && l < i <= j <= r.
    while(i < j){
      while(i < j && a(i) <= x) i += 1
      while(i < j && a(j-1) > x) j -= 1
      if(i < j){ // a(i) > x && a(j-1) <= x, so swap.
        val t = a(i); a(i) = a(j-1); a(j-1) = t 
      }
    }
    // Swap pivot into position i-1, and return that index.
    a(l) = a(i-1); a(i-1) = x; i-1
  }
}

// =======================================================

/** Sequential implementation of Quicksort. */
class Quicksort(protected val a: Array[Int]) extends QuicksortT{
  /** Sort a. */
  def sort() = qsort(0, a.length)

  /** Sort the segment a[l..r). */
  private def qsort(l: Int, r: Int): Unit =
    if(r-l > 1){ val m = partition(l, r); qsort(l,m); qsort(m+1,r) }
}

// =======================================================

/** Basic concurrent implementation. */
class ConcurrentQuicksort1(protected val a: Array[Int], numWorkers: Int)
    extends QuicksortT{
  /** A Task is a pair (l,r), representing the task of making progress on the
    * interval a[l..r). */
  type Task = (Int, Int)

  /** The bag of tasks. */
  object Bag{
    private val getC = new SyncChan[Task]
    private val addC = new SyncChan[(Task,Task)]
    private val doneC = new SyncChan[Unit]

    def get(): Task = getC?()

    def add(t1: Task, t2: Task): Unit = addC!(t1,t2)

    def done(): Unit = doneC!()

    private def server = thread("server"){
      val queue = new scala.collection.mutable.Queue[Task]
      queue.enqueue((0, a.length))
      var busyWorkers = 0
      serve(
        queue.nonEmpty && getC =!=> { busyWorkers += 1; queue.dequeue() }
        | busyWorkers > 0 && addC =?=> { case (t1,t2) => 
            queue.enqueue(t1); queue.enqueue(t2); busyWorkers -= 1 }
        | busyWorkers > 0 && doneC =?=> { _ => busyWorkers -= 1 }
      )
      assert(queue.isEmpty && busyWorkers == 0)
      getC.endOfStream()
    }

    fork(server)
  } // End of Bag.

  /** A single worker. */
  private def worker = thread("worker"){
    repeat{
      val (l,r) = Bag.get()
      if(r-l > 1){ val m = partition(l, r); Bag.add((l,m), (m+1,r)) }
      else Bag.done()
    }
  }

  /** Sort a. */
  def sort() = run(|| (for(i <- 0 until numWorkers) yield worker))
}

// =======================================================

/** Improved concurrent implementation. */
class ConcurrentQuicksort2(protected val a: Array[Int], numWorkers: Int)
    extends QuicksortT{

  /** A Task is a pair (l,r), representing the task of making progress on the
    * interval a[l..r). */
  type Task = (Int, Int)

  /** The bag of tasks. */
  object Bag{
    private val getC = new SyncChan[Task]
    private val addC = new SyncChan[Task]
    private val doneC = new SyncChan[Unit]

    def get(): Task = getC?()

    def add(t: Task): Unit = addC!t

    def done(): Unit = doneC!()

    private def server = thread("server"){
      if(true){ // Use queue.
        val queue = new scala.collection.mutable.Queue[Task]
        queue.enqueue((0, a.length)); var busyWorkers = 0
        serve(
          queue.nonEmpty && getC =!=> { busyWorkers += 1; queue.dequeue() }
          | busyWorkers > 0 && addC =?=> { t => queue.enqueue(t) }
          | busyWorkers > 0 && doneC =?=> { _ => busyWorkers -= 1 }
        )
        assert(queue.isEmpty && busyWorkers == 0)
        getC.endOfStream()
      }
      else{ // Use stack.  This seems slightly slower.
        val stack = new scala.collection.mutable.Stack[Task]
        stack.push((0, a.length)); var busyWorkers = 0
        serve(
          stack.nonEmpty && getC =!=> { busyWorkers += 1; stack.pop() }
          | busyWorkers > 0 && addC =?=> { t => stack.push(t) }
          | busyWorkers > 0 && doneC =?=> { _ => busyWorkers -= 1 }
        )
        assert(stack.isEmpty && busyWorkers == 0)
        getC.endOfStream()
      }
    }

    fork(server)
  } // End of Bag.

  /** Lower limit on size of task for which subtasks should be returned to the
    * bag. */
  val Limit = 20_000 max (a.size / (10*numWorkers))

  private def qsort(l: Int, r: Int): Unit =
    if(r-l > 1){ val m = partition(l, r); qsort(l,m); qsort(m+1,r) }

  /** A single worker. */
  private def worker = thread("worker"){
    var l = -1; var r = -1
    // A value of l and r with l < r indicates that this worker is still
    // responsible for the segment a[l..r).  However, if l = r, the worker
    // has no current segment.
    repeat{
      assert(l <= r)
      if(l == r){ 
        val task = Bag.get(); l = task._1; r = task._2; assert(r-l > 1) 
      }  
      if(r-l < Limit){ qsort(l,r); r = l; Bag.done() }
      else{
        val m = partition(l, r)
        if(m-l > 1){ // Continue with (l,m).
          if(r-(m+1) > 1) Bag.add((m+1,r))
          r = m
        }
        else if(r-(m+1) > 1) l = m+1 // Continue with (m+1,r).
        else{ r = l; Bag.done() } // This segment finished.
      }
    }
  }

  /** Sort a. */
  def sort() = if(a.size > 1) run(|| (for(i <- 0 until numWorkers) yield worker))
}


// =======================================================

/** Test of the implementations. */
object QuicksortTest{
  import scala.util.Random
  import java.lang.System.nanoTime

  // Max size of array; max value in arrays.
  val MaxSize = 100; val Max = 100

  var sequential = false; var concurrent1 = false; var concurrent2 = false 

  val NumWorkers = 8

  /** Sort a. */
  private def doSort(a: Array[Int]) = {
    if(sequential) new Quicksort(a).sort() 
    else if(concurrent1) new ConcurrentQuicksort1(a, NumWorkers).sort()
    else{ assert(concurrent2); new ConcurrentQuicksort2(a, NumWorkers).sort() }
  }

  /** Do a single test. */
  private def doTest = {
    val n = Random.nextInt(MaxSize); val a = Array.fill(n)(Random.nextInt(Max))
    val a1 = a.clone.sorted; doSort(a)
    assert(a.sameElements(a1), a.mkString(", ")+"\n"+a1.mkString(", "))
  }

  private def doTiming(size: Int) = {
    val a = Array.fill(size)(Random.nextInt(size))
    val t0 = nanoTime; doSort(a)
    println(s"${(nanoTime-t0)/1000000} ms")
  }
  // concurrent2 is faster than sequential for size greater than about
  // 1_000_000.

  def main(args: Array[String]) = {
    // Parse arguments
    var i = 0; var timing = false; var size = -1
    while(i < args.length) args(i) match{
      case "--timing" => timing = true; size = args(i+1).toInt; i += 2
      case "--seq" => sequential = true; i += 1
      case "--conc1" => concurrent1 = true; i += 1
      case "--conc2" => concurrent2 = true; i += 1
    }

    // Run the tests. 
    if(timing) doTiming(size)
    else{
      val t0 = nanoTime
      for(i <- 0 until 10000){ doTest; if(i%100 == 0) print(".") }
      println(s"\n${(nanoTime-t0)/1_000_000} ms")
    }
  }

}
