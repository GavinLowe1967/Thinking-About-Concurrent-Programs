package tacp.interactingPeers

import ox.scl._

trait RingFoldT[T]{
  def apply(): ThreadGroup
}

/** Calculate the fold of f over xs. */
class RingFold[T](xs: Array[T], f: (T,T) => T, outs: Array[SyncChan[T]]) 
    extends RingFoldT[T]{
  private val n = xs.length
  require(n >= 2 && outs.length == n)

  /** The channels connecting nodes, indexed by the recipient's identity:
    * node(i) receives on chans(i) and sends on chans((i+1)%n). */
  private val chans = Array.fill(n)(new SyncChan[T]) 

  /** A single node. */
  private def node[T](me: Int) = thread{
    val left = chans(me); val right = chans((me+1)%n)
    val out = outs(me); val x = xs(me)
    if(me == 0){
      right!x // Start things going.
      val result = left?(); right!result // Receive final result and pass it on.
      out!result
    }
    else{
      val y = left?(); right!f(y,x) // Add my value to value being passed.
      val result = left?()          // Receive final result.
      if(me != n-1) right!result    // Pass it on if I'm not the last node.
      out!result
    }
  }  

  /** The complete ring. */
  def apply(): ThreadGroup = || (for(i <- 0 until n) yield node(i))
}

// =======================================================

/** Calculate the fold of f over xs for associative and commutative f. */
class ACRingFold[T](xs: Array[T], f: (T,T) => T, outs: Array[SyncChan[T]]) 
    extends RingFoldT[T]{
  private val n = xs.length
  require(n >= 2 && outs.length == n)

  /** The channels connecting nodes, indexed by the recipient's identity:
    * node(i) receives on chans(i) and sends on chans((i+1)%n).  The channels
    * need to be buffered. */
  private val chans = Array.fill(n)(new OnePlaceBuffChan[T]) 

  /** A single node. */
  private def node[T](me: Int) = thread{
    val left = chans(me); val right = chans((me+1)%n)
    val out = outs(me); val x = xs(me); var y = x
    // Inv: at the start of round i, y = foldl f xs[me-i..me], where the indices
    // are interpreted mod n. 
    for(i <- 0 until n-1){
      right!y         // = foldl f xs[me-i..me]
      val z = left?() // = foldl f xs[me-1-i..me-1]
      y = f(z, x)     // = foldl f xs[me-(i+1)..me], maintaining invariant
    }
    // y = foldl f xs[me-(n-1)..me] = foldl f xs since f is AC.
    out!y
  }  

  /** The complete ring. */
  def apply(): ThreadGroup = || (for(i <- 0 until n) yield node(i))
}

// =======================================================

import scala.util.Random

object RingFoldTest{
  /** A thread that expects to receive expected on each channel in chans, and
    * throws an exception if an incorrect value is received. */
  def checker(chans: Array[SyncChan[Int]], xs: Array[Int], f: (Int,Int) => Int)
  = thread{
    // Calculate expected value
    val expected = xs.reduceLeft(f)
    for(chan <- chans){ 
      val res = chan?(); 
      assert(res == expected, 
        "xs = "+xs.mkString(",")+s"; expected = $expected; received = $res")
    }
  }

  /** A single test of either RingFold or ACRingFold, using random values.
    * @param ac use ACRingFold if true */
  def doTest(ac: Boolean) = {
    val n = 2+Random.nextInt(20)
    val xs = Array.fill(n)(Random.nextInt(100))
    val outs = Array.fill(n)(new SyncChan[Int])
    val a, b, c = Random.nextInt(20)
    // println(s"$a $b $c")
    def f(x: Int, y: Int) = if(ac) x+y+c else a*x+b*y+c
    val rf: RingFoldT[Int] =
      if(ac) new ACRingFold[Int](xs, f, outs)
      else new RingFold[Int](xs, f, outs)
    run(rf() || checker(outs, xs, f))
  }

  def main(args: Array[String]) = {
    // Expect command line argument "AC" for the associative, commutative
    // version.
    val ac = args.nonEmpty && args(0) == "AC"
    for(i <- 0 until 5000){ doTest(ac); if(i%100 == 0) print(".") }
    println()
  }
}
