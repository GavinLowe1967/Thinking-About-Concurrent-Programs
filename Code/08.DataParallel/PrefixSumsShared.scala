package tacp.dataParallel

import ox.scl._

class PrefixSumsShared(n: Int, a: Array[Int]) extends PrefixSumsT{
  require(n == a.size)

  /** Shared array, in which sums are calculated. */
  private val sum = new Array[Int](n) 

  /** Barrier synchronisation object. */
  private val barrier = new Barrier(n)

  /** An individual thread.  summer(me) sets sum[me] equal to sum(a[0..me]). */
  private def summer(me : Int) = thread("Summer "+me){
    // Invariant: gap = 2^r and sum(me) = sum a(me-gap .. me] 
    // (with fictious values a(i) = 0 for i < 0).  r is the round number.

    // Each round uses two barrier synchronisations, one at the start and one
    // in the middle: between the two synchronisations, all sum variables are
    // treated as read-only; after the second synchronisation, sum(k) may be
    // written by Summer(k).
    
    var r = 0; var gap = 1; sum(me) = a(me)

    while(gap < n){ 
      barrier.sync(me)
      if(gap <= me){                
        val inc = sum(me-gap)    // inc = sum a(me-2*gap .. me-gap].
        barrier.sync(me)
        sum(me) = sum(me) + inc  // s = sum a(me-2*gap .. me].
      }
      else barrier.sync(me)
      r += 1; gap += gap;        // s = sum a(me-gap .. me].
    }
  }

  // Put system together
  def apply(): Array[Int] = {
    run (|| (for (i <- 0 until n) yield summer(i)))
    sum
  }
}

/** Prefix sums example, using shared variables and two arrays. */
class PrefixSumsShared2(n: Int, a: Array[Int]) extends PrefixSumsT{
  require(n == a.size)

  /** Shared array, in which sums are calculated. */
  private val sum1, sum2 = new Array[Int](n) 

  private var result: Array[Int] = null

  /** Barrier synchronisation object. */
  private val barrier = new Barrier(n)

  /** An individual thread.  summer(me) sets sum[me] equal to sum(a[0..me]). */
  private def summer(me : Int) = thread("Summer "+me){
    // Invariant: gap = 2^r and prevSum(me) = \sum a(me-gap .. me] 
    // (with fictious values a(i) = 0 for i < 0).  r is the round number.

    // Each round uses two barrier synchronisations, one at the start and one
    // in the middle: between the two synchronisations, all sum variables are
    // treated as read-only; after the second synchronisation, sum(k) may be
    // written by Summer(k).
    
    var currentSum = sum1; var prevSum = sum2
    var r = 0; var gap = 1; prevSum(me) = a(me)
    barrier.sync(me)

    while(gap < n){ 
      val inc = if(gap <= me) prevSum(me-gap) else 0  // inc = \sum a(me-2*gap .. me-gap].
      currentSum(me) = prevSum(me) + inc // currentSum(me) = \sum a(me-2*gap .. me].
      barrier.sync(me)
      r += 1; gap += gap;        // currentSum(me) = sum a(me-2*gap .. me].
      val t = prevSum; prevSum = currentSum; currentSum = t 
      // prevSum(me) = \sum a(me-gap .. me].
    }
    if(me == 0) result = prevSum
  }

  // Put system together
  def apply(): Array[Int] = {
    run (|| (for (i <- 0 until n) yield summer(i)))
    result
  }
}

