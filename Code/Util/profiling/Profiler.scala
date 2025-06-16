/** Some profiling functions */
package tacp.util.profiling

import scala.collection.mutable.ArrayBuffer

/** The object implements a number of named timers and named counters.  Code 
  * can be timed, with the time taken added to that of the named timer.  
  * Similarly, the number of times particular points in the code are reached
  * can be counted, with the count recorded against a named counter. 
  *
  * The normal usages are:
  *
  *  - `Profiler.time("timer-name"){ code }`,
  *    which times `code`, recording the time against timer `timer-name`, and
  *   returns the result of `code`;
  *  - `Profiler.count("counter-name")`, which adds one onto counter 
  *    `counter-name`;
  *  - `Profiler.report` which gives a summary of the profiling.
  *
  * Note that normal scoping rules apply.  In code such as
  * `Profiler.time("answer"){ val x = 6*7 }`, the scope of `x` will  be just
  * the inner statement; normal use would be 
  * `val x = Profiler.time("answer"){ 6*7 }`.
  *
  * For use with concurrent code, it is recommended to start by calling
  * `setWorkers(p)` to set the number of concurrent threads to `p` 
  * (8 by default).  If two threads have the same ID (mod `p`) then they
  * may interfere, both in terms of correctness, and through creating cache
  * contention.
  *
  * As with all profilers, there is a fairly significant overhead.
  */
object Profiler{
  /** Number of workers */
  private var p = 64

  /** The names of timers. */
  private var tnames = Array.fill(p)(new ArrayBuffer[String]())

  /** The values of timers.  times(w)(i) contains the time recorded by
    * worker w against timer tnames(w)(i). */
  private var times = Array.fill(p)(new ArrayBuffer[Long]())

  // Invariant: tnames.length = times.length = p
  // forall w in [0..p) . tnames(w).length = times(w).length
  // This represents the mapping 
  // { tname -> \sum { times(w)(i) | w <- [0..p), i <- [0..tnames(w).length), 
  //                                 tnames(w)(i) = tname } }

  /** The names of counters */
  private var cnames = Array.fill(p)(new ArrayBuffer[String]())

  /** The counters. */
  private var counts = Array.fill(p)(new ArrayBuffer[Long]())

  // Invariant: cnames.length = counts.length = p
  // forall w in [0..p) . cnames(w).length = counts(w).length
  // This represents the mapping 
  // { cname -> \sum { counts(w)(i) | w <- [0..p), i <- [0..cnames(w).length), 
  //                                  cnames(w)(i) = cname } }

  /** Locks used to protect shared variables.  These guard against the number
    * of workers being set incorrectly. */
  private var locks = Array.fill(p)(new AnyRef)

  /** Set the number of concurrent workers */
  def setWorkers(p: Int) = { 
    this.p = p; locks = Array.fill(p)(new AnyRef); clear 
  }

  /** Clear all counters and timers. */
  def clear = {
    tnames = Array.fill(p)(new ArrayBuffer[String]())
    times = Array.fill(p)(new ArrayBuffer[Long]())
    cnames = Array.fill(p)(new ArrayBuffer[String]())
    counts = Array.fill(p)(new ArrayBuffer[Long]())
  }

  /** Time cmd, recording the duration against the current thread */
  def time[A](tname:String)(cmd: => A) : A = {
    val w = ThreadID.get % p // this worker's ID
    var i = 0; val myNames = tnames(w); val myTimes = times(w)
    // Find the index for the timer tname
    locks(w).synchronized{
      while(i < myNames.length && myNames(i) != tname) i += 1
      if(i == myNames.length){ myNames += tname; myTimes += 0 }
    }
    // start the timer
    val t0 = java.lang.System.currentTimeMillis()
    // run the code
    try{ cmd } finally {
      // stop the timer and record the duration
      val duration = java.lang.System.currentTimeMillis()-t0
      locks(w).synchronized{ myTimes(i) += duration }
    }
  }

  // @deprecated("Two-argument notime", "21/10/2013") @inline 
  // def notime[A](w: Int, tname:String)(cmd: => A) : A = cmd

  /** Do not time cmd.
    * This function is provided to allow timing to be easily turned on and off.
    * Note, though, htat it has a non-zero cost. */
  @inline def notime[A](tname:String)(cmd: => A) : A = cmd

  /** Increment the counter, recording it against the current thread.
    * @param increment the amount of the increment (default 1). */
  def count(cname: String, increment: Int = 1) = {
    val w = ThreadID.get % p // this worker's ID
    assert(w<p, "Not enough entries in Profiler for worker "+w+
	   "; use Profiler.setWorkers to set the number of workers.")
    val myNames = cnames(w); val myCounts = counts(w)
    // Find the index for the counter cname
    var i = 0
    locks(w).synchronized{
      while(i < myNames.length && myNames(i) != cname) i += 1
      if(i == myNames.length){ myNames += cname; myCounts += 0 }
      myCounts(i) += increment
    }
  }

  /** Get a summary of the profiler results.
    * @return a pair(tPairs, cPairs), where tPairs gives the result for the 
    * timers as a List of (timer-name, time) pairs, and cPairs gives the
    * result of the counters as a List of (counter-name, count) pairs. */
  def getReport : (List[(String,Long)], List[(String,Long)]) = {
    // (timer name, time) pairs
    val tPairs0 : Seq[(String,Long)] = 
      (for(w <- 0 until p) yield tnames(w).zip(times(w))).flatten
    // (counter name, count) pairs
    val cPairs0 : Seq[(String,Long)] = 
      for(w <- 0 until p; i <- 0 until cnames(w).length)
      yield (cnames(w)(i), counts(w)(i))

    // Merge pairs with same name; pre: list is sorted. 
    def mergePairs(pairs: Seq[(String, Long)]) : List[(String, Long)] = {
      val pairsA = pairs.toArray; val N = pairsA.length
      var i = 0; var result = List[(String, Long)]()
      // result holds the merger of pairsA[0..i); those keys are disjoint from
      // later keys.
      while(i < N){
        val (st,n) = pairsA(i); var c = n.asInstanceOf[Long]; i += 1
        while(i < N && pairsA(i)._1 == st){ c += pairsA(i)._2; i += 1 }
        result ::= ((st,c))
      }
      result.reverse
    }

    // Recursive version; gives stack overflow on large cases.
    //def mergePairs[A : Numeric](pairs: Seq[(String, A)]) : Seq[(String, A)] = {
    // if(pairs.isEmpty) pairs
    // else{
    //   val (st,n) = pairs.head
    //   val (matches, others) = pairs.span(_._1==st)
    //   (st, matches.map(_._2).sum) +: mergePairs(others)
    // }

    (mergePairs(tPairs0.sorted), mergePairs(cPairs0.sorted))
  }

  /** Get the value associated with a particular counter. */
  def getCounter(name: String) : Long = {
    var result = 0L
    for(w <- 0 until p; i <- 0 until cnames(w).length; if cnames(w)(i) == name)
      result += counts(w)(i)
    result
  }

  /** Convert n to a String, with commas if at least 4 digits. */
  private def toCommaString(n: Long) : String = {
    // Insert commas into a string representing a long, whose length is a
    // multiple of 3
    def iter(st: String) : String =
      if(st.length == 3) st else st.take(3)+","+iter(st.drop(3))

    val nStr = n.toString; val len = nStr.length
    if(len <= 3) nStr
    else{
      val pref = nStr.take(len%3); val rest = nStr.drop(len%3)
      if(pref.nonEmpty) pref+","+iter(rest) else iter(rest)
    }
  }


  /** Print report of all timers and counters. */
  def report = synchronized{
    val (tPairs, cPairs) = getReport

    // max width of names
    val maxW = 
      if(tPairs.isEmpty && cPairs.isEmpty) -1
      else ( (for((tn,_) <- tPairs) yield tn.length) ++
             (for((cn,_) <- cPairs) yield cn.length) ) .max

    if(tPairs.nonEmpty){
      println("TIMERS:")
      for((tn,t) <- tPairs)
        println(tn+": "+(" "*(maxW-tn.length))+toCommaString(t))
      println()
    }
    if(cPairs.nonEmpty){
      println("COUNTERS:")
      for((cn,c) <- cPairs)
        println(cn+": "+(" "*(maxW-cn.length))+toCommaString(c))
    }
    // Reset here?
  }

}
