package tacp.interactingPeers

import ox.scl._

trait LeadershipElection{
  /** Run the protocol using identity `id`, receiving messages on `in`, and
    * sending on `out`. */
  def apply(id: Int, in: ??[Int], out: !![Int]): Boolean
}

// =======================================================

class SimpleLeaderRing extends LeadershipElection{
  def apply(id: Int, in: ??[Int], out: !![Int]): Boolean = {
    var max = id; var done = false; out!id
    while(!done){
      val x = in?()
      if(x > max) max = x
      else if(x == id) done = true
      if(!done) out!x    
    }
    max == id
  }
}

// =======================================================

/** The Peterson Leadership Election protocol. 
  * This implementation assumes all identities are non-negative. */
class PetersonLeader extends LeadershipElection{
  def apply(id: Int, in: ??[Int], out: !![Int]): Boolean = {
    var relaying = false; var leader = false; var tid = id
    while(!leader && !relaying){      
      // out!tid; val x = in?(); out!x; val y = in?()
      // if(x == tid){ out!(-1); leader = true }
      // else if(x > tid && x > y) tid = x
      // else relaying = true 
      out!tid; val x = in?()
      if(x == tid){ out!(-1); leader = true }
      else{
        out!x; val y = in?()
        if(x > tid && x > y) tid = x
        else relaying = true
      }
    }
    // Relaying
    while(relaying){
      val x = in?(); out!x
      if(x < 0) relaying = false
    }
    leader
  }
}


// =======================================================

import scala.util.Random

object LeadershipElectionTest{
  /** A random permutation of [0..n). */
  def randomIds(n: Int): Array[Int] = {
    val a = Array.tabulate(n)(i => i)
    for(i <- 0 until n-1){
      val j = i+Random.nextInt(n-i)
      val t = a(i); a(i) = a(j); a(j) = t  // swap a(i), a(j)
    }
    a
  }

  var sel = "Simple"

  def doTest = {
    val lr: LeadershipElection = 
      if(sel == "Peterson") new PetersonLeader
      else{ assert(sel == "Simple"); new SimpleLeaderRing }
    val n = 1+Random.nextInt(100) // Number of nodes in the ring.
    // println(s"n = $n")
    // chans(i) goes from thread(i-1) to thread(i)
    val chans = Array.fill(n)(new OnePlaceBuffChan[Int])
    val ids = randomIds(n); val results = new Array[Boolean](n)
    def worker(me: Int) = thread{
      results(me) = lr(ids(me), chans(me), chans((me+1)%n))
    }
    run( || (for(i <- 0 until n) yield worker(i)) )
    // Check exactly one node thinks it's the leader.
    var count = 0
    for(i <- 0 until n; if(results(i))) count += 1
    assert(count == 1, results.mkString(", "))
  }

  def main(args: Array[String]) = {
    if(args.nonEmpty) sel = args(0)
    for(i <- 0 until 10000){ doTest; if(i%100 == 0) print(".") }
    println()
  }
}
