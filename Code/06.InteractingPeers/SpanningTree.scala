package tacp.interactingPeers

import ox.scl._

import scala.collection.mutable.ArrayBuffer

/** Build a spanning tree for the graph with `n` nodes [0..`n`), and adjacency
  * relation given by `adjacency`.  The adjacency relation should be symmetric
  * and irreflexive. */ 
class SpanningTree(n: Int, adjacency: Array[Array[Boolean]]){
  require(adjacency.length == n && adjacency.forall(_.length == n))

  private type NodeId = Int
  private type Edge = (NodeId,NodeId)

  private trait Msg
  /** A message from `pred` telling the recipient to start exploration. */
  private case class Start(pred: NodeId) extends Msg
  /** A message indicating that the sender has constructed a subtree using
    * `edges`. */
  private case class Finished(edges: List[Edge]) extends Msg
  /** Message indicating that the relevant edge should not be included. */
  //private case object No extends Msg

  private var result: List[Edge] = null

  private val chans = Array.ofDim[BuffChanT[Msg]](n,n)

  private val verbose = false 

  /** A node with identity `me`.  The channels in `ins` and `outs` connect this
    * node with its neighbours, with `ins(i)` and `outs(i)` connecting to the
    * same neighbour. */
  private def node(me: Int, ins: Array[??[Msg]], outs: Array[!![Msg]]) 
  = thread(s"node($me)"){
    val size = ins.length; require(outs.length == size); var edges = List[Edge]()
    if(size != 0){
      // Identity of this node's predecessor, and its index in the arrays.
      var pred = -1; var pIx = -1; var terminated = false
      // Waiting phase
      if(me != 0) attempt{
        alt(| ( 
          for(i <- 0 until size)
          yield ins(i) =?=> { case Start(p) => pred = p; pIx = i }
        ) )
      }{ terminated = true; if(verbose) println(s"$me: terminated") }
      if(!terminated){
        // Sending phase
        if(verbose)println(s"$me: sending; pred = $pred; pIx = $pIx")
        for(i <- 0 until size; if i != pIx) outs(i)!Start(me)
        // Receiving phase
        val pending = Array.fill(size)(true)
        if(pIx >= 0) pending(pIx) = false
        if(verbose) println(s"$me: receiving; "+pending.mkString(", "))
        // Receive back Finished message from any neighbour we're waiting on.
        serve(| (
          for(i <- 0 until size)
          yield pending(i) && ins(i) =?=> { m =>
            if(verbose) println(s"$me: received $m")
            m match{
              case Finished(es) => edges = es++edges
              case Start(_) => // outs(i)!No
              // case No =>
            }
            pending(i) = false
          }
        ) ) // end of serve
        // Finishing phase
        if(pIx >= 0) outs(pIx)!Finished((pred,me)::edges)
      } // end of if(!terminated)
      if(verbose) println(s"$me: finishing: $edges")
    } // end of if(ins.nonEmpty)
    if(me == 0){
      result = edges
      for(i <- 0 until n; j <- 0 until n){
        val c = chans(i)(j); if(c != null) c.close()
      }
    } 
  }

  def apply(): List[Edge] = {
    // Initialise channels
    // chans(i)(j) is from i to j
    for(i <- 0 until n; j <- 0 until n; if adjacency(i)(j)){
// TODO: work out why capacity 2 needed.  Required for 4 or 5 nodes, I think.
      assert(i != j && adjacency(j)(i))
      chans(i)(j) = new /* BuffChan[Msg](2) */ OnePlaceBuffChan[Msg]
    }
    // In-ports for node i.
    def mkIns(i: NodeId): Array[??[Msg]] = {
      val ins1 = new ArrayBuffer[??[Msg]]
      for(j <- 0 until n; if adjacency(i)(j)) ins1 += chans(j)(i)
      ins1.toArray
    }
    // Out-ports for node i.
    def mkOuts(i: NodeId): Array[!![Msg]] = {
      val outs1 = new ArrayBuffer[!![Msg]]
      for(j <- 0 until n; if adjacency(i)(j)) outs1 += chans(i)(j)
      outs1.toArray
    }
    run(|| (for(i <- 0 until n) yield node(i, mkIns(i), mkOuts(i))))
    result
  }
}

// =======================================================

import scala.util.Random

object SpanningTreeTest{
  /** Run a single test on SpanningTree. */
  def doTest = {
    val n = 2+Random.nextInt(9)
    val adjacency = Array.ofDim[Boolean](n,n)
    for(i <- 0 until n; j <- 0 until i; if Random.nextDouble() <= 0.3){ 
      adjacency(i)(j) = true; adjacency(j)(i) = true 
    }
    val tree = new SpanningTree(n, adjacency)()

    // Check `adjacency` and `result` are compatible.
    def errMsg = adjacency.map(_.mkString(", ")).mkString("\n")+"\n"+tree
    // Nodes reached by the tree.
    val reached = new Array[Boolean](n); reached(0) = true
    for((a,b) <- tree){
      assert(reached(a), s"Unreached source of edge: ($a, $b)\n"+errMsg)
      assert(!reached(b), s"Node reached by two paths: $b\n"+errMsg)
      assert(adjacency(a)(b), s"Non-edge used: ($a, $b)\n"+errMsg) 
      reached(b) = true
    }
    // Check no other node should have been reached.
    for(i <- 0 until n; if !reached(i); j <- 0 until n)
      assert(!(reached(j) && adjacency(i)(j)), s"$i not reached:\n"+errMsg)
  }

  def main(args: Array[String]) = {
    for(i <- 0 until 1000000){ 
      doTest; if(i%100 == 0) print("."); if(i%10000 == 0) print(i) 
    }
    println()
  }

}
