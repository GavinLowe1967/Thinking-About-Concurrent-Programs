package tacp.interactingPeers

import ox.scl._

trait GridMaxT{
  def apply(): Array[Array[Int]]
}


class GridMax(n: Int, xss: Array[Array[Int]]) extends GridMaxT{
  require(n >= 1 && xss.length == n && xss.forall(_.length == n))

  /** Array to hold results. */
  private val results = Array.ofDim[Int](n,n)

  /** Worker with coordinates (i,j) and starting value x, with channels to allow
    * data to be passed upwards and rightwards. */
  private def worker(i: Int, j: Int, x: Int, readUp: ??[Int], writeUp: !![Int], 
                     readRight: ??[Int], writeRight: !![Int])
    = thread("worker"+(i,j)){
    var myMax = x
    // propogate values along rows
    for(k <- 1 until n){
      writeRight!myMax; myMax = myMax max readRight?()
    }
    // propogate values upwards
    for(k <- 1 until n){
      writeUp!myMax; myMax = myMax max readUp?()
    }
    // Store result
    results(i)(j) = myMax
  }

  /** Channels by which values are passed rightwards; indexed by coords of
    * recipients. */
  private val right = Array.fill(n,n)(new OnePlaceBuffChan[Int])

  /** Channels by which values are passed upwards; indexed by coords of
    * recipients. */
  private val up = Array.fill(n,n)(new OnePlaceBuffChan[Int])

  /** Run the system, and return array storing results obtained. */
  def apply(): Array[Array[Int]] = {
    val workers = 
      || (for(i <- 0 until n; j <- 0 until n) yield
            worker(i, j, xss(i)(j),
                   up(i)(j), up(i)((j+1)%n), right(i)(j), right((i+1)%n)(j)))
    run(workers)
    results
  }
}

// =======================================================

class LogGridMax(n: Int, xss: Array[Array[Int]]) extends GridMaxT{
  private val n2 = n*n

  /** Array to hold results. */
  private val results = Array.ofDim[Int](n,n)

  /* Each node (i,j) will have a pseudo-identity n*i+j in [0..n2).  We arrange
   * the nodes into a heap using these pseudo-identities.  
   * 
   * The following channels are for messages going up and down the heap, and
   * are indexed by the child identities. */
  private val up, down = Array.fill(n2)(new SyncChan[Int]) 

  def worker(i: Int, j: Int, x: Int) = thread(s"worker($i, $j)"){
    val myId = n*i+j; val child1 = 2*myId+1; val child2 = 2*myId+2
    var myMax = x
    if(child1 < n2) myMax = myMax max up(child1)?()
    if(child2 < n2) myMax = myMax max up(child2)?()
    if(myId != 0){ up(myId)!myMax; myMax = down(myId)?() }
    if(child1 < n2) down(child1)!myMax
    if(child2 < n2) down(child2)!myMax
    results(i)(j) = myMax
  }

  /** Run the system, and return array storing results obtained. */
  def apply(): Array[Array[Int]] = {
    val workers = 
      || (for(i <- 0 until n; j <- 0 until n) yield worker(i, j, xss(i)(j)))
    run(workers)
    results
  }
}
