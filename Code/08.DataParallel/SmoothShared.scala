package tacp.dataParallel

import ox.scl._

/** This version uses two arrays. */
class SmoothShared(a: Smooth.Image, p: Int, maxIters: Int){
  private val n = a.length; private val w = a(0).length
  require(a.forall(_.length == w))

  // The two arrays.  We set a1 to be the original a. 
  private val a1 = a; private val a2 = Array.ofDim[Boolean](n,w)

  /** Barrier used at the end of each round, and also to decide termination. */
  private val combBarrier = new AndBarrier(p)

  /** Worker thread. */
  private def worker(me: Int) = thread("worker"+me){
    val start = me*n/p; val end = (me+1)*n/p
    // This worker is responsible for rows [start..height).
    var oldA = a1; var newA = a2  // Old and new versions of the image.
    var done = false; var iters = 0

    while(!done && iters < maxIters){
      var myDone = true // have no values changed yet?
      for(i <- start until end; j <- 0 until w){
        newA(i)(j) = Smooth.majority(oldA, i, j)
        myDone &&= (newA(i)(j) == oldA(i)(j))
      }
      iters += 1; done = combBarrier.sync(me, myDone) // Synchronise with others
      if(!done && iters < maxIters){ val t = oldA; oldA = newA; newA = t }
    }

    // Copy newA into a, if not already equal. 
    if(newA != a) for(i <- start until end) a(i) = newA(i)
  }

  /** Smooth the image. */
  def apply() = run( || (for (i <- 0 until p) yield worker(i)) )
}
