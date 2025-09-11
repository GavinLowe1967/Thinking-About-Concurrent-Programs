package tacp.dataParallel


object Smooth{
  type Row = Array[Boolean]
  type Image = Array[Row]

   /** Test if at least half of the neightbours of a(i)(j) are set. */
   def majority(a: Image, i: Int, j: Int): Boolean = {
     val n = a.length; val w = a(0).length
     var sum = 0 // # set neighbours so far
     var count = 0 // # neighbours so far
     for(i1 <- i-1 to i+1; if i1 >= 0 && i1 < n;
         j1 <- j-1 to j+1; if j1 >= 0 && j1 < w){
       count += 1; if(a(i1)(j1)) sum += 1
     }
     2*sum >= count
   }

  /** Print a. */
  def printArray(a: Image) = 
    println(a.map(_.map(b => if(b) "1" else "0").mkString).mkString("\n"))
}

/** A sequential smoothing algorithm. */
class SmoothSequential(a: Smooth.Image, maxIters: Int){
  private val n = a.length; private val w = a(0).length
  assert(a.forall(_.length == w))

  def apply() = {
    var done = false; var iters = 0

    while(!done && iters < maxIters){
      done = true
      val newA = Array.ofDim[Boolean](n, w)
      for(i <- 0 until n; j <- 0 until w){
	newA(i)(j) = Smooth.majority(a, i, j)
	done &&= (newA(i)(j) == a(i)(j))
      }
      iters += 1
      for(i <- 0 until n) a(i) = newA(i) // update for next round
    }
  }
}
      
