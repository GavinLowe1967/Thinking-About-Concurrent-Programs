package tacp.datatypes

// A simple implementation of partial solutions
class Partial{
  /** Array holding the digits played so far, with 0 representing a blank
    * square. */
  private val contents = Array.ofDim[Int](9,9)

  /** Initialise from a file. */
  def init(fname: String) = {
    val lines = scala.io.Source.fromFile(fname).getLines()
    for(i <- 0 until 9){
      val line = lines.next()
      for(j <- 0 until 9){
	val c = line.charAt(j)
	if(c.isDigit) contents(i)(j) = c.asDigit
	else { assert(c=='.'); contents(i)(j) = 0 }
      }
    }
  }

  /** Print. */
  def printPartial() = {
    for(i <- 0 until 9){
      for(j <- 0 until 9) print(contents(i)(j))
      println()
    }
    println()
  }

  /** Is the partial solution complete? */
  def complete : Boolean = {
    for(i <- 0 until 9; j <- 0 until 9) if(contents(i)(j) == 0) return false
    true
  }

  /** Find a blank position; precondition: complete returns false. */
  def nextPos: (Int,Int) = {
    for(i <- 0 until 9; j <- 0 until 9) if(contents(i)(j) == 0) return (i,j)
    throw new RuntimeException("No blank position")
  }

  /** Can we play value d in position (i,j); precondition: (i,j) is blank. */
  def canPlay(i:Int, j:Int, d:Int): Boolean = {
    // Check if d appears in row i
    for(j1 <- 0 until 9) if(contents(i)(j1) == d) return false
    // Check if d appears in column j
    for(i1 <- 0 until 9) if(contents(i1)(j) == d) return false
    // Check if d appears in this 3x3 block
    val basei = i/3*3; val basej = j/3*3
    for(i1 <- basei until basei+3; j1 <- basej until basej+3)
      if(contents(i1)(j1) == d) return false
    // All checks passed
    true
  }

  /** Create a new partial solution, extending this one by playing d in
    * position (i,j). */
  def play(i:Int, j:Int, d:Int) : Partial = {
    val p = new Partial
    for(i1 <- 0 until 9; j1 <- 0 until 9) 
      p.contents(i1)(j1) = contents(i1)(j1)
    p.contents(i)(j) = d
    p
  }
}

// =======================================================

/** A graph corresponding to Sudoku problems.  From each node p of the graph,
  * there is a path to every solution of p.  Further, there is at most one
  * path between any pair of nodes p1 and p2. */
object SudokuGraph extends Graph[Partial]{
  /** The successors of a particular partial solution.
    * 
    * It is guaranteed that any solution of p is also a solution of a member
    * of succs(p), and vice versa.  Further, each element of succs(p) has
    * fewer blank squares than p.
    * 
    * Pre: !p.complete. */
  def succs(p: Partial): List[Partial] = {
    val (i,j) = p.nextPos
    (for(d <- 1 to 9; if p.canPlay(i, j, d)) yield p.play(i, j, d)).toList
  }
}

// -------------------------------------------------------

/** A program for solving Sudoku problems, based on a DFGraphSearch object.  
  * 
  * This expects to find the filename for the starting position as the first
  * argument on the command line.  */
object Sudoku{
  val numWorkers = 8 // The number of workers.

  def main(args: Array[String]) = {
    // Parse command line arguments.
    var fname = ""; var useSeq = false; var i = 0
    while(i < args.length) args(i) match{
      case "--seq" => useSeq = true; i += 1
      case w => 
        if(fname.isEmpty) fname = w 
        else{ println(s"Extra argument: $w"); sys.exit() }
        i += 1
    }
    if(fname.isEmpty){ println("Missing filename"); sys.exit() }

    val p = new Partial; p.init(fname)
    val g = SudokuGraph
    val solver: DFGraphSearch[Partial] =
      if(useSeq) new SeqDFGraphSearch(g) 
      else new ConcDFGraphSearch(g, numWorkers)
    solver(p, _.complete) match{
      case Some(p1) => p1.printPartial()
      case None => println("No solution found.")
    }
  }
}
