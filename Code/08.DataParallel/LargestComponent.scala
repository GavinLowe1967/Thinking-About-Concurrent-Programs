package tacp.dataParallel

import ox.scl._
import scala.math.Ordering.Implicits._

/** The superclass of the concurrent and sequential algorithm classes.  This
  * is a convenient place for common code. */
abstract class LargestComponent(image: Array[Array[Int]]){
  val Height = image.length; require(Height > 0)
  val Width = image(0).length; require(image.forall(_.length == Width))

  /** An index, giving the coordinates of a value. */
  type Index = (Int,Int)

  type IndexArray = Array[Array[Index]]

  /** The smallest index (in indexes) of (r,c) and its same-colour
    * neighbours. */
  protected def smallestNeighbourIndex(indexes: IndexArray, r: Int, c: Int)
      : Index = {
    var minIx = indexes(r)(c)
    for((r1,c1) <- List((r-1,c), (r+1,c), (r,c-1), (r,c+1)))
      if(r1 >= 0 && r1 < Height && c1 >= 0 && c1 < Width && 
          image(r)(c) == image(r1)(c1) && indexes(r1)(c1) < minIx)
        minIx = indexes(r1)(c1)
    minIx
  }

  /** Print a matrix of Indexes. */
  protected def printIndexes(indexes: IndexArray) = 
    for(r <- 0 until Height){
      for(c <- 0 until Width) print(indexes(r)(c).toString+"\t")
      println()
    }

  /** Return the minimum index in the largest connected component, and the size
    * of that component.  In the case of equal size largest components, return
    * the one with the smaller minimum index. */
  def apply(): (Index, Int)
}

// =======================================================

/** Concurrent program to find the largest connected component in an image. 
  * @param image the image to be worked on.  In the range of the array, each
  * value represents a different colour.*/
class ConcLargestComponent(image: Array[Array[Int]], p: Int) 
    extends LargestComponent(image){

  /** Combining barrier, used in stage 1 for coordination and deciding
    * termination. */
  private val barrier1 = new OrBarrier(p)

  /** Variable that holds the final result. */
  private var result: (Index, Int) = null

  /** Arrays that hold the indexes on alternate rounds. */
  private val indexes0, indexes1 = Array.ofDim[Index](Height,Width)

  /** A List representing a count of Indexes.  Each entry gives an Index and a
    * count for it.  The list is ordered by Indexes. */
  private type IndexCount = List[(Index,Int)]

  /** Merge two IndexCount lists. */
  private def merge(counts1: IndexCount, counts2: IndexCount): IndexCount = {
    var cs1 = counts1; var cs2 = counts2
    val result = new scala.collection.mutable.ArrayBuffer[(Index,Int)]()
    while(cs1.nonEmpty && cs2.nonEmpty){
      val (ix1,c1) = cs1.head; val (ix2,c2) = cs2.head
      if(ix1 < ix2){ result += ((ix1, c1)); cs1 = cs1.tail }
      else if(ix1 == ix2){ 
        result += ((ix1, c1+c2)); cs1 = cs1.tail; cs2 = cs2.tail 
      }
      else{ result += ((ix2, c2)); cs2 = cs2.tail }
    }
    cs1.foreach(result += _); cs2.foreach(result += _)
    result.toList
  }

  /** Combining barrier for state 2. */
  private val barrier2 = new CombiningBarrier(p, merge)

  /** A worker responsible for rows [start..end) where start = id*height, end =
    * (id+1)*height.  This worker may also read rows start-1 (if start > 0)
    * and end (if end < Height). */
  private def worker(id: Int) = thread("worker"+id){
    // STAGE 1: calculate the index of each location.

    // Initialise my share of the indexes in indexes0
    val start = id*Height/p; val end = (id+1)*Height/p
    for(r <- start until end; c <- 0 until Width) indexes0(r)(c) = (r,c)
    barrier1.sync(id, false) // dummy argument, or use a separate barrier
    var lastIndexes = indexes0; var theseIndexes = indexes1
    var done = false

    while(!done){
      var change = false // have we seen a change on this round?
        // Update my share of theseIndexes based on lastIndexes
      for(r <- start until end; c <- 0 until Width){
        // Find minimum index between (r,c) and its neighbours
        val minIx = smallestNeighbourIndex(lastIndexes, r, c)
        theseIndexes(r)(c) = minIx
        if(minIx != lastIndexes(r)(c)) change = true
      }
      // Sync with others; set done = true if no thread saw any change.
      done = ! barrier1.sync(id, change)
      // Swap theseIndexes and lastIndexes for the next round.
      if(!done){
        val t = theseIndexes; theseIndexes = lastIndexes; lastIndexes = t
      }
    } // end of while(!done)

    // STAGE 2: collectively count the number in each component.
    // Build up counts for rows start to end in a map.
    val map = new scala.collection.mutable.HashMap[Index,Int]
    for(r <- start until end; c <- 0 until Width){
      val ix = theseIndexes(r)(c)
      map.get(ix) match{
        case Some(n) => map += ix -> (n+1)
        case None => map += ix -> 1
      }
    }
    var myCounts = map.iterator.toList.sorted
    // Merge the workers' myCounts arrays.
    val allCounts = barrier2.sync(id, myCounts)
    // worker(0) writes final result to result variable.
    if(id == 0) result = allCounts.maxBy(_._2)
  } // end of worker


  /** Find the size of the largest connected component using p workers. */
  def apply(): (Index, Int) = {
    run(|| (for(i <- 0 until p) yield worker(i)))
    result
  }
}

// =======================================================

class SeqLargestComponent(image: Array[Array[Int]])
    extends LargestComponent(image){

  def apply(): (Index, Int) = {
    // PART 1
    var lastIndexes = Array.tabulate(Height, Width){ case (r,c) => (r,c) }
    var theseIndexes = Array.ofDim[Index](Height, Width)
    var done = false
    while(!done){
      done = true
      for(r <- 0 until Height; c <- 0 until Width){
        // Find minimum index between (r,c) and its neighbours
        val minIx = smallestNeighbourIndex(lastIndexes, r, c)
        theseIndexes(r)(c) = minIx
        if(minIx != lastIndexes(r)(c)) done = false
      }
      if(!done){ // swap
        val t = theseIndexes; theseIndexes = lastIndexes; lastIndexes = t
      }
    }

    // PART 2
    val allIndexes = theseIndexes.flatten.sorted // All indexes, in order
    val size = Height*Width; assert(allIndexes.length == size)
    // Index with most occurrences so far, and its count
    var maxCount = -1; var maxIndex = (-1,-1); var i = 0
    while(i < size){
      // Count occurrences of allIndexes(i)
      var j = i+1; while(j < size && allIndexes(j) == allIndexes(i)) j += 1
      val count = j-i
      if(count > maxCount){ maxCount = count; maxIndex = allIndexes(i) }
      i = j
    }
    (maxIndex, maxCount)
  }
}

// =======================================================

import scala.util.Random

/** Test harness, comparing the concurrent algorithm against the sequential
  * one. */
object LargestComponentTest{
  /** Run a single test. */
  def doTest = {
    val p = 2+Random.nextInt(10) // # threads
    val Height = p*(Random.nextInt(6))+1+Random.nextInt(10) // # rows
    val Width = 1+Random.nextInt(20)
    val colours = 2+Random.nextInt(2)
    // Generate and print random image
    val image = Array.fill(Height,Width)(Random.nextInt(colours))
    // Run the algorithm
    val seqResult = new SeqLargestComponent(image)()
    val concResult = new ConcLargestComponent(image, p)()
    if(seqResult != concResult){
      for(r <- 0 until Height){ 
        for(c <- 0 until Width) print(image(r)(c)); println()
      }
      println(s"seqResult = $seqResult; concResult = $concResult")
      sys.exit()
    }
  }

  def main(args: Array[String]) = {
    for(i <- 0 until 1000){ doTest; if(i%10 == 0) print(".") }
    println()
  }
}
