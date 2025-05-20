package tacp.channels

import ox.scl._

/** Object to run Quicksort, using a recursively defined network of threads. */
object QSort{
  // Quicksort: sort data received on in, and output on out
  def qSort(in: ??[Int], out: !![Int]): ThreadGroup = thread("QSort"){
    attempt{
      val pivot = in?()
      val toHigher, toLower, fromHigher, fromLower = new SyncChan[Int]

      // Main controller thread.
      def controller = thread("Controller"){
	// Split data received on in between higher and lower, depending on
	// whether it is >= pivot or < pivot, respectively.
	repeat{ val x = in?(); if(x < pivot) toLower!x else toHigher!x }
	// We've received the final input, so close the channels to the
	// sub-sorters.
	toHigher.endOfStream(); toLower.endOfStream()
	// Now output the results.
	repeat{ out!(fromLower?()) }; out!pivot; repeat{ out!(fromHigher?()) }
	out.endOfStream()
      }      

      // Put the system together, and run it.
      run(
	controller || qSort(toHigher, fromHigher) || qSort(toLower, fromLower)
      )
    }{
      out.endOfStream() // We've received no data, so just close.
    }
  }
}

// =======================================================

import scala.util.Random

object QSortTest{
  // Number of elements to sort; range of input values.
  val MaxSize = 100; val Max = 100

  /** Run a single test.  Generate random inputs.  Pass them in to a sorter.
    * Receive outputs.  Check result is as expected. */
  def doTest = {
    val size = Random.nextInt(MaxSize)
    val xs = Array.fill(size)(Random.nextInt(Max))
    val ys = new Array[Int](size)
    val in, out = new SyncChan[Int]
    def sender = thread("sender"){ for(x <- xs) in!x; in.endOfStream() }
    def receiver = thread("receiver"){ 
      var i = 0; repeat{ ys(i) = out?(); i += 1 } 
    }
    run(sender || QSort.qSort(in, out) || receiver)
    assert(xs.sorted.sameElements(ys),
      "Inputs: "+xs.mkString(", ")+"\nExpected: "+xs.sorted.mkString(", ")+
      "\nReceived: "+ys.mkString(", "))
  }

  def main(args : Array[String]) = {
    for(i <- 0 until 1000){ doTest; if(i%10 == 0) print(".") }
    println()
  }   
}
