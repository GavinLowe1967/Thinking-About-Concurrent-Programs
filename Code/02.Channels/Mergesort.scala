package tacp.channels
import ox.scl._

object Mergesort{
  /** Merge sorted streams received on `in1` and `in2` into a single sorted
    * stream on `out`. */
  private def merge(in1: ??[Int], in2: ??[Int], out: !![Int]) = {
    var x1 = in1?(); var x2 = in2?(); var closed1 = false; var closed2 = false
    // Inv: closed1 is true if in1 has been detected as closed; otherwise x1
    // holds the last value read from in1, which has not yet been output.  And
    // similarly for closed2, x2, in2.
    while(!closed1 && !closed2){
      if(x1 <= x2){ out!x1; attempt{ x1 = in1?() }{ closed1 = true } }
      else{ out!x2; attempt{ x2 = in2?() }{ closed2 = true } }
    }
    if(closed2){ out!x1; repeat{ out!(in1?()) } }
    else{ assert(closed1); out!x2; repeat{ out!(in2?()) } }
    out.endOfStream()
  }

  /** Peform mergesort on the stream of data received on `in`, outputting the
    * sorted stream on `out`. */
  def mergesort(in: ??[Int], out: !![Int]): ThreadGroup = thread("mergesort"){
    var x1 = -1; var x2 = -1
    attempt{ 
      x1 = in?()
      attempt{
        x2 = in?()
        val to1, to2, from1, from2 = new SyncChan[Int]
        def controller = thread("controller"){
          to1!x1; to2!x2
          repeat{ to1!(in?()); to2!(in?()) }
          to1.endOfStream(); to2.endOfStream()
          merge(from1, from2, out)
        } // End of controller.
        // Put system together and run it.
        run( controller || mergesort(to1, from1) || mergesort(to2, from2) )
      }{ out!x1; out.endOfStream() } // Received only x1.
    }{ out.endOfStream() } // Received empty stream.
  }

  import scala.util.Random

  // Max number of elements to sort; range of input values.
  val MaxSize = 100; val Max = 100

  /** Run a single test.  Generate random inputs.  Pass them in to a
    * sorter.  Receive outputs.  Check result is as expected. */
  def doTest = {
    val size = Random.nextInt(MaxSize)
    val xs = Array.fill(size)(Random.nextInt(Max))
    val ys = new Array[Int](size)
    val in, out = new SyncChan[Int]
    def sender = thread("sender"){ for(x <- xs) in!x; in.endOfStream() }
    def receiver = thread("receiver"){ 
      var i = 0; repeat{ ys(i) = out?(); i += 1 } 
    }
    run(sender || mergesort(in, out) || receiver)
    assert(xs.sorted.sameElements(ys))
  }

  def main(args: Array[String]) = {
    for(i <- 0 until 1000){
      doTest; if(i%10 == 0) print(".")
    }
    println()
  }


}


