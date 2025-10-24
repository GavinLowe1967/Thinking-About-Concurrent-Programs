package tacp.locks

import ox.scl._

import scala.util.Random

/** A test for the above implementations. */
object CounterArrayTest{
  val iters = 10 // # incs and decs by each thread.

  val Coarse = 0; val Fine = 1; val Striped = 2; val JVM = 3

  def doTest(switch: Int) = {
    val n = 1+Random.nextInt(20); val numWorkers = 2+Random.nextInt(10)
    val ca: CounterArray = 
      if(switch == Coarse) new CoarseCounterArray(n) 
      else if(switch == Fine) new FineGrainedCounterArray(n)
      else{ 
        val stripes = 1+Random.nextInt(n)
        if(switch == Striped) new StripedCounterArray(n, stripes) 
        else new tacp.jvmMonitors.StripedCounterArray(n, stripes)
      }
    def worker = thread("worker"){
      // Generate random permutation of [0..n).
      val random = new Random; val indices = (0 until n).toArray
      for(i <- 0 until n-1){
        // Swap indices(i) and random element of indices[i..n).
        val j = i+random.nextInt(n-i); val t = (j)
        indices(j) = indices(i); indices(i) = t
      }
      // Perform incs and decs in order of indices
      for(_ <- 0 until iters){
        for(i <- indices) ca.inc(i)
        for(i <- indices) ca.dec(i)
      }
    } // end of worker
    run(|| (for (_ <- 0 until numWorkers) yield worker))
    for(i <- 0 until n) assert(ca.get(i) == 0)
  }

  def main(args: Array[String]) = {
    var switch = Coarse; var i = 0
    while(i < args.length) args(i) match{
      case "--fine" => switch = Fine; i += 1
      case "--striped" => switch = Striped; i += 1
      case "--coarse" => switch = Coarse; i += 1
      case "--JVM" => switch = JVM; i += 1
    }

    for(i <- 0 until 10000){ doTest(switch); if(i%200 == 0) print(".") }
    println()
  }
}
