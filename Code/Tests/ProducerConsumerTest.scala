package tacp.tests

import ox.scl._

import tacp.monitors.ProducerConsumer

object ProducerConsumerTest{
  // Number of iterations by each producer or consumer, respectively.
  val pIters = 100; val cIters = 200
  val n = 3  // Size of each buffer.
  val numProducers = 6 // Number of producers
  // Number of items produced equals number of items consumed. 
  assert(pIters*numProducers == n*cIters)
  val maxValue = 20 // Maximum value added to the buffer.

  /** Sequential specification type.  Note the sequential specification type
    * needs to be immutable, and equality needs to correspond to "==", so we
    * can't use an array here. */
  type S = List[Int]

  /** Sequential put operation. */
  def seqPut(x: Int)(buffer: S): (Unit, S) = {
    require(buffer.length < n); ((), buffer :+ x)
  }
  // It might be better to build buffer in reverse, to avoid the O(n) ":+"

  /** Sequential get operation. */
  def seqGet(buffer: S): (List[Int], S) = {
    require(buffer.length == n); (buffer, List[Int]())
  }

  /** A worker thread. Thread 0 is the consumer. */
  def worker(me: Int, log: LinearizabilityLog[S,ProducerConsumer]) = {
    val random = new scala.util.Random
    if(me == 0) for(i <- 0 until cIters) log(_.get.toList, "get", seqGet)
    else 
      for(i <- 0 until pIters){
        val x = random.nextInt(maxValue); log(_.put(x), s"put($x)", seqPut(x))
      }
  }
  // Note that with the get method, we cast the result to a List, to allow
  // comparison using "==".

  val Monitor = 0; val Semaphore = 1

  /** Perform a single test. */
  def doTest(choice: Int) = {
    val concBuffer: ProducerConsumer = choice match{
      case Monitor => new tacp.monitors.MonitorProducerConsumer(n)
      case Semaphore => new tacp.semaphores.SemaphoreProducerConsumer(n)
    }
    val seqBuffer = List[Int]()
    val tester = LinearizabilityTester[S, ProducerConsumer](
      seqBuffer, concBuffer, 2*n+1, worker _)
    if(tester() <= 0) sys.exit()
  }


  // The main method
  def main(args: Array[String]) = {
    var choice = 0; var i = 0
    while(i < args.length) args(i) match{
      case "--semaphore" => choice = Semaphore; i += 1
    }
    for(i <- 0 until 5000){ doTest(choice); if(i%10 == 0) print(".") }
    println()
  }
}
