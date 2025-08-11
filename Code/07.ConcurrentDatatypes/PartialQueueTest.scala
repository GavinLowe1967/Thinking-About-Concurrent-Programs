package tacp.datatypes

import scala.collection.immutable.Queue
import ox.scl._

object PartialQueueTest{
  var iters = 20 // Number of iterations by each worker
  // Note: higher values of iters lead to large memory usage.
  val MaxVal = 20 // Maximum value placed in the queue
  val p = 4      // Number of workers

  type SeqQueue = Queue[Int]; type ConcQueue = PartialQueue[Int]

  def seqEnqueue(x: Int)(q: SeqQueue) : (Unit, SeqQueue) = 
    ((), q.enqueue(x))
  def seqDequeue(q: SeqQueue) : (Int, SeqQueue) = {
    require(q.nonEmpty); q.dequeue
  }

  /** A worker for the LinTesters */
  def worker(me: Int, log: LinearizabilityLog[SeqQueue, ConcQueue]) = {
    val random = new scala.util.Random
    for(i <- 0 until iters){
      if(me%2 == 0){
        val x = random.nextInt(MaxVal)
        log(_.enqueue(x), s"enqueue($x)", seqEnqueue(x))
      }
      else log(_.dequeue, "dequeue", seqDequeue)
    }
  }

  def doTest = {
    val concQueue = new ServerPartialQueue[Int]; val seqQueue = Queue[Int]()
    val tester =
      LinearizabilityTester[SeqQueue,ConcQueue](seqQueue, concQueue, p, worker)
    if(tester() <= 0) sys.exit()
    concQueue.shutdown()
  }


  def main(args: Array[String]) = {
    // parse arguments
    var i = 0; var queueType = "server"
    var reps = 10000  // Number of repetitions
    while(i < args.length) args(i) match{
      case "--iters" => iters = args(i+1).toInt; i += 2 
      case "--reps" => reps = args(i+1).toInt; i += 2 
      //case "--monitor" => queueType = "monitor"; i += 1
      // case "--semaphore" => queueType = "semaphore"; i += 1
      // case "--countingSemaphore" => queueType = "counting semaphore"; i += 1
      case arg => println("Unrecognised argument: "+arg); sys.exit()
    }

    for(r <- 0 until reps){ doTest; if(r%50 == 0) print(".") } 
    println()
  }
}
