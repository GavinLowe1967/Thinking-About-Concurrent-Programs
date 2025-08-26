package tacp.datatypes

import ox.scl._
import scala.collection.mutable.Queue

/** A partial queue that terminates if all worker threads are attempting to
  * dequeue, and the queue is empty.
  * @param numWorkers the number of worker threads. */
class TerminatingPartialQueue[A](numWorkers: Int){
  /** Channel for enqueueing. */
  private val enqueueChan = new SyncChan[A]

  private type ReplyChan = SyncChan[Option[A]]

  /** Channel for dequeueing. */
  private val dequeueChan = new SyncChan[ReplyChan]

  /** Channel for shutting down the queue. */
  private val shutdownChan = new SyncChan[Unit]

  /** Enqueue x.
    * @throws Closed if the queue has been shutdown. */
  def enqueue(x: A): Unit = enqueueChan!x

  /** Attempt to dequeue a value.
    * @throws Closed if the queue has been shutdown. */
  def dequeue(): Option[A] = {
    val reply = new ReplyChan; dequeueChan!reply; reply?()
  }

  /** Shut down this queue. */
  def shutdown() = attempt{ shutdownChan!() }{ }
  // Note: it's possible that the server has already terminated, in which case
  // we catch the StopException.

  /** The server process. */
  private def server = thread("server"){
    // Currently held values.
    val queue = new Queue[A]
    // Queue holding reply channels for current dequeue attempts.
    val waiters = new Queue[ReplyChan]
    // Inv: queue.isEmpty or waiters.isEmpty.
    var done = false

    serve(!done)(
      enqueueChan =?=> { x => 
        if(waiters.nonEmpty){ // Pass x directly to a waiting dequeue.
          assert(queue.isEmpty); waiters.dequeue()!Some(x)
        }
        else queue.enqueue(x)
      }
      |  
      dequeueChan =?=> { reply =>
        if(queue.nonEmpty)
          reply!Some(queue.dequeue()) // Service request immediately.
        else{
          waiters.enqueue(reply)
          if(waiters.length == numWorkers) done = true
        }
      }
      |
      shutdownChan =?=> { _ => done = true }
    )

    // Terminated.
    for(c <- waiters) c!None
    enqueueChan.close(); dequeueChan.close(); shutdownChan.close()
  }

  fork(server)
}

// =======================================================

import scala.util.Random

/** A linearizability tester for the Terminating Partial Queue. */
object TerminatingPartialQueueTest{
  val MaxVal = 20 // Maximum value placed in the queue

  import scala.collection.immutable.Queue

  /** The sequential specification object.  A value Some(q) represents a queue
    * with contents q that has not yet reached termination.  A state None
    * represents that termination has been reached. */ 
  type SeqQueue = Option[Queue[Int]]

  /** The type of queues to test. */
  type ConcQueue = TerminatingPartialQueue[Int]

  /** Sequential enqueue.  This can happen only before the termination state. */
  def seqEnqueue(x: Int)(oq: SeqQueue) : (Unit, SeqQueue) = oq match{
    case Some(q) => ((), Some(q.enqueue(x)))
    case None => throw new IllegalArgumentException  // Isn't allowed.
  }

  /** Sequential dequeue.  If this happens from an empty queue, we enter the
    * termination state. */
  def seqDequeue(oq: SeqQueue) : (Option[Int], SeqQueue) = oq match{
    case Some(q) => 
      if(q.nonEmpty){ val(x,q1) = q.dequeue; (Some(x), Some(q1)) } 
      else (None, None)
    case None => (None, oq)
  }

  /** A worker for the LinTesters */
  def worker(me: Int, log: LinearizabilityLog[SeqQueue, ConcQueue]) = {
    val random = new Random(me+Random.nextInt()); var done = false
    while(!done){
      if(random.nextFloat() < 0.3){
        val x = random.nextInt(MaxVal)
        log(_.enqueue(x), s"enqueue($x)", seqEnqueue(x))
      }
      else
        log(q => {val res = q.dequeue(); done = (res == None); res},
          "dequeue", seqDequeue)
    }
  }

  def doTest = {
    val p = 4 // Number of workers.
    val concQueue = new ConcQueue(p); val seqQueue = Some(Queue[Int]())
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
      //case "--iters" => iters = args(i+1).toInt; i += 2 
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
