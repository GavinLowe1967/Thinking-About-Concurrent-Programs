package tacp.datatypes

import ox.scl._
import scala.collection.mutable.{Stack,Queue}

/** A partial queue that terminates if all worker threads are attempting to
  * dequeue, and the queue is empty.
  * @param numWorkers the number of worker threads. */
class TerminatingPartialStack[A](numWorkers: Int){
  /** Channel for pushing. */
  private val pushChan = new SyncChan[A]

  private type ReplyChan = OnePlaceBuffChan[Option[A]]

  /** Channel for popping. */
  private val popChan = new SyncChan[ReplyChan]

  /** Channel for shutting down the stack. */
  //private val shutdownChan = new SyncChan[Unit]

  /** Push x.
    * @throws Stopped if the stack has been shutdown. */
  def push(x: A): Unit = pushChan!x

  /** Attempt to pop a value.  Returns None if the termination condition has
    * been reached.
    * @throws Stopped if the stack has been shutdown. */
  def pop(): Option[A] = { 
    val reply = new ReplyChan; popChan!reply; reply?() 
  }

  /** Shut down this stack. */
  def shutdown() = { pushChan.close(); popChan.close() } 
    // attempt{ shutdownChan!(()) }{ }
  // Note: it's possible that the server has already terminated, in which case
  // we catch the Stopped exception.

  /** The server process. */
  private def server = thread("server"){
    // Currently held values
    val stack = new Stack[A]()
    // Queue holding reply channels for current pop attempts.
    val waiters = new Queue[ReplyChan]()
    var done = false
    // Main loop.  Invariant: stack.isEmpty or waiters.isEmpty.
    serve(!done)(
      pushChan =?=> { x => 
        if(waiters.nonEmpty){ // pass x directly to a waiting pop
          assert(stack.isEmpty); waiters.dequeue()!Some(x)
        }
        else stack.push(x)
      }
      |  
      popChan =?=> { reply =>
        if(stack.nonEmpty) reply!Some(stack.pop()) // service request immediately
        else{
          waiters.enqueue(reply)
          if(waiters.length == numWorkers) done = true
        }
      }
      // |
      // shutdownChan =?=> { _ => done = true }
    )
    // Termination.
    for(c <- waiters) c!None
    pushChan.close(); popChan.close() // ; shutdownChan.close()
  }

  fork(server)
}
