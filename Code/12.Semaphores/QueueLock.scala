package tacp.semaphores

import ox.scl._
import tacp.jvmMonitors.LockT

/** A class providing first-come, first-served mutual exclusion. */
class QueueLock extends LockT{
  /** A queue of semaphores for waiting threads. */
  private val queue = new scala.collection.mutable.Queue[Semaphore]

  /** Is the resource busy? */
  private var busy = false

  /** Semaphore for mutual exclusion on this object's variables. */
  private val mutex = new MutexSemaphore

  /** Acquire the lock. */
  def acquire() = {
    mutex.down()
    if(busy || !queue.isEmpty){ // Have to wait.
      val sem = new SignallingSemaphore
      queue.enqueue(sem)
      mutex.up(); sem.down() // Wait for turn.
      assert(!busy)  
    }
    busy = true
    mutex.up()
  }

  /** Release the lock. */
  def release() = {
    mutex.down()
    busy = false
    if(queue.nonEmpty){ // Wake up next thread.
      val first = queue.dequeue(); first.up()
    }
    else mutex.up()
  } 
}

// -------------------------------------------------------

// Note: there is a linearizability tester for locks in the Tests directory.

object SimpleQueueLockTest{
  /* The following should really be specifiable via the command line... */
  var p = 5 // number of clients
  var iters = 1000 // # iterations by each client
  val reps = 1000 // # times to repeat

  // Events put into the log
  abstract class LogEvent
  case class Enter(c: Int) extends LogEvent
  case class Leave(c: Int) extends LogEvent

  /** A client */
  def client(me: Int, controller: QueueLock, log: Log[LogEvent]) = thread{
    for(_ <- 0 until iters){
      controller.acquire()
      log.add(me, Enter(me))
      log.add(me, Leave(me))
      controller.release()
    }
  }

  /** Check that events represents a valid log: at most one thread is in the CR
    * at a time.
    * @return true if the log is valid.  */
  def checkLog(events: Array[LogEvent]): Boolean = {
    var error = false; var i = 0
    def flagError = {
      println("Error found:")
      println(events.take(i+1).mkString("\n"))
      error = true
    }
    while(i < events.size && !error) events(i) match{
      case Enter(c) => events(i+1) match{
        case Leave(`c`) => i += 2
        case _ => flagError
      }
      case Leave(c) => flagError
    }
    !error
  }

  /** Run a single test. */
  def runTest = {
    val log = new Log[LogEvent](p)
    val controller = new QueueLock
    val clients = || (for (i <- 0 until p) yield client(i, controller, log))
    run(clients)
    if(!checkLog(log.get)) sys.exit()
  }

  def main(args: Array[String]) = {
    for(r <- 0 until reps){
      runTest
      if(r%5 == 0) print(".")
    }
    println()
  }

}
