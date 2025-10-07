package tacp.monitors

import ox.scl._
import scala.util.Random

/** Test harness for the SyncChanManyMany and SyncChanSemaphores classes.
  * A command line flag of --sem indicates that the semaphore-based version 
  * should be used. */
object SharedSyncChanTest{
  // We will log using events of the following form.
  abstract class LogEvent
  case class BeginSend(me: Int, x: Int) extends LogEvent
  case class EndSend(me: Int) extends LogEvent
  case class BeginReceive(me: Int) extends LogEvent
  case class EndReceive(me: Int, x: Int) extends LogEvent

  // We arrange for all messages to use different data values, to simplify
  // checking of the log.

  /** Check the log es is valid. */
  def checkLog(es: Array[LogEvent]) = {
    /* We traverse the log, finding each BeginSend event, and then finding the
     * matching EndSend, BeginReceive and EndReceive events, and checking that
     * the operations overlap. */
    var i = 0 // Index of next event to consider.
    val n = es.length 

    // Print the log, highlighting event k, and exit.
    def printLog(k: Int) = {
      for(j <- 0 to n) println(s"$j:\t${es(j)}"+(if(j==k) "***" else ""))
      sys.exit()
    }

    while(i < n){
      // Search for the next BeginSend event.
      es(i) match{
        case BeginSend(sid, x) => 
          // Find matching EndSend(sid).
          var endS = i+1; while(es(endS) != EndSend(sid)) endS += 1
          // Find matching EndReceive(rid, x).
          var endR = i+1; var rid = -1 // Id of receiver, or -1 if not yet found.
          while(endR < n && rid < 0){
            es(endR) match{
              case EndReceive(rid0, `x`) => rid = rid0
              case _ => {}
            }
            endR += 1 
          }
          if(rid < 0){ println(s"Error: no receive of $x found."); printLog(i) }
          // Find matching BeginReceive(rid).
          var beginR = endR-1
          while(beginR >= 0 && es(beginR) != BeginReceive(rid)) beginR -= 1
          // Check the intervals [i,endS] and [beginR,endR] overlap.
          assert(beginR >= 0) // This shouldn't happen.
          if(endS < beginR){
            println(s"Error: matching sends and receives of $x don't overlap")
            printLog(i) 
          }

        case _ => {}
      } // End of match.
      i += 1
    } // End of while loop.
  }

  /** Do a single test. 
    * @param useSemaphores should the semaphore implementation be used? */
  def doTest(useSemaphores: Boolean) = {
    val p = 8 // Number of senders and receivers.
    val iters = 10 // Number of items for each sender to send.
    val chan: SyncChanT[Int] = // Channel to test.
      if(useSemaphores) ??? // new SyncChanSemaphores[Int] 
      else new SharedSyncChan[Int]
    val log = new Log[LogEvent](2*p) // Shared log; receivers use ids [p..2p).

    // The sender; all values sent are distinct. 
    def sender(me: Int) = thread{
      for(i <- 0 until iters){
        val x = me*iters+i; log.add(me, BeginSend(me, x))
        chan.send(x); log.add(me, EndSend(me))
      }
    }
    // The receiver.
    def receiver(me: Int) = thread{
      for(i <- 0 until iters){
        log.add(me+p, BeginReceive(me)); val x = chan.receive()
        log.add(me+p, EndReceive(me, x))
      }
    }

    val senders = || (for(i <- 0 until p) yield sender(i))
    val receivers = || (for(i <- 0 until p) yield receiver(i))
    run(senders || receivers)
    checkLog(log.get)
  }

  def main(args: Array[String]) = {
    val useSemaphores = args.nonEmpty && args(0) == "--sem"
    for(r <- 0 until 10000){ doTest(useSemaphores); if(r%100 == 0) print(".") }
    println()
  }
}
