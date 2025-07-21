package tacp.clientServer

import ox.scl._

import scala.collection.mutable.Queue

trait FilterChanT[A]{
  def send(x: A): Unit

  def receive(p: A => Boolean): A

  def shutdown(): Unit
}

// =======================================================

class FilterChan[A] extends FilterChanT[A]{
  /** Channel the sender uses to send to the server. */
  private val fromSender = new SyncChan[(A, Chan[Unit])]

  /** Synchronously send x. */
  def send(x: A) = { 
    val replyChan = new OnePlaceBuffChan[Unit]; fromSender!(x, replyChan); replyChan?()
  }

  /** Channel receivers use to send to the server. */
  private val fromReceiver = new SyncChan[(A => Boolean, Chan[A])]
  
  /** Synchronously receive a value that satisfied p. */
  def receive(p: A => Boolean): A = {
    val replyChan = new OnePlaceBuffChan[A]; fromReceiver!(p, replyChan); replyChan?()
  }

  private val shutdownChan = new SyncChan[Unit]

  /** Shut down the server. */
  def shutdown() = shutdownChan!() 

  /** The server process. */
  private def server = thread("ServerFilterChan"){
    // Queue of pending requests from senders.
    val pendingSends = new Queue[(A, Chan[Unit])]
    // Queue of pending requests from receivers.
    val pendingRecs = new Queue[(A => Boolean, Chan[A])]
    var isShutdown = false
    serve(!isShutdown)(
      fromSender =?=> { case (x,c) => 
        // Traverse pendingRecs to see if a request matches.
        var i = 0; var done = false; val len = pendingRecs.length
        while(i < len && !done){
          val (p, cr) = pendingRecs.dequeue()
          if (p(x)){ cr!x; c!(); done = true }
          else{ pendingRecs.enqueue((p, cr)); i += 1 }
        }
        if(!done) pendingSends.enqueue((x,c))
      }
      | fromReceiver =?=> { case (p,c) =>
        // Traverse pendingSends to see if a value matches.
        var i = 0; var done = false; val len = pendingSends.length
        while(i < len && !done){
          val (x, cs) = pendingSends.dequeue()
          if(p(x)){ c!x; cs!(); done = true }
          else{ pendingSends.enqueue((x, cs)); i += 1 }
        }
        if(!done) pendingRecs.enqueue((p,c))
      }
      | shutdownChan =?=> { _ => isShutdown = true }
    )
    fromSender.close(); fromReceiver.close()
    for((_,c) <- pendingSends) c.endOfStream()
    for((_,c) <- pendingRecs) c.endOfStream()
  }

  fork(server)
}

// =======================================================

class FaultyFilterChan[A] extends FilterChanT[A]{
  /** Channel the sender uses to send to the server. */
  private val fromSender = new SyncChan[A]

  /** Synchronously send x. */
  def send(x: A) = fromSender!x

  /** Channel receivers use to send to the server. */
  private val fromReceiver = new SyncChan[(A => Boolean, Chan[A])]
  
  /** Synchronously receive a value that satisfied p. */
  def receive(p: A => Boolean): A = {
    val replyChan = new SyncChan[A]; fromReceiver!(p, replyChan); replyChan?()
  }

  private val shutdownChan = new SyncChan[Unit]

  /** Shut down the server. */
  def shutdown() = shutdownChan!() 

  /** The server process. */
  private def server = thread("ServerFilterChan"){
    // Queue of pending requests from senders.
    val pendingSends = new Queue[A]
    // Queue of pending requests from receivers.
    val pendingRecs = new Queue[(A => Boolean, Chan[A])]
    var isShutdown = false
    serve(!isShutdown)(
      fromSender =?=> { x => 
        // Traverse pendingRecs to see if a request matches.
        var i = 0; var done = false; val len = pendingRecs.length
        while(i < len && !done){
          val (p, cr) = pendingRecs.dequeue()
          if (p(x)){ cr!x; done = true }
          else{ pendingRecs.enqueue((p, cr)); i += 1 }
        }
        if(!done) pendingSends.enqueue(x)
      }
      | fromReceiver =?=> { case (p,c) =>
        // Traverse pendingSends to see if a value matches.
        var i = 0; var done = false; val len = pendingSends.length
        while(i < len && !done){
          val x = pendingSends.dequeue()
          if(p(x)){ c!x; done = true }
          else{ pendingSends.enqueue(x); i += 1 }
        }
        if(!done) pendingRecs.enqueue((p,c))
      }
      | shutdownChan =?=> { _ => isShutdown = true }
    )
    fromSender.close(); fromReceiver.close()
    // for((_,c) <- pendingSends) c.endOfStream()
    for((_,c) <- pendingRecs) c.endOfStream()
  }

  fork(server)
}

// =======================================================

object FilterChanTest{
  /** We will run n senders and n receivers. */
  var n = 4

  var iters = 10 // Number of iterations per worker.

  trait LogEvent
  case class BeginSend(id: Int, x: Int) extends LogEvent
  case class EndSend(id: Int) extends LogEvent
  case class BeginReceive(id: Int) extends LogEvent
  case class EndReceive(id: Int, x: Int) extends LogEvent
  type Log1 = Log[LogEvent]

  def sender(me: Int, chan: FilterChanT[Int], log: Log1) 
  = thread(s"sender($me)"){
    // This thread sends [me*iters .. (me+1)*iters).
    for(x <- me*iters until (me+1)*iters){
      log.add(me, BeginSend(me, x)); chan.send(x); log.add(me, EndSend(me))
    }
  }

  def receiver(me: Int, chan: FilterChanT[Int], log: Log1) 
  = thread(s"receiver($me)"){
    def p(x: Int) = x%n == me
    val logId = me+n // Identity for logging purposes.
    for(x <- 0 until iters){
      log.add(logId, BeginReceive(me)); val x = chan.receive(p)
      log.add(logId, EndReceive(me, x))
    }
  }

  def checkLog(events: Array[LogEvent]): Boolean = {
    def giveError(i: Int) = {
      println(s"\nUnmatched End event at index $i: "+events(i))
      println(events.mkString("\n"))
    }
    /* Traverse log, keeping track of which senders and receivers are current, and
     * which have been matched; and for those that are current but unmatched,
     * the values they are trying to send or will subsequently receive.  If we
     * see an unmatched EndReceive, check it corresponds to a current
     * unmatched sender, and mark the latter as matched.  If we see an
     * unmatched EndSend, do likewise. */
    val Out = 0; val Unmatched = 1; val Matched = 2
    val senderState = Array.fill(n)(Out); val receiverState = Array.fill(n)(Out)
    val senderValue = new Array[Int](n); val receiverValue = new Array[Int](n)
    for(i <- 0 until events.length) events(i) match{
      case BeginSend(s, x) => 
        assert(senderState(s) == Out)
        senderState(s) = Unmatched; senderValue(s) = x

      case BeginReceive(r) =>
        assert(receiverState(r) == Out); receiverState(r) = Unmatched
        // Find corresponding EndReceive(r,_)
        var j = i+1; var done = false
        while(!done) events(j) match{
          case EndReceive(r1, x) if r == r1 => receiverValue(r) = x; done = true
          case _ => j += 1
        }

      case EndSend(s) =>
        if(senderState(s) == Matched) senderState(s) = Out
        else{
          assert(senderState(s) == Unmatched); var r = 0; val x = senderValue(s)
          while(r < n && senderState(s) == Unmatched){
            if(receiverState(r) == Unmatched && 
                receiverValue(r) == x){ // s and r match. 
              senderState(s) = Out; receiverState(r) = Matched
            }
            else r += 1
          } // End of while.
          if(senderState(s) == Unmatched){ // Found an error.
            giveError(i); return false
          }
        }

      case EndReceive(r, x) =>
        if(receiverState(r) == Matched) receiverState(r) = Out
        else{
          assert(receiverState(r) == Unmatched); var s = 0
          // Search for matching sender.
          while(s < n && receiverState(r) == Unmatched){
            if(senderState(s) == Unmatched && senderValue(s) == x){
              // s and r match
              senderState(s) = Matched; receiverState(r) = Out
            }
            else s += 1
          } // End of while.
          if(receiverState(r) == Unmatched){ // Found an error.
            giveError(i); return false
          }
        }
    } // End of outer for/match.
    true
  }

  /** Are we using the faulty version. */
  var faulty = false

  def doTest = {
    val chan: FilterChanT[Int] = 
      if(faulty) new FaultyFilterChan[Int] else new FilterChan[Int]
    val log = new Log1(2*n)
    val senders = || (for(i <- 0 until n) yield(sender(i, chan, log)))
    val receivers = || (for(i <- 0 until n) yield (receiver(i, chan, log)))
    run(senders || receivers)
    assert(checkLog(log.get))
    chan.shutdown()
  }

  def main(args: Array[String]) = {
    // Parse arguments
    var i = 0; var reps = 5000
    while(i < args.length) args(i) match{
      case "-n" => n = args(i+1).toInt; i += 2
      case "--iters" => iters = args(i+1).toInt; i += 2
      case "--reps" => reps = args(i+1).toInt; i += 2
      case "--faulty" => faulty = true; i += 1
      case arg => println(s"Illegal argument: $arg"); sys.exit() 
    }

    for(i <- 0 until reps){ doTest; if(i%50 == 0) print(".") }
    println()
  }


}
