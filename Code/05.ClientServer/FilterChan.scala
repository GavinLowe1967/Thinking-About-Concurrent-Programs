package tacp.clientServer

import ox.scl._

import scala.collection.mutable.Queue

class FilterChan[A]{
  /** Channel the sender uses to send to the server. */
  private val fromSender = new SyncChan[(A, Chan[Unit])]

  /** Synchronously send x. */
  def send(x: A) = { 
    val replyChan = new SyncChan[Unit]; fromSender!(x, replyChan); replyChan?()
  }

  /** Channel receivers use to send to the server. */
  private val fromReceiver = new SyncChan[(A => Boolean, Chan[A])]
  
  /** Synchronously receive a value that satisfied p. */
  def receive(p: A => Boolean): A = {
    val replyChan = new SyncChan[A]; fromReceiver!(p, replyChan); replyChan?()
  }

  private val shutdownChan = new SyncChan[Unit]

  /** Shut down the server. */
  def shutdown() = shutdownChan!() 

  /** Find an element of q that satisfies prop. */
  private def queueFind[A](q: Queue[A], prop: A => Boolean): Option[A] = {
    var i = 0; var result: Option[A] = None; val len = q.length
    while(i < len && result == None){
      val x = q.dequeue()
      if (prop(x)) result = Some(x) else{ q.enqueue(x); i += 1 }
    }
    result
  }

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
        // def matches(pair: (A => Boolean, Chan[A])) = pair._1(x)
        // queueFind(pendingRecs, matches _) match{
        //   case Some((p,cr)) => cr!x; c!()
        //   case None => pendingSends.enqueue((x,c))
        // } 
        var i = 0; var done = false; val len = pendingRecs.length
        while(i < len && !done){
          val (p, cr) = pendingRecs.dequeue()
          if (p(x)){ cr!x; c!(); done = true }
          else{ pendingRecs.enqueue((p, cr)); i += 1 }
        }
        if(!done) pendingSends.enqueue((x,c))
      }
      | fromReceiver =?=> { case (p,c) =>
        // def matches(pair: (A, Chan[A])) = p(pair._1)
        // queueFind(pendingSends, matches _) match{
        //   case Some((x,cs)) => c!x; cs!()
        //   case None => pendingRecs.enqueue((p,c))
        // }
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

object FilterChanTest{
  /** We will run n senders and n receivers. */
  val n = 4

  val iters = 10 // Number of iterations per worker.

  def sender(me: Int, chan: FilterChan[Int]) = thread(s"sender($me)"){
    // This thread will send me, me+n, me+2*n,..., me+(iters-1)*n.
    for(i <- 0 until iters) chan.send(me+i*n)
  }

  def receiver(me: Int, chan: FilterChan[Int]) = thread(s"receiver($me)"){
    def p(x: Int) = x%n == me
    // This thread expects to receive me, me+n, me+2*n,..., me+(iters-1)*n.
    for(i <- 0 until iters){
      val x = chan.receive(p); assert(p(x)); assert(x == me+i*n)
    }
  }

  def doTest = {
    val chan = new FilterChan[Int]
    val senders = || (for(i <- 0 until n) yield(sender(i, chan)))
    val receivers = || (for(i <- 0 until n) yield (receiver(i, chan)))
    run(senders || receivers)
    chan.shutdown()
  }

  def main(args: Array[String]) = {
    for(i <- 0 until 1000){ doTest; if(i%50 == 0) print(".") }
    println()
  }


}
