package tacp.clientServer

import ox.scl._

import scala.util.Random

/** A tester for an AtomicBroadcast object. */
object AtomicBroadcastTest{
  /** The type of objects to be tested. */
  type AB = AtomicBroadcastT[Int]

  trait LogEvent
  case class BeginSend(x: Int) extends LogEvent
  case object EndSend extends LogEvent
  case object BeginReceive extends LogEvent
  case class EndReceive(x: Int) extends LogEvent

  type Log1 = Log[LogEvent] 

  var iters = 5

  val Max = 100

  def sender(me: Int, ab: AB, log: Log1) = thread("sender"){
    for(i <- 0 until iters){
      val x = Random.nextInt(Max); log.add(me, BeginSend(x))
      ab.send(x); log.add(me, EndSend)
    }
  }

  def receiver(me: Int, ab: AB, log: Log1) = thread(s"receiver($me)"){
    for(i <- 0 until iters){
      log.add(me, BeginReceive); val x = ab.receive(); log.add(me, EndReceive(x))
    }
  }

  def checkLog(n: Int, events: Array[LogEvent]) = {
    def mkError(i: Int) = 
      s"Error at index $i: ${events(i)}\n"+events.mkString("\n")

    // Traverse the log.  currentSend = Some(x) if there is a current send of
    // x that has not yet synchronised; otherwise it equals None.
    // receiversCalled is the number of receivers that have called the
    // operation but not yet synchronised.  receiversToReturn is the number of
    // receivers that have synchronised but not yet returned.  currentVal is
    // the value that those receivers should return.
    var currentSend: Option[Int] = None; var currentVal = -1
    var receiversCalled = 0; var receiversToReturn = 0
    for(i <- 0 until events.length){
      events(i) match{
        case BeginSend(x) => 
          assert(currentSend == None, mkError(i)); currentSend = Some(x)
        case EndSend => 
          assert(currentSend == None, mkError(i))
        case BeginReceive => 
          receiversCalled += 1
        case EndReceive(x) => 
          assert(receiversToReturn > 0, mkError(i))
          assert(x == currentVal, mkError(i)); receiversToReturn -= 1
      }
      if(currentSend != None && receiversCalled == n){
        // The threads can now synchronise.
        currentVal = currentSend.get; currentSend = None
        receiversCalled = 0; receiversToReturn = n
      }
    }
  }

  private val Server = 0; private val Monitor = 1

  def doTest(choice: Int) = {
    val n = // Number of receivers.
      (if(choice == Monitor) 1 else 0)+Random.nextInt(10) 
    val ab: AB = 
      if(choice == Server) new AtomicBroadcast[Int](n) 
      else new tacp.monitors.AtomicBroadcast[Int](n)
    val log = new Log1(n+1)
    val receivers = || (for(i <- 0 until n) yield receiver(i, ab, log))
    run(sender(n, ab, log) || receivers)
    checkLog(n, log.get)
    ab.shutdown()
  }

  def main(args: Array[String]) = {
    var choice = Server; var i = 0
    while(i < args.length) args(i) match{
      case "--monitor" => choice = Monitor; i += 1
    }

    for(i <- 0 until 5000){ doTest(choice); if(i%50 == 0) print(".") }
    println()
  }

}
