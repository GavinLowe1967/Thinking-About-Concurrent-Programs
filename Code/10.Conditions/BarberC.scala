package tacp.monitors

import ox.scl._

/** Trait for a synchronisation object for the sleeping barber problem. */
trait BarberT{
  /** Customer arrives, waits for barber to be ready.  Synchronises with
    * getNextCustomer. */
  def getHaircut(): Unit

  /** Barber wakes up next customer.  Synchronises with getHaircut. */
  def getNextCustomer(): Unit

  /** Customer waits for barber to finish haircut.  Synchronises with
    * finishedCut. */
  def waitForHaircut(): Unit

  /** Barber finishes haircut.  Synchronises with waitForHaircut.  */
  def finishedCut(): Unit
}

// =======================================================

/** Synchronisation object for the sleeping barber problem, using SCL monitors
  * and conditions. */
object BarberC extends BarberT{
  private var barberAvailable = false
  private var barberDone = false

  private val lock = new Lock

  private val barberAvailableC, chairOccupiedC, barberDoneC, customerLeftC =
    lock.newCondition

  /** Customer arrives, waits for barber to be ready.  
    * Multiple customers can be in this method. */
  def getHaircut() = lock.mutex{
    barberAvailableC.await(barberAvailable) // Wait for barber.
    barberAvailable = false // Clear for next round.
    chairOccupiedC.signal() // Signal to barber.
  }

  /** Barber wakes up next customer. */
  def getNextCustomer() = lock.mutex{
    barberAvailable = true
    barberAvailableC.signal() // Wake up a sleeping customer.
    chairOccupiedC.await() // Wait for signal.
  }

  /** Customer waits for barber to finish haircut.
    * At most one customer can be in this method. */
  def waitForHaircut() = lock.mutex{
    if(!barberDone) barberDoneC.await()  // Wait for barber to finish.
    barberDone = false  // Clear for next round.
    customerLeftC.signal() // Signal to barber.
  }

  /** Barber finishes haircut. */
  def finishedCut() = lock.mutex{
    barberDone = true; barberDoneC.signal() // wake up the customer
    customerLeftC.await() // wait for customer to leave
  }
}

// =======================================================

/** Objects to synchronise between a thread of type A and a single thread of
  * type B.  There may be many threads of type A, but a single thread of type
  * B. */
class Sync{
  /** Is the B thread ready to synchronise? */
  private var bReady = false

  private val lock = new Lock

  /** Condition to signal that an A thread is ready. */
  private val aReadyC = lock.newCondition

  /** Condition to signal that the B thread is ready. */
  private val bReadyC = lock.newCondition

  /** Operation for a thread of type A to synchronise. */
  def syncA() = lock.mutex{
    bReadyC.await(bReady); bReady = false // Wait for B (1), and consume signal.
    aReadyC.signal() // Signal to B at (2).
  }

  /** Operation for a thread of type B to synchronise. */
  def syncB() = lock.mutex{
    bReady = true; bReadyC.signal() // Signal to A at (1).
    aReadyC.await() // Wait for A (2).
  }
}

// =======================================================

/** A test for Sync. */
object SyncTest{
  /** Events written in the log. */
  trait LogEvent
  case object BeginA extends LogEvent; case object EndA extends LogEvent
  case object BeginB extends LogEvent; case object EndB extends LogEvent

  /** Check that events represents a correct log. */
  def checkLog(events: Array[LogEvent]) = {
    // We check that, at each point, the number of EndA events does not exceed
    // the number of BeginB events, and the number of EndB events does not
    // exceed the number of BeginA events.
    var numBeginA = 0; var numBeginB = 0; var numEndA = 0; var numEndB = 0
    for(i <- 0 until events.length) events(i) match{
      case BeginA => numBeginA += 1; case BeginB => numBeginB += 1
      case EndA => numEndA += 1; assert(numEndA <= numBeginB)
      case EndB => numEndB += 1; assert(numEndB <= numBeginA) 
    }
  }

  /** Do a single test. */
  def doTest = {
    val iters = 10; val numA = 5 // Iterations per A thread; number of A threads.
    val sync = new Sync; val log = new Log[LogEvent](numA+1)
    def aThread(me: Int) = thread(s"A($me)"){
      for(i <- 0 until iters){ 
        log.add(me, BeginA); sync.syncA(); log.add(me, EndA) 
      }
    }
    def bThread = thread(s"B"){
      val myId = numA // Identity to use in the log.
      for(i <- 0 until numA*iters){ 
        log.add(myId, BeginB); sync.syncB(); log.add(myId, EndB) 
      }
    }
    run((|| (for (i <- 0 until numA) yield aThread(i))) || bThread)
    checkLog(log.get)
  }

  def main(args: Array[String]) = {
    for(i <- 0 until 10000){ doTest; if(i%100 == 0) print(".") }
    println()
  }
}

// =======================================================

/** An implementation of BarberT using two Sync objects. */
object Barber2 extends BarberT{
  /** Synchronisation object for the getHaircut/getNextCustomer
    * synchronisation. */
  private val sync1 = new Sync
  /** Synchronisation object for the waitForHaircut/finishedCut
    * synchronisation. */
  private val sync2 = new Sync

  def getHaircut() = sync1.syncA()
  def getNextCustomer() = sync1.syncB()
  def waitForHaircut() = sync2.syncA()
  def finishedCut() = sync2.syncB()
}

// =======================================================

import scala.util.Random
import Thread.sleep

/** A simulation of the sleeping barber. */
object Barber{
  private val barberObj: BarberT = Barber2

  def barber = thread("Barber"){
    while(true){
      sleep(Random.nextInt(500))
      println("Barber ready")
      barberObj.getNextCustomer()
      println("Barber cutting hair")
      sleep(Random.nextInt(50+1000))
      println("Barber finished")
      barberObj.finishedCut()
    }
  }

  def customer(me: Int) = thread("Customer"+me){
    while(true){
      sleep(Random.nextInt(6000))
      println("Customer "+me+" arrived")
      barberObj.getHaircut()
      println("Customer "+me+" getting haircut")
      barberObj.waitForHaircut()
      println("Customer "+me+" finished haircut")
    }
  }

  val numCustomers = 5

  def system = {
    val customers = || (for(i <- 0 until numCustomers) yield customer(i))
    barber || customers
  }

  def main(args:Array[String]) = run(system)
}
