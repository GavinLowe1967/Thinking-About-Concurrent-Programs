package tacp.monitors

import ox.scl._

/** The sleeping barber problem, using SCL monitors and conditions. */
object Barber{
  private var barberAvailable = false
  private var barberDone = false

  private val lock = new Lock

  private val barberAvailableC, chairOccupiedC, barberDoneC, customerLeftC =
    lock.newCondition

  /** Customer arrives, waits for barber to be ready.  
    * Multiple customers can be in this method. */
  def getHaircut = lock.mutex{
    barberAvailableC.await(barberAvailable) // Wait for barber.
    barberAvailable = false // Clear for next round.
    chairOccupiedC.signal() // Signal to barber.
  }

  /** Barber wakes up next customer. */
  def getNextCustomer = lock.mutex{
    barberAvailable = true
    barberAvailableC.signal() // Wake up a sleeping customer.
    chairOccupiedC.await() // Wait for signal.
  }

  /** Customer waits for barber to finish haircut.
    * At most one customer can be in this method. */
  def waitForHaircut = lock.mutex{
    if(!barberDone) barberDoneC.await()  // Wait for barber to finish.
    barberDone = false  // Clear for next round.
    customerLeftC.signal() // Signal to barber.
  }

  /** Barber finishes haircut. */
  def finishedCut = lock.mutex{
    barberDone = true; barberDoneC.signal() // wake up the customer
    customerLeftC.await() // wait for customer to leave
  }
}

// -------------------------------------------------------

import scala.util.Random
import Thread.sleep

object BarberC{
  def barber = thread("Barber"){
    while(true){
      sleep(Random.nextInt(500))
      println("Barber ready")
      Barber.getNextCustomer
      println("Barber cutting hair")
      sleep(Random.nextInt(50+1000))
      println("Barber finished")
      Barber.finishedCut
    }
  }

  def customer(me: Int) = thread("Customer"+me){
    while(true){
      sleep(Random.nextInt(6000))
      println("Customer "+me+" arrived")
      Barber.getHaircut
      println("Customer "+me+" getting haircut")
      Barber.waitForHaircut
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
