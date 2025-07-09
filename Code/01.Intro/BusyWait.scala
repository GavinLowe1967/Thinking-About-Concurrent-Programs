package tacp.race

import ox.scl._

/** Program to show that naive signalling doesn't work. */
object BusyWait{
  var answer = 0

  var done = false

  /** Thread to set answer to 42, and signal to t2. */
  def t1 = thread("t1"){ answer = 42; done = true }

  /** Thread to wait for a signal, and read answer. */
  def t2 = thread("t2"){
    while(!done){ } // Busy wait.  Don't do this!!!
    val myAnswer = answer; assert(myAnswer == 42)
  }

  /** Parallel composition. */
  def system = t1 || t2

  def main(args: Array[String]) = { 
    for(i <- 0 until 10000){ answer = 0; done = false; run(system); print(".") }
  }
}
