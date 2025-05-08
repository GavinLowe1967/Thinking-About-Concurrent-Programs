package tacp.race

import ox.scl._

/** Program to show the dangers of undisciplined shared variables. */
object Race{
  var x = 0

  /** Thread to increment x 1000 times. */
  def t1 = thread{ for(i <- 0 until 1000) x = x+1 }

  /** Thread to decrement x 1000 times. */
  def t2 = thread{ for(i <- 0 until 1000) x = x-1 }

  /** Parallel composition. */
  def system = t1 || t2

  def main(args: Array[String]) = { run(system); println(x) }
}
