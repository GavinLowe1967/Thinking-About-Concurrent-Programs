package tacp.jvmMonitors

import ox.scl._

/** A counter, implemented using JVM synchronized blocks. */
object Counter{
  private var x = 0
  def inc() = synchronized{ x += 1 }
  def dec() = synchronized{ x -= 1 }
  def get = synchronized{ x }
}

/** A test for Counter. */
object CounterTest{
  def p = thread{ for(i <- 0 until 1000) Counter.inc() }
  def q = thread{ for(i <- 0 until 1000) Counter.dec() }
  def system = p || q

  def main(args : Array[String]) = { run(system); println(Counter.get) }
}
