package tacp.locks

import ox.scl._

/** A simple counter, protected using a lock. */
class Counter{
  /** The value of the counter. */
  private var c = 0

  /** Lock used to protect c. */
  private val lock = new Lock

  /** Increment the counter. */
  def inc() = lock.mutex{ c += 1 }

  /** Decrement the counter. */
  def dec() = lock.mutex{ c -= 1 }

  /** Get the value of the counter. */
  def get: Int = lock.mutex{ c}
}

/** A test for Counter. */
object CounterTest{
  val iters = 1000

  /** Perform a single test. */
  def doTest = {
    val c = new Counter
    def p = thread("p"){ for(i <- 0 until iters) c.inc() }
    def q = thread("q"){ for(i <- 0 until iters) c.dec() }
    run(p || q)
    assert(c.get == 0)
  }

  def main(args: Array[String]) = {
    for(r <- 0 until 10000){ doTest; if(r%500 == 0) print(".") }
    println()
  }
}
