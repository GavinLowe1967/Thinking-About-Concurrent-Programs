package tacp.locks

import ox.scl._

trait CounterT{
  /** Increment the counter. */
  def inc(): Unit

  /** Decrement the counter. */
  def dec(): Unit

  /** Get the value of the counter. */
  def get: Int
}

// =======================================================


/** A simple counter, protected using a lock. */
class Counter0 extends CounterT{
  /** The value of the counter. */
  private var c = 0

  /** Lock used to protect c. */
  private val lock = new Lock

  /** Increment the counter. */
  def inc() = { lock.acquire(); c += 1; lock.release() }

  /** Decrement the counter. */
  def dec() = { lock.acquire(); c -= 1; lock.release() }

  /** Get the value of the counter. */
  def get: Int = { lock.acquire(); val result = c; lock.release(); result }
}

// =======================================================

/** A simple counter, protected using a lock. */
class Counter extends CounterT{
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

// =======================================================

/** A test for Counter. */
object CounterTest{
  val iters = 1000

  var version = 1

  /** Perform a single test. */
  def doTest = {
    val c: CounterT = if(version == 0) new Counter0 else new Counter
    def p = thread("p"){ for(i <- 0 until iters) c.inc() }
    def q = thread("q"){ for(i <- 0 until iters) c.dec() }
    run(p || q)
    assert(c.get == 0)
  }

  def main(args: Array[String]) = {
    if(args.nonEmpty && args(0) == "-0") version = 0

    for(r <- 0 until 10000){ doTest; if(r%500 == 0) print(".") }
    println()
  }
}
