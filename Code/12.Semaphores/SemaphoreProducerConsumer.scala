package tacp.semaphores

import ox.scl._

class SemaphoreProducerConsumer(n: Int) extends tacp.monitors.ProducerConsumer{
  require(n >= 1)

  /** Array to hold the data. */
  private var a = new Array[Int](n)

  /** Number of items added to a so far. */
  private var count = 0

  /** Semaphore to signal to a producer that the buffer is not full. */
  private val nonFull = new MutexSemaphore

  /** Semaphore to signal to a consumer that the buffer is full. */
  private val full = new SignallingSemaphore

  /** Add x to the buffer, waiting until it is not full. */
  def put(x: Int) = {
    nonFull.down()
    assert(count < n)
    a(count) = x; count += 1
    if(count == n) full.up() else nonFull.up() // Pass the baton.
  }

  /** Get the contents of the buffer, once it is full. */
  def get: Array[Int] = {
    full.down()
    assert(count == n)
    val result = a
    a = new Array[Int](n); count = 0
    nonFull.up() // Pass the baton to a producer.
    result
  }
}
