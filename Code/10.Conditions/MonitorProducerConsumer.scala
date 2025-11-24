package tacp.monitors

import ox.scl._

trait ProducerConsumer{
  /** Add x to the buffer, waiting until it is not full. */
  def put(x: Int): Unit

  /** Get the contents of the buffer, once it is full. */
  def get: Array[Int]
}

/** An implementation using a monitor. */
class MonitorProducerConsumer(n: Int) extends ProducerConsumer{
  require(n >= 1)

  /** Array to hold the data. */
  private var a = new Array[Int](n)

  /** Number of items added to a so far. */
  private var count = 0

  /** Monitor to control synchronisation. */
  private val lock = new Lock

  /** Condition for signalling that the old buffer has been taken. */
  private val notFull = lock.newCondition

  /** Condition for signalling that the buffer has been filled. */
  private val full = lock.newCondition

  /** Add x to the buffer, waiting until it is not full. */
  def put(x: Int) = lock.mutex{
    notFull.await(count < n)
    a(count) = x; count += 1
    if(count == n) full.signal()
  }

  /** Get the contents of the buffer, once it is full. */
  def get: Array[Int] = lock.mutex{
    if(count < n) full.await()
    val result = a
    a = new Array[Int](n); count = 0
    notFull.signalAll()
    result    
  }
}
