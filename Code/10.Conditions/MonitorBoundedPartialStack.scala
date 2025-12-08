package tacp.datatypes

import ox.scl._
import scala.collection.mutable.Stack

/** A bounded partial stack implemented as a monitor. */
class MonitorBoundedPartialStack[T](bound: Int) extends PartialStack[T]{
  /** The stack itself. */
  private val stack = new Stack[T]

  /** A monitor object, to control the synchronisations. */
  private val lock = new Lock

  /** Condition for signalling that the stack is not full. */
  private val notFull = lock.newCondition

  /** Condition for signalling that the stack is not empty. */
  private val notEmpty = lock.newCondition

  /** Push x.  Blocks while the queue is full. */
  def push(x: T) = lock.mutex{
    notFull.await(stack.length < bound) 
    stack.push(x)
    notEmpty.signal()
  }

  /** Dequeue a value.  Blocks until the queue is non-empty. */
  def pop(): T = lock.mutex{
    notEmpty.await(stack.nonEmpty) // Wait for a signal.
    val result = stack.pop()
    notFull.signal()
    result
  }
}
