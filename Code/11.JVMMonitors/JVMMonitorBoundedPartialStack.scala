package tacp.datatypes

import ox.scl._

import scala.collection.mutable.Stack

/** A bounded partial stack implemented using a JVM monitor. */
class JVMMonitorBoundedPartialStack[T](bound: Int) extends PartialStack[T]{
  /** The current state of the stack. */
  private val stack = new Stack[T]

  /** Push x. */
  def push(x: T) = synchronized{ 
    while(stack.length == bound) wait() // Wait for a space (1).
    stack.push(x)
    notifyAll() // Signal to a waiting pop at (2).
  }

  /** Try to pop, blocking while the stack is empty.  */
  def pop(): T = synchronized{ 
    while(stack.isEmpty) wait() // Wait for a value (2).
    val result = stack.pop()
    notifyAll() // Signal to a waiting push at (1).
    result
  }
}
