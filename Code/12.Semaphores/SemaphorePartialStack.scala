package tacp.datatypes

import ox.scl._
import scala.collection.mutable.Stack

class SemaphorePartialStack[T] extends PartialStack[T]{
  /** The stack itself. */
  private val stack = new Stack[T]

  /** Semaphore for mutual exclusion. */
  private val mutex = new MutexSemaphore

  /** Semaphore for signalling to waiting pop operations. */
  private val nonEmpty = new SignallingSemaphore

  /** The number of pop operations waiting. */
  private var waitingPops = 0

  def push(x: T) = {
    mutex.down()
    stack.push(x)
    if(waitingPops > 0) nonEmpty.up() // Pass baton to waiting pop.
    else mutex.up()
  }

  def pop(): T = {
    mutex.down()
    if(stack.isEmpty){ 
      waitingPops += 1; mutex.up(); nonEmpty.down()  // Wait for signal.
        waitingPops -= 1
    }
    assert(stack.nonEmpty); val result = stack.pop()
    mutex.up(); result
  }
}
