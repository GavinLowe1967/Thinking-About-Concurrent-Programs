package tacp.datatypes

import ox.scl._

/** A total queue. */
trait TotalStack[T]{
  /** Push x. */
  def push(x: T): Unit

  /** Pop a value.  Returns None if the queue is empty. */
  def pop(): Option[T]

  /** Shut down the queue. */
  def shutdown(): Unit
}

// =======================================================

class ServerStack[T] extends TotalStack[T]{
  /** Channel for pushing. */
  private val pushC = new SyncChan[T]

  /** Channel for popping. */
  private val popC = new SyncChan[Option[T]]

  /** Push x onto the stack. */
  def push(x: T) = pushC!x

  /** Optionally pop a value from the stack.
    * @return Some(x) where x is the value popped, or None if the stack is empty. */
  def pop(): Option[T] = popC?()

  private def server = thread{
    val stack = new scala.collection.mutable.Stack[T]
    serve(
      pushC =?=> { x => stack.push(x) }
      | popC =!=> { if(stack.isEmpty) None else Some(stack.pop()) }
    )
  }

  fork(server)

  /** Shut down the stack, terminating the server thread. */
  def shutdown() = { pushC.close(); popC.close() }
}

// =======================================================
