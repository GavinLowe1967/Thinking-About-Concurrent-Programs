package tacp.datatypes

import ox.scl._

/** A partial stack. */
trait PartialStack[T]{
  /** Push x onto the stack. */
  def push(x: T): Unit

  /** Pop a value from the stack.  Blocks until the stack is non-empty. */
  def pop(): T

  /** Shut down the stack. */
  def shutdown(): Unit = {}
} 

// =======================================================

class ServerPartialStack[T] extends PartialStack[T]{
  private val pushChan = new SyncChan[T]

  private val popChan = new SyncChan[T]

  /** Push x. */
  def push(x: T) = pushChan!x

  /** Pop a value.  Blocks until the stack is non-empty. */
  def pop(): T = popChan?()

  private def server = thread("server"){
    val stack = new scala.collection.mutable.Stack[T]
    serve(
      pushChan =?=> { x => stack.push(x) }
      | stack.nonEmpty && popChan =!=> stack.pop()
    )
  }

  fork(server)

  /** Shut down the queue. */
  override def shutdown() = { pushChan.close(); popChan.close() }
}

