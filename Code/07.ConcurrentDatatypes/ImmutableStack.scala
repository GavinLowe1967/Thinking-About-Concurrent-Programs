package tacp.datatypes

/** An immutable stack holding data of type A. */
class ImmutableStack[A](private val stack: List[A] = List()){

  /** Push x onto the stack.  Returns a new stack extending this with x. */
  def push(x: A): ImmutableStack[A] = new ImmutableStack(x :: stack)

  /** Perform a pop from the stack.  Returns the value popped and the resulting
    * stack. */
  def pop2(): (A, ImmutableStack[A]) = {
    require(stack.nonEmpty); (stack.head, new ImmutableStack(stack.tail))
  }

  /** Is the stack empty? */
  def isEmpty = stack.isEmpty

  override def hashCode = stack.hashCode

  override def equals(other: Any) = other match{
    case st: ImmutableStack[_] => st.stack == this.stack
  }
}
