package tacp.monitors
 
import ox.scl._

/** Trait for the readers and writers problem. */
trait ReadersWritersLock{
  /** A reader enters the critical section. */
  def readerEnter(): Unit
  /** A reader exits the critical section. */
  def readerExit(): Unit
  /** A writer enters the critical section. */
  def writerEnter(): Unit
  /** A writer exits the critical section. */
  def writerExit(): Unit
} 

