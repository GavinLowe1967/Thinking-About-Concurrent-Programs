package tacp.datatypes

import ox.scl._

/** A total queue. */
trait TotalQueue[T]{
  /** Enqueue x. */
  def enqueue(x: T): Unit

  /** Dequeue a value.  Returns None if the queue is empty. */
  def dequeue(): Option[T]

  /** Shut down the queue. */
  def shutdown(): Unit
}

// -------------------------------------------------------

import scala.collection.mutable.Queue

class ServerTotalQueue[T] extends TotalQueue[T]{
  private val enqueueChan = new SyncChan[T]

  private val dequeueChan = new SyncChan[Option[T]]

  private def server = thread("server"){
    val queue = new Queue[T]
    serve(
      enqueueChan =?=> { x => queue.enqueue(x) }
      | dequeueChan =!=> { if(queue.nonEmpty) Some(queue.dequeue()) else None }
    )
  }

  fork(server)

  def enqueue(x: T) = enqueueChan!x

  def dequeue(): Option[T] = dequeueChan?()

  def shutdown() = { enqueueChan.close(); dequeueChan.close() }
}

// ==================================================================

class ServerTotalQueue2[T] extends TotalQueue[T]{
  private val enqueueChan = new SyncChan[T]

  private val dequeueChan = new SyncChan[SyncChan[Option[T]]]

  private def server = thread("server"){
    val queue = new Queue[T]
    serve(
      enqueueChan =?=> { x => queue.enqueue(x) }
      | dequeueChan =?=> { c => 
        if(queue.nonEmpty) c!Some(queue.dequeue()) else c!None }
    )
  }

  fork(server)

  def enqueue(x: T) = enqueueChan!x

  def dequeue(): Option[T] = { 
    val c = new SyncChan[Option[T]]; dequeueChan!c; c?() 
  }

  def shutdown() = { enqueueChan.close(); dequeueChan.close() }
}

// =======================================================

/** A deliberately faulty implementation of TotalChan.  The error is that this
  * uses buffered channels. */ 
class FaultyTotalQueue[T] extends TotalQueue[T]{

  private val enqueueChan = new OnePlaceBuffChan[T]

  private val dequeueChan = new OnePlaceBuffChan[Option[T]]

  private def server = thread("server"){
    val queue = new Queue[T]
    serve(
      enqueueChan =?=> { x => queue.enqueue(x) }
      | dequeueChan =!=> { if(queue.nonEmpty) Some(queue.dequeue()) else None }
    )
  }

  fork(server)

  def enqueue(x: T) = enqueueChan!x

  def dequeue(): Option[T] = dequeueChan?()

  def shutdown() = { enqueueChan.close(); dequeueChan.close() }

}
