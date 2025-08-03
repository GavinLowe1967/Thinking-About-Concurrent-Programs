// Simple client-server resource-allocation mechanism.

package tacp.clientServer

import ox.scl._

/** A trait for the different resource-allocation servers. */
trait RAServer{
  /** Client identities. */
  type ClientId = Int

  /** Resource identities. */
  type Resource = Int

  /** Request a resource. */
  def requestResource(me: ClientId): Option[Resource]

  /** Return a resource. */
  def returnResource(me: ClientId, r: Resource): Unit

  /** Shut down the server. */
  def shutdown: Unit
}

// -------------------------------------------------------

/** A resource server. 
  * This version assumes the number of clients is known initially. 
  * @param clients the number of clients.
  * @param numResources the number of resources.  */
class RAServer1(clients: Int, numResources: Int) extends RAServer{
  /* Channel for requesting a resource. */
  private val acquireRequestChan = new SyncChan[ClientId]

  /* Channels for optionally returning a resouce, indexed by client IDs. */
  private val acquireReplyChan = 
    Array.fill(clients)(new SyncChan[Option[Resource]])

  /* Channel for returning a resource. */
  private val returnChan = new SyncChan[Resource]

  /* Channel for shutting down the server. */
  private val shutdownChan = new SyncChan[Unit]

  private def server = thread("server"){
    // Record whether resource i is available in free(i)
    val free = Array.fill(numResources)(true); var done = false

    serve(!done)(
      acquireRequestChan =?=> { c => 
	// Find free resource
	var r = 0
	while(r < numResources && !free(r)) r += 1
	if(r == numResources) acquireReplyChan(c)!None
        else{  // Pass resource r back to client c.
	  free(r) = false; acquireReplyChan(c)!Some(r)
        }
      }
      | returnChan =?=> { r => free(r) = true }
      | shutdownChan =?=> { _ => done = true }
    )

    acquireRequestChan.close(); returnChan.close(); shutdownChan.close()
    acquireReplyChan.foreach(_.close())
  }

  // Fork off the server
  fork(server)

  /** Request a resource. */
  def requestResource(me: ClientId): Option[Resource] = {
    acquireRequestChan!me  // send request
    acquireReplyChan(me)?() // wait for response
  }

  /** Return a resource. */
  def returnResource(me: ClientId, r: Resource) = returnChan!r

  /** Shut down the server. */
  def shutdown = shutdownChan!()
}

// =======================================================

/** A resource server. 
  * This version assumes the number of clients is not known initially.
  * @param numResources the number of resources.  */
class RAServer2(numResources: Int) extends RAServer{
  private type ReplyChan = Chan[Option[Resource]]
  /* Channel for requesting a resource. */
  private val acquireRequestChan = new SyncChan[ReplyChan]
  /* Channel for returning a resource. */
  private val returnChan = new SyncChan[Resource]
  /* Channel for shutting down the server. */
  private val shutdownChan = new SyncChan[Unit]

  private def server = thread{
    // Record whether resource i is available in free(i)
    val free = Array.fill(numResources)(true); var done = false

    serve(!done)(
      acquireRequestChan =?=> { replyChan => 
	// Find free resource
	var r = 0
	while(r < numResources && !free(r)) r += 1
	if(r == numResources) replyChan!None
        else{  // Pass resource r back to client 
	  free(r) = false; replyChan!Some(r)
        }
      }
      | returnChan =?=> { r => free(r) = true }
      | shutdownChan =?=> { _ => done = true }
    )

    acquireRequestChan.close(); returnChan.close(); shutdownChan.close()
  }

  // Fork off the server
  server.fork

  /** Request a resource. */
  def requestResource(me: ClientId): Option[Resource] = {
    val replyChan = new OnePlaceBuffChan[Option[Resource]]
    acquireRequestChan!replyChan  // send request
    replyChan?() // wait for response
  }

  /** Return a resource. */
  def returnResource(me: ClientId, r: Resource) = returnChan!r

  /** Shut down the server. */
  def shutdown = shutdownChan!()
}


// -------------------------------------------------------

/** A resource server. 
  * This version buffers requests until they can be served.
  * @param numResources the number of resources.  */
class RAServer3(numResources: Int) extends RAServer{
  private type ReplyChan = Chan[Option[Resource]]
  /* Channel for requesting a resource. */
  private val acquireRequestChan = new SyncChan[ReplyChan]
  /* Channel for returning a resource. */
  private val returnChan = new SyncChan[Resource]
  /* Channel for shutting down the server. */
  private val shutdownChan = new SyncChan[Unit]

  private def server = thread{
    // Record whether resource i is available in free(i)
    val free = Array.fill(numResources)(true)
    // Reply channels for requests that cannot be served immediately.
    val pending = new scala.collection.mutable.Queue[ReplyChan]
    // Invariant: if pending is non-empty, then all entries in free are false.
    var done = false

    serve(!done)(
      acquireRequestChan =?=> { replyChan => 
	// Find free resource
	var r = 0
	while(r < numResources && !free(r)) r += 1
	if(r == numResources) pending.enqueue(replyChan) // client has to wait
        else{  // Pass resource r back to client 
	  free(r) = false; replyChan!Some(r)
        }
      }
      | returnChan =?=> { r =>
          if(pending.nonEmpty)
            pending.dequeue()!Some(r) // allocate r to blocked client
          else free(r) = true
      }
      | shutdownChan =?=> { _ => done = true }
    )
    acquireRequestChan.close(); returnChan.close(); shutdownChan.close()
  }

  // Fork off the server
  server.fork

  /** Request a resource. 
    * In fact, this version never returns None. */
  def requestResource(me: ClientId): Option[Resource] = {
    val replyChan = new OnePlaceBuffChan[Option[Resource]]
    acquireRequestChan!replyChan  // send request
    replyChan?() // wait for response
  }

  /** Return a resource. */
  def returnResource(me: ClientId, r: Resource) = returnChan!r

  /** Shut down the server. */
  def shutdown = shutdownChan!()
}
