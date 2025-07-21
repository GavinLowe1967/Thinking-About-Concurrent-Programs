
package tacp.clientServer

import ox.scl._

import RAServer._

/** A partial resource server. 
  * This version buffers requests until they can be served.
  * @param numResources the number of resources.  */
class PartialRAServer(numResources: Int){
  private type ReplyChan = Chan[Resource]
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
	if(r == numResources) pending.enqueue(replyChan) // Client has to wait.
        else{  // Pass resource r back to client. 
	  free(r) = false; replyChan!r
        }
      }
      | returnChan =?=> { r =>
          if(pending.nonEmpty)
            pending.dequeue()!r // Allocate r to blocked client.
          else free(r) = true
      }
      | shutdownChan =?=> { _ => done = true }
    )
    acquireRequestChan.close(); returnChan.close(); shutdownChan.close()
  }

  // Fork off the server
  fork(server)

  /** Request a resource. 
    * In fact, this version never returns None. */
  def requestResource(me: ClientId): Resource = {
    val replyChan = new OnePlaceBuffChan[Resource]
    acquireRequestChan!replyChan  // Send request.
    replyChan?() // Wait for response.
  }

  /** Return a resource. */
  def returnResource(me: ClientId, r: Resource) = returnChan!r

  /** Shut down the server. */
  def shutdown() = shutdownChan!()
}

// =======================================================

object PartialRATest{
  // Import log-based definitions from RATest.
  import RATest.{LogEvent,GotResource,ReturnedResource,checkLog}

  /* We use clients that never holds more than two resources each, and return
   * all their resources at the end.  We assume that there are more resources
   * than clients.  This means that any time all resources are allocated, some
   * client must be about to return a resource, so the system cannot
   * deadlock. */

  var iters = 1000

  def client(me: ClientId, resourceServer: PartialRAServer, log: Log[LogEvent]) 
  = thread(s"client $me"){
    var got = new scala.collection.mutable.Queue[Resource]()
    val random = new scala.util.Random
    for(_ <- 0 until iters){
      if(got.length < 2 && random.nextInt(2) == 0){ // Acquire new resource
	val r = resourceServer.requestResource(me)
        log.add(me, GotResource(me, r)); got.enqueue(r)
      }
      else if(!got.isEmpty){     // Return resource
	val r = got.dequeue()
        log.add(me, ReturnedResource(me, r))
	resourceServer.returnResource(me, r)
      }
    }
    // At end, return resources.
    for(r <- got){
      log.add(me, ReturnedResource(me, r))
      resourceServer.returnResource(me, r)
    }
  }

  /** Run a single test. */
  def runTest(
      resourceServer: PartialRAServer, numClients: Int, numResources: Int) = {
    require(numResources > numClients)
    val log = new Log[LogEvent](numClients)
    val clients = 
      || (for (i <- 0 until numClients) yield client(i, resourceServer, log))
    try{ run(clients) } finally{ log.toFile("logFile") }
    resourceServer.shutdown()
    if(!checkLog(log.get, numResources)) sys.exit()
  }

  def main(args: Array[String]) = {
    var numClients = 5; var numResources = 6; var reps = 1000
    for(r <- 0 until reps){
      val resourceServer = new PartialRAServer(numResources)
      runTest(resourceServer, numClients, numResources)
      if(r%5 == 0) print(".")
    }
    println()
  }
}
