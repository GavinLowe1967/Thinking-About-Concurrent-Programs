// Simple client-server resource-allocation mechanism.

package tacp.clientServer

import ox.scl._

object RAServer{
  /** Client identities. */
  type ClientId = Int

  /** Resource identities. */
  type Resource = Int
}

import RAServer._

/** A trait for the different resource-allocation servers. */
trait RAServer{
  /** Request a resource. */
  def requestResource(me: ClientId): Option[Resource]

  /** Return a resource. */
  def returnResource(me: ClientId, r: Resource): Unit

  /** Shut down the server. */
  def shutdown(): Unit
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
    // acquireReplyChan.foreach(_.close())
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
  def shutdown() = shutdownChan!()
}

// -------------------------------------------------------

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
  def shutdown() = shutdownChan!()
}





// =======================================================

import ox.scl.debug.Log
import scala.util.Random

/** An object to test the resource allocation server. */
object RATest{
  var iters = 1000 // # iterations by each client

  // Events put into the log
  abstract class LogEvent
  case class GotResource(c: ClientId, r: Resource) extends LogEvent
  case class ReturnedResource(c: ClientId, r: Resource) extends LogEvent

  /** A client */
  def client(me: ClientId, resourceServer: RAServer, log: Log[LogEvent]) 
  = thread(s"client $me"){
    var got = new scala.collection.mutable.Queue[Resource]()
    val random = new Random
    for(_ <- 0 until iters){
      if(random.nextInt(2) == 0){ // Acquire new resource
	resourceServer.requestResource(me) match{
          case Some(r) =>  log.add(me, GotResource(me, r)); got.enqueue(r)
          case None => {}  // try again
        }
      }
      else if(!got.isEmpty){     // Return resource
	val r = got.dequeue()
        log.add(me, ReturnedResource(me, r))
	resourceServer.returnResource(me, r)
      }
    }
  }

  /** Check that events represents a valid log: if a GotResource event happens,
    * no thread is currently holding the resource.
    * @return true if the log is valid.  */
  def checkLog(events: Array[LogEvent], numResources: Int): Boolean = {
    // Array showing which resources are held
    val held = Array.fill(numResources)(false)
    var error = false; var i = 0
    while(i < events.size && !error){
      events(i) match{
        case GotResource(_, r) =>
          if(held(r)){ // error!
            println("Error found:")
            println(events.take(i+1).mkString("\n"))
            error = true
          }
          else held(r) = true
        case ReturnedResource(_, r) => held(r) = false
      }
      i += 1
    }
    !error
  }

  /** Run a single test. */
  def runTest(resourceServer: RAServer, numClients: Int, numResources: Int) = {
    //println
    val log = new Log[LogEvent](numClients)
    val clients = 
      || (for (i <- 0 until numClients) yield client(i, resourceServer, log))
    try{ run(clients) } finally{ log.toFile("logFile") }
    resourceServer.shutdown()
    if(!checkLog(log.get, numResources)) sys.exit()
  }

  def main(args: Array[String]) = {
    // Parse command line arguments
    var numClients =  5 // number of clients
    var numResources = 10 // # resources
    var rsType = 1 // Which resource server to use
    var i = 0; var buffered = false
    var reps = 1000 // # times to repeat
    while(i < args.length) args(i) match{
      case "-1" => rsType = 1; i += 1
      case "-2" => rsType = 2; i += 1
      //case "-3" => rsType = 3; i += 1
      case "--buffered" => buffered = true; i += 1
      case "--iters" => iters = args(i+1).toInt; i += 2
      case "--reps" => reps = args(i+1).toInt; i += 2
      case arg => println("Unrecognised argument: "+arg); sys.exit()
    }

    // If RSServer runs out of resources, it deadlocks; the following tries to
    // avoid this.
    //if(rsType == 3){ numClients  = 30; numResources = 90; iters = 15 }
    /* A better approach might be to prevent clients from requesting another
     * resource if they already hold two, and to choose numResources such that
     * p < numResources < 2*p.  This cannot deadlock, but will require threads
     * to wait. */

    for(r <- 0 until reps){
      val resourceServer =
        if(rsType == 1) new RAServer1(numClients, numResources)
        else{ assert(rsType == 2); new RAServer2(numResources) }
        // else{ assert(rsType == 3); new RAServer3(numResources) }
      runTest(resourceServer, numClients, numResources)
      if(r%10 == 0) print(".")
      // println
    }
    println()
  }
}

// =======================================================

/** Linearisation testing for the resource allocation modules. */
object RALinTest{
  val numResources = 3
  var iters = 1000 // # iterations by each client
  var numWorkers = 5 // number of clients

  /** The sequential specification object.  `free` records which resources are
    * free. */
  class SeqAlloc(val free: Array[Boolean]){
    /** Produce a new SeqAlloc from this by updating so r maps to b. */
    def update(r: Int, b: Boolean): SeqAlloc = {
      val free1 = free.clone; free1(r) = b; new SeqAlloc(free1)
    }

    /* Override equality and hashCode. */
    override def equals(that: Any) = that match{
      case s: SeqAlloc => s.free.sameElements(free)
    }

    override def hashCode = {
      var h = 0; var i = 0
      while(i < numResources){ h *= 2; if(free(i)) h += 1; i += 1 }
      h
    }
  }

  /** A sequential request. */
  def seqRequest(s: SeqAlloc): (Option[Resource], SeqAlloc) = {
    var r = 0; val free = s.free
    while(r < numResources && !free(r)) r += 1
    if(r == numResources) (None, s)
    else (Some(r), s.update(r, false))
  }

  /** A sequential return. */
  def seqReturn(r: Resource)(s: SeqAlloc): (Unit, SeqAlloc) = 
    ((), s.update(r, true))

  /** A worker for the LinTesters */
  def worker(me: Int, log: LinearizabilityLog[SeqAlloc, RAServer]) = {
    val random = new scala.util.Random
    var got = new scala.collection.mutable.Queue[Resource]()
    for(_ <- 0 until iters){
      if(random.nextInt(2) == 0){ // Acquire new resource.
        // `res` will hold the result of the request, to allow the worker to
        // record which resources it holds.
        var res: Option[Resource] = None
        log(rs => {res = rs.requestResource(me); res}, "request", seqRequest)
        res match{ case Some(r) => got.enqueue(r); case None => {} }
      }
      else if(!got.isEmpty){     // Return resource,
	val r = got.dequeue()
        log(_.returnResource(me, r), s"return($r)", seqReturn(r))
      }
    }
  }

  /** Do a single test. */
  def doTest = {
    val ra = new RAServer1(numWorkers, numResources) 
    val s = new SeqAlloc(Array.fill(numResources)(true))
    val tester = 
      LinearizabilityTester[SeqAlloc, RAServer](s, ra, numWorkers, worker _)
    assert(tester() > 0)
    ra.shutdown() // Terminate server.
  }

  def main(args: Array[String]) = {
    for(i <- 0 until 100){ doTest; if(i%5 == 0) print(".") }
    println()
  }
}
