package tacp.jvmMonitors

import ox.scl._
import scala.collection.mutable.Queue

/** A partial queue that terminates if all worker threads are attempting to
  * dequeue, and the queue is empty.  This implementation uses a monitor
  * internally.
  * @param numWorkers the number of worker threads. */
class MonitorTerminatingPartialQueue[A](numWorkers: Int){
  /** The queue itself. */
  private val queue = new Queue[A]

  /** The number of threads currently waiting to perform a dequeue. */
  private var waiting = 0

  /** Has the queue been shut down? */
  private var done = false

  /** Enqueue x.  */
  def enqueue(x: A) = synchronized{ 
    if(!done){
      queue.enqueue(x)
      if(waiting > 0) notify()
    }
  }

  /** Attempt to dequeue a value.  
    * @return None if the queue has been shut down, or it the queue is empty
    * and all threads are trying to dequeue. */
  def dequeue(): Option[A] = synchronized{
    if(!done && queue.isEmpty){
      if(waiting == numWorkers-1) shutdown() // System should terminate
      else{ // we need to wait
        waiting += 1
        while(queue.isEmpty && !done) wait()
        waiting -= 1
      }
    }
    if(done) None 
    else Some(queue.dequeue())
  }

  /** Shut down this queue. */
  def shutdown() = synchronized{ done = true; notifyAll() }
}


// =======================================================

import tacp.datatypes.{Graph,GraphSearch,MonitorShardedSet}

/** A class to search Graph g concurrently, using numWorkers workers. */
class MonitorGraphSearch[N](g: Graph[N], numWorkers: Int) 
    extends GraphSearch[N](g){

  /** Try to find a path in g from start to a node that satisfies isTarget. */
  def apply(start: N, isTarget: N => Boolean): Option[List[N]] = {
    // The number of shards to use: a power of 2, at least 4*numShards
    var numShards = 4; while(numShards < 4*numWorkers) numShards *= 2
    // All nodes seen so far.
    val seen = new MonitorShardedSet[N](numShards); seen.add(start)
    // Queue storing edges and the path leading to that edge
    val queue = new MonitorTerminatingPartialQueue[(N, List[N])](numWorkers)
    queue.enqueue((start, List(start)))    
    // Variable that ends up holding the result.  Protected by synchronized block.
    var result: Option[List[N]] = None

    // A single worker
    def worker = thread("worker"){
      var done = false
      while(!done) queue.dequeue() match{
        case Some((n, path)) =>
          for(n1 <- g.succs(n)){
            if(isTarget(n1)) synchronized{
              if(result.isEmpty){ // we're the first one to find a result
                result = Some(path :+ n1) // store result
                queue.shutdown() // cause others to terminate
                done = true
              }
            }
            else if(seen.add(n1)) queue.enqueue((n1, path :+ n1))
          } 
        case None => done = true
      } // end of while(...) ... match
    }

    // Release the workers. 
    run(|| (for(_ <- 0 until numWorkers) yield worker))
    result
  }
}
