package tacp.datatypes

/** A trait representing an unlabelled graph with nodes of type N. */
trait Graph[N]{
  /** The successors of node n. */
  def succs(n: N): List[N]
}

// =======================================================

/** Trait representing a graph search problem in graph g. */
abstract class GraphSearch[N](g: Graph[N]){
  type Path = List[N]

  /** Try to find a path in g from start to a node that satisfies isTarget. */
  def apply(start: N, isTarget: N => Boolean): Option[Path]
}

// =======================================================

import scala.collection.mutable.{HashSet,Queue}

/** Sequential graph search implementation. */
class SeqGraphSearch[N](g: Graph[N]) extends GraphSearch[N](g){
  def apply(start: N, isTarget: N => Boolean): Option[Path] = {
    if(isTarget(start)) Some(List(start))
    else{
      // Queue storing nodes and the path leading to that node.
      val queue = new Queue[(N, Path)](); queue += ((start, List(start)))
      // All nodes seen so far.
      val seen = new HashSet[N]; seen.add(start)

      while(queue.nonEmpty){
        val (n, path) = queue.dequeue()
        for(n1 <- g.succs(n)){
          if(isTarget(n1)) return Some(path :+ n1)
          else if(seen.add(n1)) queue.enqueue((n1, path :+ n1))
        }
      }
      None
    }
  }
}

// =======================================================

import ox.scl._


/** A concurrent set with an add operation. */
class ConcSet[A]{
  private type ReplyChan = OnePlaceBuffChan[Boolean]

  /** Channel from clients to the server. */
  private val addC = new SyncChan[(A, ReplyChan)]

  /** Add x to this set.  Return true if x was not previously in the set. */
  def add(x: A): Boolean = {
    val c = new ReplyChan; addC!(x,c); c?()
  }

  private def server = thread{
    val set = new HashSet[A]
    repeat{ val (x,c) = addC?(); val res = set.add(x); c!res }
  }

  fork(server)

  /** Shut down the object. */
  def shutdown() = addC.close()
}

// =======================================================

/** A concurrent set with an add operation.  Implementation using a monitor. */
class MonitorConcSet[A]{
  /** The underlying set. */
  private val set = new HashSet[A]

  /** Add x to this set.  Return true if x was not previously in the set. */
  def add(x: A): Boolean = synchronized{ set.add(x) }
}

// -------------------------------------------------------

/** A class to search Graph g concurrently, using numWorkers worker threads. */
class ConcGraphSearch[N](g: Graph[N], numWorkers: Int) extends GraphSearch[N](g){

  /** Try to find a path in g from start to a node that satisfies isTarget. */
  def apply(start: N, isTarget: N => Boolean): Option[List[N]] = {
    if(isTarget(start)) Some(List(start))
    else{
      // All nodes seen so far.
      val seen = new ConcSet[N]; seen.add(start)
      // Queue storing edges and the path leading to that edge
      val queue = new TerminatingPartialQueue[(N, Path)](numWorkers)
      queue.enqueue((start, List(start)))

      // Channel on which a worker tells the coordinator that it has found a
      // solution.
      val pathFound = new SyncChan[Path]

      // A single worker
      def worker = thread("worker"){
        var done = false
        repeat(!done){
          queue.dequeue() match{
            case Some((n, path)) =>
              for(n1 <- g.succs(n)){
                if(isTarget(n1)){ pathFound!(path :+ n1); done = true } // Done.
                else if(seen.add(n1)) queue.enqueue((n1, path :+ n1))
              }
            case None => done = true
          }
        }
        pathFound.close() // Causes coordinator to close down.
      }

      // Variable that ends up holding the result; written by coordinator.
      var result: Option[Path] = None

      def coordinator = thread("coordinator"){
        attempt{ result = Some(pathFound?()) }{ }
        queue.shutdown() // Close queue; this will cause most workers to terminate.
        seen.shutdown() // Shutdown seen set.
        pathFound.close() // In case another thread has found solution
      }

      val workers = || (for(_ <- 0 until numWorkers) yield worker)
      run(workers || coordinator)
      result
    }
  }
}
