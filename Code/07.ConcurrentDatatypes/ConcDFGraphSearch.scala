package tacp.datatypes

import ox.scl._

class ConcDFGraphSearch[N](g: Graph[N], numWorkers: Int)
    extends DFGraphSearch[N](g){
  /**The number of workers. */
  // val numWorkers = 8

  /** Perform a depth-first search in g, starting from start, for a node that
    * satisfies isTarget.  This assumes that the start node is not itself a
    * target. */
  def apply(start: N, isTarget: N => Boolean): Option[N] = {
    // The stack supporting the depth-first search, holding nodes.
    val stack = new TerminatingPartialStack[N](numWorkers); stack.push(start)
    val solnFound = new SyncChan[N] // From workers to controller.

    // A worker thread
    def worker = thread("worker"){
      var done = false
      repeat(!done){
        stack.pop() match{
          case Some(n) => 
            for(n1 <- g.succs(n)){
              if(isTarget(n1)) solnFound!n1 else stack.push(n1)
            }
          case None => done = true
        }
      }
      solnFound.close() // Causes coordinator to terminate.
    }

    // Variable that ends up holding the result; written by coordinator. 
    var result: Option[N] = None

    // The coordinator.
    def coordinator = thread("coordinator"){
      attempt{ result = Some(solnFound?()) }{ }
      stack.shutdown() // Close stack; this will cause most workers to terminate.
      solnFound.close() // In case another thread has found solution.
    }

    val workers = || (for(_ <- 0 until numWorkers) yield worker)
    run(workers || coordinator)
    result
  }
}
