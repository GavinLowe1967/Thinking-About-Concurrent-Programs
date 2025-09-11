package tacp.dataParallel

import ox.scl._
import tacp.datatypes.{Graph,GraphSearch,ServerConcSet,ServerTotalQueue}

class BFSGraphSearch[N](g: Graph[N], numWorkers: Int) extends GraphSearch[N](g){
  /** Try to find a path in g from start to a node that satisfies isTarget. */
  def apply(start: N, isTarget: N => Boolean): Option[List[N]] = {
    if(isTarget(start)) Some(List(start))
    else{
      // All nodes seen so far.
      val seen = new ServerConcSet[N]; seen.add(start)
      val q1, q2 = new ServerTotalQueue[(N, Path)]
      q1.enqueue((start, List(start)))
      // Combining function for the barrier.  Each argument is a pair
      // (done,added), where done is true if a worker has found a solution,
      // and added is true if a worker added to the next ply.
      def f(pair1: (Boolean,Boolean), pair2: (Boolean,Boolean)) = 
        (pair1._1 || pair2._1, pair1._2 || pair2._2)
      val barrier = new CombiningBarrier(numWorkers, f)
      // Channel on which a worker tells the coordinator that it has found a
      // solution.
      val pathFound = new SyncChan[Path]
      var result: Option[Path] = None

      /* A single worker. */ 
      def worker(me: Int) = thread(s"worker($me)"){
        var done = false; var thisPly = q1; var nextPly = q2
        while(!done){
          // plyDone records if we've completed the current ply.  added
          // records if we've added a node to the next ply.
          var plyDone = false; var added = false
          repeat(!plyDone){
            thisPly.dequeue() match{
              case Some((n, path)) =>
                for(n1 <- g.succs(n)){
                  if(isTarget(n1)){
                    pathFound!(path :+ n1); plyDone = true; done = true
                  } // Done.
                  else if(seen.add(n1)){
                    nextPly.enqueue((n1, path :+ n1)); added = true
                  }
                }
              case None => plyDone = true
            }
          } // End of inner repeat loop.
          val (d,a) = barrier.sync(me, (done,added)); done = d || !a
          if(!done){ // Swap thisPly and nextPly for the next round. 
            val t = thisPly; thisPly = nextPly; nextPly = t
          }
        }
        pathFound.close()
      } // End of worker.

      def coordinator = thread("coordinator"){
        attempt{ result = Some(pathFound?()) }{ }
        q1.shutdown(); q2.shutdown(); seen.shutdown(); pathFound.close()
      }

      val workers = || (for(i <- 0 until numWorkers) yield worker(i))
      run(workers || coordinator)
      result
    }
  }
}
