package tacp.datatypes

import ox.scl._

class SetMax{
  private val addC = new SyncChan[Int]

  private val getC = new SyncChan[Int]

  /** Add x to the set. */
  def add(x: Int) = addC!x

  /** Get the maximum entry, and shut down the server. */
  def getMax(): Int = getC?()

  private def server = thread{
    val set = new scala.collection.mutable.HashSet[Int]
    var done = false
    serve(!done)(
      addC =?=> { x => set.add(x) }
      | getC =!=> { done = true; set.max }
    )
  }
}


object ArrayMax{
  def apply(a: Array[Int], numWorkers: Int): Int = {
    val n = a.length; val set = new SetMax
    def worker(me: Int) = thread{
      val start = me*n/numWorkers; val end = (me+1)*n/numWorkers
      for(i <- start until end) set.add(a(i))
    }
    run(|| (for (i <- 0 until numWorkers) yield worker(i)))
    set.getMax()
  }

}
