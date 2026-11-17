package tacp.dataParallel
import ox.scl._

trait Recurrence{
  val N: Int

  protected val table = Array.ofDim[Int](N,N)

  /** An arbitrary function. */
  def g(i: Int, j: Int): Int = i+j

  /** Update the entry in (i,j) of table.  
    * Pre: if i > 0, table(i-1)(j) has been calculated; 
    * if j > 0, table(i)(j-1) has been calculated. */
  def updateEntry(i: Int, j: Int) = {
    table(i)(j) = 
      if(i == 0){ if(j == 0) 0 else table(i)(j-1) + g(i,j) }
      else if(j == 0) table(i-1)(j) + g(i,j) 
      else table(i)(j-1) + table(i-1)(j) + g(i,j)
  }

  /** Calculate all values in the table. */
  def calculate: Array[Array[Int]]
}

// ==================================================================

/** Sequential implementation. */
class SeqRecurrence(override val N: Int) extends Recurrence{
  def calculate = {
    for(i <- 0 until N; j <- 0 until N) updateEntry(i,j)
    table
  }
}

// ==================================================================

/** First concurrent solution.  Each worker(i) is responsible for row i.
  * The algorithm proceeds in rounds.  On round r, entries (i,j) such that i+j
  * = r are calculated. */
class BarrierRecurrence(override val N: Int) extends Recurrence{
  private val barrier = new Barrier(N)

  private def worker(i: Int) = thread{
    for(r <- 0 until 2*N-1){
      val j = r-i
      if(j >= 0 && j < N) updateEntry(i, j)
      barrier.sync(i)
    }
  }

  def calculate = {
    run( || (for( i <- 0 until N) yield worker(i)) )
    table
  }
}

// ==================================================================

/** Second concurrent solution.  Each worker(i) is responsible for row i.
  * Worker(i) sends a message to worker(i+1) each time it completes an entry.
  */
class ChannelsRecurrence(override val N: Int) extends Recurrence{
  /** Channels for synchronisation.  channels(i) is used for messages from
    * worker(i) to worker(i+1). */
  private val channels = Array.fill(N-1)(new BuffChan[Unit](N))

  private def worker(i: Int) = thread{
    for(j <- 0 until N){
      if(i > 0) channels(i-1)?()
      updateEntry(i,j)
      if(i < N-1) channels(i)!()
    }
  }

  def calculate = {
    run( || (for( i <- 0 until N) yield worker(i)) )
    table
  }
}

// ==================================================================

/** Third concurrent solution.   Each worker(i) is responsible for row i.
  * Worker(i) increments done(i) each time it completes an entry. */
class MonitorRecurrence(override val N: Int) extends Recurrence{
  /** done(i) holds a value j indicating that worker(i) has completed the entry
    * (i,j), or initial value -1 to indicate that no such entry has been
    * completed. */
  private val done = Array.fill(N-1)(-1)

  /** locks(i) protects done(i). */
  private val locks = Array.fill(N-1)(new AnyRef)

  private def worker(i : Int) = thread{
    for(j <- 0 until N){
      if(i > 0) locks(i-1).synchronized{ while(done(i-1) < j) locks(i-1).wait() }
      updateEntry(i,j)
      if(i < N-1) locks(i).synchronized{ done(i) = j; locks(i).notify() }
    }
  }

  def calculate = {
    run( || (for( i <- 0 until N) yield worker(i)) )
    table
  }
}

// ==================================================================

class SemaphoresRecurrence(override val N: Int) extends Recurrence{
  /** sems(i) is used as a signal from worker(i) to worker(i+1) to indicate that
    * it has completed another entry in table.  We need counting semaphores to
    * accumulate signals. */
  private val sems = Array.fill(N-1)(new CountingSemaphore)

  private def worker(i : Int) = thread{
    for(j <- 0 until N){
      if(i > 0) sems(i-1).down()
      updateEntry(i,j)
      if(i < N-1) sems(i).up()
    }
  }

  def calculate = {
    run( || (for( i <- 0 until N) yield worker(i)) )
    table
  }
}

// ==================================================================

/** Testing code. */
object RecurrenceTest {
  type Table = Array[Array[Int]]

  val N = 10

  def printTable(table: Table) = 
    for(i <- 0 until N){
      for(j <- 0 until N) print(table(i)(j).toString+"\t"); println()
    }

  /** Run a test on cr.  Make sure it produces the same result as table. */
  def test(cr: Recurrence, table: Table) = {
    val table1 = cr.calculate
    // printTable(table1); println
    for(i <- 0 until N) assert(table(i).sameElements(table1(i)))
  }

  def main(args: Array[String]) = {
    val sr = new SeqRecurrence(N)
    val table = sr.calculate
    printTable(table); println()

    for(i <- 0 until 1000){
      test(new BarrierRecurrence(N), table)
      test(new ChannelsRecurrence(N), table)
      test(new MonitorRecurrence(N), table)
      test(new SemaphoresRecurrence(N), table)
      if(i%10 == 0) print(".")
    }

    println()
  }

}

// ==================================================================

