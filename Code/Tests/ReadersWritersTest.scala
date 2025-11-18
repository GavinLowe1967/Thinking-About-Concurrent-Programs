package tacp.tests

import ox.scl._

import tacp.monitors.{ReadersWritersLock, MonitorReadersWriters0, 
  MonitorReadersWriters}

/** A linearizability tester for the readers and writers problem. */
object ReadersWritersTest{
  var iters = 200  // Number of iterations by each worker.
  val p = 6 // Number of workers.

  /** The type of concurrent objects being tested. */
  type CRW = ReadersWritersLock

  /** The type of sequential specification objects, recording the number of
    * readers and writers currently in the critical region. */
  case class SRW(readers: Int, writers: Int)

  def seqReaderEnter(s: SRW): (Unit, SRW) = {
    val SRW(r,w) = s; require(w == 0); ((), SRW(r+1, w))
  }

  def seqReaderExit(s: SRW): (Unit, SRW) = {
    val SRW(r,w) = s; assert(r > 0); ((), SRW(r-1, w))
  }

  def seqWriterEnter(s: SRW): (Unit, SRW) = {
    val SRW(r,w) = s; require(w == 0 && r == 0); ((), SRW(0, 1))
  }

  def seqWriterExit(s: SRW): (Unit, SRW) = {
    val SRW(r,w) = s; assert(w == 1); ((), SRW(0, 0))
  }

  def worker(me: Int, log: LinearizabilityLog[SRW, CRW]) = {
    val random = new scala.util.Random
    // Pause for a short while.
    def pause() = while(random.nextInt(1000) > 0){} 
    for(i <- 0 until iters){
      pause()
      if(random.nextInt(2) == 0){
        log(_.writerEnter(), "writerEnter", seqWriterEnter)
        pause()
        log(_.writerExit(), "writerExit", seqWriterExit)
      }
      else{
        log(_.readerEnter(), "readerEnter", seqReaderEnter)
        pause()
        log(_.readerExit(), "readerExit", seqReaderExit)
      }
    }
  }

  val MRW0 = 0; val MRW = 1; val Semaphore = 2

  def doTest(choice: Int) = {
    val rw: CRW = choice match{
      case MRW0 => new MonitorReadersWriters0
      case MRW => new MonitorReadersWriters
      case Semaphore => new tacp.semaphores.SemaphoreReadersWriters
    }
    val seqRW = SRW(0,0)
    val tester = LinearizabilityTester[SRW,CRW](seqRW, rw, p, worker _)
    assert(tester() > 0)
  }

  def main(args: Array[String]) = {
    var i = 0; var choice = MRW0
    while(i < args.length) args(i) match{
      case "--monitor0" => choice = MRW0; i += 1
      case "--monitor" => choice = MRW; i += 1
      case "--semaphore" => choice = Semaphore; i += 1
    }

    for(i <- 0 until 1000){ doTest(choice); if(i%10 == 0) print(".") }
    println()
  }

}
