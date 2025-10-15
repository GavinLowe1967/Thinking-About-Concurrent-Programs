package tacp.clientServer

import ox.scl._

/** A tester for an exchanger. */
object ExchangerTest{
  val CS = 0; val Mon = 1 // Choice of implementation to test.

  /** Do a single test. */
  def doTest(switch: Int) = {
    val n = 2*scala.util.Random.nextInt(10); val results = new Array[Int](n)
    val exchanger: ExchangerT[Int] =
      if(switch == CS) new Exchanger[Int]
      else{ assert(switch == Mon); new tacp.monitors.MonitorExchanger[Int] }
    def worker(me: Int) = thread(s"worker($me)"){ 
      val x = exchanger.exchange(me); results(me) = x 
    }
    run(|| (for(i <- 0 until n) yield worker(i)))
    for(i <- 0 until n){ val x = results(i);  assert(x != i && results(x) == i) }
    exchanger.shutdown()
  }

  def main(args: Array[String]) = {
    var switch = CS; var i = 0
    while(i < args.length) args(i) match{
      case "--monitor" => switch = Mon; i += 1
    }

    for(i <- 0 until 5000){ doTest(switch); if(i%50 == 0) print(".") }
    println()
  }
}
 
