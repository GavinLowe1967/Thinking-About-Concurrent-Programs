package tacp.tests

import ox.scl._
import tacp.datatypes.{
  PartialStack, MonitorBoundedPartialStack, JVMMonitorBoundedPartialStack}

/** A linearizability tester for bounded partial stacks. */
object BoundedPartialStackTest{
  var iters = 200  // Number of iterations by each worker
  val MaxVal = 20 // Maximum value placed in the queue
  //var pushProb = 0.5 // probability of doing a push
  var bound = 10 // the bound on the length of the queue
  val p = 4      // Number of workers

  type SeqStack = ImmutableStack[Int]
  type ConcStack = PartialStack[Int]

  def seqPush(x: Int)(st: SeqStack) : (Unit, SeqStack) = {
    require(st.length < bound); ((), st.push(x))
  }
  def seqPop(st: SeqStack) : (Int, SeqStack) = {
    require(!st.isEmpty); st.pop2()
  }

  /** A worker for the LinTesters */
  def worker(me: Int, log: LinearizabilityLog[SeqStack, ConcStack]) = {
    val random = new scala.util.Random(scala.util.Random.nextInt()+me*45207)
    for(i <- 0 until iters){
      if(me%2 == 0){
        val x = random.nextInt(MaxVal)
        log(_.push(x), "push("+x+")", seqPush(x))
      }
      else log(_.pop(), "pop", seqPop)
    }
  }

  val Monitor = 0; val JVMMonitor = 1

  def doTest(choice: Int) = {
    // The shared concurrent queue
    val concStack: ConcStack = choice match{
      case Monitor => new MonitorBoundedPartialStack[Int](bound)
      case JVMMonitor => new JVMMonitorBoundedPartialStack[Int](bound)
    }
    val seqStack = new ImmutableStack[Int]()
    val tester = LinearizabilityTester[SeqStack,ConcStack](
      seqStack, concStack, p, worker _)
    assert(tester() > 0)
    concStack.shutdown()
  }

  def main(args: Array[String]) = {
    // parse arguments
    var i = 0; var choice = Monitor
    var reps = 10000  // Number of repetitions
    while(i < args.length) args(i) match{
      case "--iters" => iters = args(i+1).toInt; i += 2 
      case "--reps" => reps = args(i+1).toInt; i += 2
      case "--JVM" => choice = JVMMonitor; i += 1
      case "--bound" => bound = args(i+1).toInt; i += 2
      case arg => println("Unrecognised argument: "+arg); sys.exit()
    }

    for(r <- 0 until reps){ doTest(choice); if(r%50 == 0) print(".") }
    println()
  }
}
