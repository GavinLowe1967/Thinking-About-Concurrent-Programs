package tacp.datatypes

import ox.scl._
import scala.collection.mutable.Stack

trait PartialStack1[T] extends PartialStack[T]{
  def shutdown() = {}
}

/** A bounded partial stack implemented as a monitor. */
class MonitorBoundedPartialStack[T](bound: Int) extends PartialStack1[T]{
  /** The stack itself. */
  private val stack = new Stack[T]

  /** A monitor object, to control the synchronisations. */
  private val lock = new Lock

  /** Condition for signalling that the stack is not full. */
  private val notFull = lock.newCondition

  /** Condition for signalling that the stack is not empty. */
  private val notEmpty = lock.newCondition

  /** Push x.  Blocks while the queue is full. */
  def push(x: T) = lock.mutex{
    notFull.await(stack.length < bound) 
    stack.push(x)
    notEmpty.signal()
  }

  /** Dequeue a value.  Blocks until the queue is non-empty. */
  def pop(): T = lock.mutex{
    notEmpty.await(stack.nonEmpty) // Wait for a signal.
    val result = stack.pop()
    notFull.signal()
    result
  }
}

// =======================================================

/** A linearizability tester for bounded partial stacks. */
object BoundedPartialStackTest{
  var iters = 200  // Number of iterations by each worker
  val MaxVal = 20 // Maximum value placed in the queue
  //var pushProb = 0.5 // probability of doing a push
  var bound = 10 // the bound on the length of the queue

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

  def main(args: Array[String]) = {
    // parse arguments
    var i = 0//; var queueType = "monitor"
    val p = 4      // Number of workers 
    var reps = 10000  // Number of repetitions
    while(i < args.length) args(i) match{
      case "--iters" => iters = args(i+1).toInt; i += 2 
      case "--reps" => reps = args(i+1).toInt; i += 2 
      // case "--monitor" => queueType = "monitor"; i += 1
      case "--bound" => bound = args(i+1).toInt; i += 2
      case arg => println("Unrecognised argument: "+arg); sys.exit()
    }

    for(r <- 0 until reps){
      // The shared concurrent queue
      val concStack = new MonitorBoundedPartialStack[Int](bound)
      val seqStack = new ImmutableStack[Int]()
      val tester = LinearizabilityTester[SeqStack,ConcStack](
        seqStack, concStack, p, worker _)
      assert(tester() > 0)
      concStack.shutdown()

      if(r%50 == 0) print(".")
    } // end of for loop
    println()
  }
}
