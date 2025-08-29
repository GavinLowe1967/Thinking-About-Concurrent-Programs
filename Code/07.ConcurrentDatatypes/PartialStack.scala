package tacp.datatypes

import ox.scl._

/** A partial stack. */
trait PartialStack[T]{
  /** Push x onto the stack. */
  def push(x: T): Unit

  /** Pop a value from the stack.  Blocks until the stack is non-empty. */
  def pop(): T

  /** Shut down the stack. */
  def shutdown(): Unit
}

// =======================================================

class ServerPartialStack[T] extends PartialStack[T]{
  private val pushChan = new SyncChan[T]

  private val popChan = new SyncChan[T]

  /** Push x. */
  def push(x: T) = pushChan!x

  /** Pop a value.  Blocks until the stack is non-empty. */
  def pop(): T = popChan?()

  private def server = thread("server"){
    val stack = new scala.collection.mutable.Stack[T]
    serve(
      pushChan =?=> { x => stack.push(x) }
      | stack.nonEmpty && popChan =!=> stack.pop()
    )
  }

  fork(server)

  /** Shut down the queue. */
  override def shutdown() = { pushChan.close(); popChan.close() }
}

// =======================================================

object PartialStackTest{
  var iters = 20 // Number of iterations by each worker
  // Note: higher values of iters lead to large memory usage.
  val MaxVal = 20 // Maximum value placed in the queue
  val p = 4      // Number of workers

  type S = ImmutableStack[Int]; type C = PartialStack[Int]

  def seqPush(x: Int)(stack: S) : (Unit, S) = 
    ((), stack.push(x))
  def seqPop(stack: S) : (Int, S) = {
    require(!stack.isEmpty); stack.pop2()
  }

  /** A worker for the LinTesters */
  def worker(me: Int, log: LinearizabilityLog[S, C]) = {
    val random = new scala.util.Random
    for(i <- 0 until iters){
      if(me%2 == 0){
        val x = random.nextInt(MaxVal)
        log(_.push(x), s"push($x)", seqPush(x))
      }
      else log(_.pop(), "pop", seqPop)
    }
  }

  def doTest = {
    val concStack = new ServerPartialStack[Int]; val seqStack = new S
    val tester = LinearizabilityTester[S,C](seqStack, concStack, p, worker)
    if(tester() <= 0) sys.exit()
    concStack.shutdown()
  }

  def main(args: Array[String]) = {
    // parse arguments
    var i = 0 // ; var queueType = "server"
    var reps = 10000  // Number of repetitions
    while(i < args.length) args(i) match{
      case "--iters" => iters = args(i+1).toInt; i += 2 
      case "--reps" => reps = args(i+1).toInt; i += 2 
      //case "--monitor" => queueType = "monitor"; i += 1
      // case "--semaphore" => queueType = "semaphore"; i += 1
      // case "--countingSemaphore" => queueType = "counting semaphore"; i += 1
      case arg => println("Unrecognised argument: "+arg); sys.exit()
    }

    for(r <- 0 until reps){ doTest; if(r%50 == 0) print(".") } 
    println()
  }
}
