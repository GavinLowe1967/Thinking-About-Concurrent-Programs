package tacp.datatypes

import ox.scl._

/** A total queue. */
trait TotalStack[T]{
  /** Push x. */
  def push(x: T): Unit

  /** Pop a value.  Returns None if the queue is empty. */
  def pop(): Option[T]

  /** Shut down the queue. */
  def shutdown(): Unit
}

// =======================================================

class ServerStack[T] extends TotalStack[T]{
  /** Channel for pushing. */
  private val pushC = new SyncChan[T]

  /** Channel for popping. */
  private val popC = new SyncChan[Option[T]]

  /** Push x onto the stack. */
  def push(x: T) = pushC!x

  /** Optionally pop a value from the stack.
    * @return Some(x) where x is the value popped, or None if the stack is empty. */
  def pop(): Option[T] = popC?()

  private def server = thread{
    val stack = new scala.collection.mutable.Stack[T]
    serve(
      pushC =?=> { x => stack.push(x) }
      | popC =!=> { if(stack.isEmpty) None else Some(stack.pop()) }
    )
  }

  fork(server)

  /** Shut down the stack, terminating the server thread. */
  def shutdown() = { pushC.close(); popC.close() }
}

// =======================================================

object StackTest{
  // # iterations by each worker
  var iters = 200

  // probability of each operation being a push
  var pushProb = 0.30

  // max value added to the stack
  var maxValue = 20

  // # runs
  var reps = 1000

  // Sequential specification type
  type S =  ImmutableStack[Int] 

  // Type of concurrent object to be tested.
  type C = ServerStack[Int]

  // Sequential push operation
  def seqPush(x: Int)(stack: S): (Unit, S) = ((), stack.push(x))

  // Sequential pop operation
  def seqPop(stack: S): (Option[Int], S) =
    if(stack.isEmpty) (None, stack)
    else{ val(x, stack1) = stack.pop2(); (Some(x), stack1) }

  // A worker thread
  def worker(me: Int, log: LinearizabilityLog[S,C]) = {
    val random = new scala.util.Random
    for(i <- 0 until iters)
      if(random.nextFloat() <= pushProb){
        val x = random.nextInt(maxValue)
        log(_.push(x), s"push($x)", seqPush(x))
      }
      else log(_.pop(), "pop", seqPop)
  }

  def doTest = {
    val concStack = new ServerStack[Int]
    val seqStack = new ImmutableStack[Int]
    val tester = LinearizabilityTester[S, C](seqStack, concStack, 4, worker)
    if(tester() <= 0) sys.exit()
    concStack.shutdown()
  }

  // The main method
  def main(args: Array[String]) = {
    for(i <- 0 until reps){ doTest; if(i%10 == 0) print(".") }
    println()
  }
}
// \end{scala}


// \begin{scala}
// import ox.cads.testing._

// object StackTest{
//   var iters = 200      // # iterations by each worker
//   var pushProb = 0.3 // probability of each operation being a push
//   var maxValue = 20 // max value added to the stack
//   var reps = 1000     // # runs

//   // Type of sequential specification objects.
//   type S = ImmutableStack[Int]

//   // Type of concurrent object to be tested.
//   type C = ServerStack[Int]

//   // Sequential push operation
//   def seqPush(x: Int)(stack: S): (Unit, S) = ((), stack.push(x))

//   // Sequential pop operation
//   def seqPop(stack: S): (Option[Int], S) =
//     if(stack.isEmpty) (None, stack)
//     else{ val(x, stack1) = stack.pop2; (Some(x), stack1) }

//   // A worker thread
//   def worker(me: Int, log: GenericThreadLog[S,C]) = {
//     val random = new scala.util.Random
//     for(i <- 0 until iters)
//       if(random.nextFloat <= pushProb){
//         val x = random.nextInt(maxValue)
//         log.log(_.push(x), "push("+x+")", seqPush(x))
//       }
//       else log.log(_.pop, "pop", seqPop)
//   }

//   // The main method
//   def main(args: Array[String]) = {
//     for(i <- 0 until reps){
//       val concStack = new ServerStack[Int]; val seqStack = new ImmutableStack[Int]
//       // The tester: Add the parameter tsLog = false on Windows.
//       val tester = LinearizabilityTester.JITGraph[S, C](seqStack, concStack, 4, worker _, iters)
//       assert(tester() > 0)
//       concStack.shutdown
//       if(i%10 == 0) print(".")
//     }
//     println()
//   }
// }

