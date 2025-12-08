package tacp.tests

import ox.scl._

import tacp.datatypes.{TotalStack,ServerStack}

object TotalStackTest{
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
  type C = TotalStack[Int]

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

