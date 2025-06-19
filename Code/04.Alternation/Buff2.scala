package tacp.alternation

import ox.scl._

/** Various implementations of two-place buffers. */ 
object Buff2{

  /** Two place buffer. */
  def buff2[T](in: ??[T], out: !![T]): ThreadGroup = {
    def empty() = { val x = in?(); full(x) }
    def full(x: T): Unit = {
      alt( out =!=> { x } ==> { empty() } | in =?=> { y => out!x; full(y) } )
    }
    thread{ attempt{empty()}{} }
  }

  // /** Two place buffer. */
  // def buff2[T](in: ??[T], out: !![T]): Unit = {
  //   val x = in?(); buff2A(in, out, x)
  // }
  // /** Two place buffer holding x. */
  // def buff2A[T](in: ??[T], out: !![T], x: T): Unit = {
  //   alt(
  //     out =!=> { x } ==> { buff2(in, out) }
  //     | in =?=> { y => out!x; buff2A(in, out, y) }
  //   )
  // }  

  /** Two place buffer, using alternation. */
  def buff2Alt[T](in: ??[T], out: !![T]) = thread{
    var x: T = null.asInstanceOf[T]  // Contents, possibly invalid.
    var empty = true // Is the buffer empty?
    serve(
      !empty && out =!=> { empty = true; x }
      | empty && in =?=> { v => x = v; empty = false }
      | !empty && in =?=> { v => out!x; x = v }
    )
  }

  /** One-place buffer. */
  def buff1[T](in: ??[T], out: !![T]) = thread{
    var x: T = null.asInstanceOf[T]  // Contents, when empty = false.
    var empty = true // Is the buffer empty?
    serve(
      !empty && out =!=> { empty = true; x }
      | empty && in =?=> { v => x = v; empty = false }
    )
  }

}

// ==================================================================

/** Test the implementations of Buff2, using linearizability testing. */
object Buff2Test{
  var iters = 50  // Number of iterations by each worker
  val MaxVal = 20 // Maximum value sent
 
  // Which instance to run?  0 = buff2, 1 = buff2Alt, 2 = buff1
  var instance = 0

  var buffered = false

  /** Build a concurrent object around an element of Buff2. */
  class Buffer {
    val in = if(buffered) new BuffChan[Int](1) else  new SyncChan[Int]  
    val out = if(buffered) new BuffChan[Int](1) else new SyncChan[Int]

    def send(x: Int) = in!x

    def receive: Int = out?()

    fork(if(instance == 0) Buff2.buff2(in,out) 
      else if(instance == 1) Buff2.buff2Alt(in, out) 
      else Buff2.buff1(in, out) 
    )
    // else thread("Buffer"){ 
    //   // if(instance == 0) attempt{ Buff2.buff2(in,out) }{}
    //   // else
    //   //   if(instance == 1) Buff2.buff2Alt(in, out)
    //   // else
    //     Buff2.buff1(in, out)
    // }.fork

    def shutdown = { in.close(); out.close() }
  }

  type SeqBuff = scala.collection.immutable.Queue[Int]
  type ConcBuff = Buffer

  /* The capacity of the buffer and channels. */
  def capacity = (if(instance == 2) 1 else 2)+(if(buffered) 2 else 0)

  /* Operations on the specification. */
  def seqSend(x: Int)(q: SeqBuff) : (Unit, SeqBuff) = {
    require(q.length < capacity); ((), q.enqueue(x))
  }
  def seqReceive(q: SeqBuff) : (Int, SeqBuff) = {
    require(q.nonEmpty); q.dequeue
  }

  /** A worker for the LinTesters */
  def worker(me: Int, log: LinearizabilityLog[SeqBuff, ConcBuff]) = {
    val random = new scala.util.Random(scala.util.Random.nextInt()+me*45207)
    for(i <- 0 until iters){
      if(me%2 == 0){
        val x = i; log(_.send(x), "send("+x+")", seqSend(x))
      }
      else log(_.receive, "receive", seqReceive)
    }
  }

  def main(args: Array[String]) = {
    // parse arguments
    var i = 0; val p = 2      // Number of workers 
    var reps = 10000   // Number of repetitions
    while(i < args.length) args(i) match{
      case "--iters" => iters = args(i+1).toInt; i += 2 
      case "--reps" => reps = args(i+1).toInt; i += 2 
      case "--alternative" => instance = 1; i += 1
      case "--onePlace" => instance = 2; i += 1
      case "--buffered" => buffered = true; i += 1
      // case "--size" => size = args(i+1).toInt; i += 2
      case arg => println("Unrecognised argument: "+arg); sys.exit()
    }

    for(r <- 0 until reps){
      val concBuff = new Buffer
      val seqBuff = scala.collection.immutable.Queue[Int]()
      val tester = LinearizabilityTester[SeqBuff,ConcBuff](
        seqBuff, concBuff, p, worker _)
      assert(tester() > 0)
      concBuff.shutdown
      if(r%100 == 0) print(".")
    } // end of for loop
    println()
  }

}
