package tacp.alternation

import ox.scl._

/** A buffer. */
object Buffer{
  /** A buffer, that receives data on `in`, and outputs it on `out`. */
  def buffer[A](in: ??[A], out: !![A]) = thread("buffer"){
    val q = new scala.collection.mutable.Queue[A]
    serve(
      in =?=> { x => q.enqueue(x) }
      | q.nonEmpty && out =!=> { q.dequeue() }
    )
    out.endOfStream()
  }
}

/** A simple test of Buffer. */
object BufferTest{
  /** The sender sends values [0..N); the recevier checks these are received as
    * expected. */
  val N = 20

  val in, out = new SyncChan[Int]

  def sender = thread("sender"){
    for(i <- 0 until N) in!i
    in.close()
  }

  def receiver = thread("receiver"){
    var i = 0
    repeat{
      val x = out?(); assert(x == i, s"$i $x"); i += 1
    }
    assert(i == N, s"$i $N")
  }

  def system = sender || receiver || Buffer.buffer(in, out)

  def doTest = { run(system); in.reopen(); out.reopen() }

  def main(args: Array[String]) = {
    for(i <- 0 until 100){ doTest; if(i%10 == 0) print(".") }
    println()
  }


}
