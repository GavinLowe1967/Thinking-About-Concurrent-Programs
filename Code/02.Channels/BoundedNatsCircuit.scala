package tacp.channels

import ox.scl._ 

/** A fine-grained concurrent network that prints the natural numbers up to a
  * limit provided on the command line. */
object BoundedNatsCircuit{
  /** Repeatedly input on `in`, printing the values received. */
  def console[A](in: ??[A]) = thread{ repeat{ println(in?()) } }

  /** Copy from `in` to `out`, prefixing with `x`. */
  def prefix[A](x: A, in: ??[A], out: !![A]) = thread{
    out!x; repeat{ out!in?() }; out.endOfStream()
  }

  /** Copy values from `in` to both `out1` and `out2`. */
  def tee[A](in: ??[A], out1: !![A], out2: !![A]) = thread{
    repeat{ val x = in?(); out1!x; out2!x }
    out1.endOfStream(); out2.endOfStream()
  }

  /** Apply `f` to all values received on `in`, and output on `out`. */
  def map[A,B](f: A => B, in: ??[A], out: !![B]) = thread{
    repeat{ out!(f(in?())) }; out.endOfStream()
  }

  def takeWhile[A](p: A => Boolean, in: ??[A], out: !![A]) = thread{
    var done = false
    repeat(!done){ 
      val x = in?(); if(p(x)) out!x else done = true
    }
    out.endOfStream()
  }

  /** The network. */
  def nats(max: Int, out: !![Int]): ThreadGroup = {
    val nats, nats1, succs, back = new SyncChan[Int]
    prefix(0, succs, nats) || takeWhile((x: Int) => x <= max, nats, nats1) ||
      tee(nats1, out, back) || map((x: Int) => x+1, back, succs)
  }

  def system(max: Int) = {
    val c = new SyncChan[Int]; nats(max, c) || console(c)
  }

  def main(args: Array[String]) = {
    val max = args(0).toInt; run(system(max))
  }
}
