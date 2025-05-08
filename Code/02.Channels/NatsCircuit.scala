package tacp.channels

import ox.scl._ 

/** A fine-grained concurrent network that prints the natural numbers. */
object NatsCircuit{
  /** Repeatedly input on `in`, printing the values received. */
  def console[A](in: ??[A]) = thread{ while(true) println(in?()) }

  /** Copy from `in` to `out`, prefixing with `x`. */
  def prefix[A](x: A, in: ??[A], out: !![A]) = thread{
    out!x; repeat{ out!in?() }
  }

  /** Copy values from `in` to both `out1` and `out2`. */
  def tee[A](in: ??[A], out1: !![A], out2: !![A]) = thread{
    repeat{ val x = in?(); out1!x; out2!x }
  }

  /** Apply `f` to all values received on `in`, and output on `out`. */
  def map[A,B](f: A => B, in: ??[A], out: !![B]) = thread{
    repeat{ out!(f(in?())) }
  }

  /** The network, outputting on out. */
  def nats(out: !![Int]): ThreadGroup = {
    val nats, succs, back = new SyncChan[Int]
    prefix(0, succs, nats) || tee(nats, out, back) || 
      map(((x: Int) => x+1), back, succs)
  }

  def system: ThreadGroup = {
    val c = new SyncChan[Int]; nats(c) || console(c)
  }

  def main(args: Array[String]) = run(system)
}
 
