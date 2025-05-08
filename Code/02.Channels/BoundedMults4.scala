package tacp.channels

import ox.scl._ 

/** Program to illustrate channel communications.  The effect is to print
  * multiples of four. */
object BoundedMults4{
  /** Send the naturals on out. */
  def nats(max: Int, out: !![Int]) = thread{ 
    var n = 0
    repeat{ out!n; n += 1 }
  }

  /** Repeatedly input on in, sending alternate elements on out. */
  def alts[A](in: ??[A], out: !![A]) = thread{ 
    repeat{ out!(in?()); in?() }
    in.close(); out.endOfStream()
  }

  /** Repeatedly input on `in`, printing the values received. */
  def console[A](in: ??[A]) = thread{ repeat{ println(in?()) } }

  /** Copy values from `in` to `out` while they satisfy the predicate `p`. */
  def takeWhile[A](p: A => Boolean, in: ??[A], out: !![A]) = thread{
    var done = false
    repeat(!done){ 
      val x = in?(); if(p(x)) out!x else done = true
    }
    in.close(); out.endOfStream()
  }

  /** Complete system. */
  def system(max: Int) = {
    // Each channel xk passes multiples of k. 
    val x1, x2, x4, out = new SyncChan[Int]
    nats(max, x1) || alts(x1, x2) || alts(x2, x4) || 
      takeWhile((x: Int) => x <= max, x4, out) || console(out)
  }

  def main(args: Array[String]) = {
    val max = args(0).toInt; run(system(max))
  }
}

