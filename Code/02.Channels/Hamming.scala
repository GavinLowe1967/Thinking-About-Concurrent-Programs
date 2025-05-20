package tacp.channels
import ox.scl._

object Hamming{
  /** Tee components, giving two outputs. */
  def tee[A](in: ??[A], out1: !![A], out2: !![A]) = thread("Tee"){
    repeat{ val v = in?(); out1!v; out2!v }
    // in.close(); 
    out1.endOfStream(); out2.endOfStream()
  }

  /** Merge two infinite ascending streams. */
  def merge(left: ??[Int], right: ??[Int], out: !![Int]) = thread("Merge"){
    // Invariant: l is last value read from left; r is last value read from
    // right
    var l = left?(); var r = right?()
    repeat{
      if(l < r){ out!l; l = left?() }
      else if(l == r){ out!l; l = left?(); r = right?() }
      else{ out!r; r = right?() }
    }
    left.close(); right.close(); out.endOfStream()
  }

  /** Copy from `in` to `out`, prefixing with `x`. */
  def prefix[A](x: A, in: ??[A], out: !![A]) = thread("Prefix"){ 
    out!x; repeat{ out!(in?()) }
    // in.close(); 
    out.endOfStream()
  }

  /** Map function f over input stream. */
  def map[A,B] (f: A => B, in: ??[A], out: !![B]) = thread("Map"){
    repeat{ out!(f(in?())) }
    // in.close(); 
    out.endOfStream()
  }

  def takeWhile[A](p: A => Boolean, in: ??[A], out: !![A]) = thread("TakeWhile"){
    var done = false
    repeat(!done){ 
      val x = in?(); if(p(x)) out!x else done = true
    }
    in.close(); out.endOfStream()
  }

  /** Put the system together. */
  def system(out: !![Int], max: Int) = {
    val h0, h1, h2 = new SyncChan[Int]     // Inputs into tee components.
    val i2, i3, i5 = new SyncChan[Int]     // Inputs into *2, *3, *5.
    val t2, t3, t5 = new UnboundedBuffChan[Int] // Outputs from *2, *3, *5.
    val m1, m2, m3 = new SyncChan[Int]    // Outputs from merges and takeWhile.
    prefix(1, m3, h0) || 
    tee(h0, i2, h1) || tee(h1, i3, h2) || tee(h2, i5, out) ||
    map((x:Int) => 2*x, i2, t2) || map((x:Int) => 3*x, i3, t3) ||
    map((x:Int) => 5*x, i5, t5) ||  
    merge(t2, t3, m1) || merge(t5, m1, m2) || 
    takeWhile((x: Int) => x <= max, m2, m3)
  }

  /** Print values from `report` onto the console, together with their index. */
  def console(c: ??[Int]) = thread{
    var i = 1
    repeat{ println(s"$i: "+(c?())); i += 1 }
  }

  def main(args: Array[String]) = { 
    var max = 1000; var i = 0
    while(i < args.length) args(i) match{
      case "--max" => max = args(i+1).toInt; i += 2
    }

    val out = new SyncChan[Int] 
    // Take amount of buffering from command line; default 0
    // val buf = if(args.length > 0){Integer.valueOf(args(0)).intValue()} else 0
    run(system(out, max) || console(out))
  }

}

/** 

With no buffering, this deadlocks after outputting 1, 2, 3, 4, 5, 6.  (The
precise place of deadlock may vary, depending upon the order in which the Tees
do their outputs, and the order in which the Merges do their inputs.)

- Tee0 is waiting to output 6 on h1

- Prefix is waiting to output 8 on h0

- Merge2 is waiting to output 9 on m2

- map (*5) is waiting to output 15 on t5

- Merge1 is waiting to output 10 on m1

- map (*3) is waiting to input on h4 (its last output was 12)

- map (*2) is waiting to input on h2 (its last output was 10)

- Tee2 is waiting to output 4 on h5

- Tee1 is wating to output 5 on h3

With buffering of at least 224, we find the 1000th Hamming number is 51200000.


*/
