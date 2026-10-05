package tacp.channels
import ox.scl._

object Primes1{
  /** Component that passes values from `in` to `out`, removing multiples of
    * `n`. */
  private def filter(in: ??[Int], out: !![Int], k: Int) = thread{
    repeat{ val m = in?(); if(m%k != 0) out!m }
    out.endOfStream()
  }

  /** Component that sieves values received on `in`, removing multiples of
    * earlier values, but passing remainder on `out`. */
  private def sieve(in: ??[Int], out: !![Int]): ThreadGroup = thread{
    attempt{
      val k = in?(); out!k; val mid = new SyncChan[Int]
      run(filter(in, mid, k) || sieve(mid, out))
    }{ out.endOfStream() }
  }

  /** Component that sends values `2 to n` on `c`. */
  private def generator(c: !![Int], n: Int) = thread{
    for(i <- 2 to n) c!i
    c.endOfStream()
  }

  /** Network to generate the primes up to `n`, and send them on `out`. */
  private def primes(out: !![Int], n: Int): ThreadGroup = {
    val in = new SyncChan[Int]
    generator(in, n) || sieve(in, out) 
  }

  /** Component that prints values received on `in`. */
  private def console(in: ??[Int]) = thread{ repeat{ println(in?()) } } 

  def system = {
    val out = new SyncChan[Int]; primes(out, 1000) || console(out)
  }

  def main(args: Array[String]) = {
    run(system)
  }

}
