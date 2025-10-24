package tacp.jvmMonitors

/** An implementation using striped locking. */
class StripedCounterArray(n: Int, stripes: Int) 
    extends tacp.locks.CounterArray(n){
  require(stripes > 0)

  /** Locks.  locks(j) protects all a(i) such that i%stripes == j. */
  private val locks = Array.fill(stripes)(new AnyRef)

  def inc(i: Int) = locks(i%stripes).synchronized{ a(i) += 1 }

  def dec(i: Int) = locks(i%stripes).synchronized{ a(i) -= 1 }

  def get(i: Int) = locks(i%stripes).synchronized{ a(i) }
}
 
