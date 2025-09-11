package tacp.dataParallel

import ox.scl._

/** A barrier synchronisation object, where each synchronisation takes 
  * O(log n) time. */
class BarrierLog(n: Int) extends BarrierT{
  /** Channels by which a thread signals to its parent that it is ready.
    * Indexed by the child's identity. */ 
  private val ready = Array.fill(n)(new SyncChan[Unit])

  /** Channels by which a thread signals to its children that it can
    * continue.  Indexed by the child's (receiver's) identity. */ 
  private val go = Array.fill(n)(new SyncChan[Unit])

  /** Barrier protocol for node with identity me, in a system of n nodes. */
  def sync(me: Int) = {
    val child1 = 2*me+1; val child2 = 2*me+2
    // Wait for ready signals from both children
    if(child1 < n) ready(child1)?()
    if(child2 < n) ready(child2)?()
    // Send ready signal to parent, and wait for go reply, 
    // unless this is the root
    if(me != 0){ ready(me)!(); go(me)?() }
    // Send go signals to children
    if(child1 < n) go(child1)!()
    if(child2 < n) go(child2)!()
  }
}

// =======================================================

/** A barrier synchronisation object, where each synchronisation takes 
  * O(log n) time. */
class CombiningBarrierLog[A](n: Int, f: (A,A) => A) 
    extends CombiningBarrierT[A](n,f){
  /** Channels by which a thread signals to its parent that it is ready.
    * Indexed by the child's identity. */ 
  private val ready = Array.fill(n)(new SyncChan[A])

  /** Channels by which a thread signals to its children that it can
    * continue.  Indexed by the child's (receiver's) identity. */ 
  private val go = Array.fill(n)(new SyncChan[A])

  /** Barrier protocol for node with identity me, in a system of n nodes. */
  def sync(me: Int, x: A): A = {
    val child1 = 2*me+1; val child2 = 2*me+2; var y = x
    // Wait for ready signals from both children
    if(child1 < n){ val x1 = ready(child1)?(); y = f(x1,y) }
    if(child2 < n){ val x2 = ready(child2)?(); y = f(x2,y) }
    // Send ready signal to parent, and wait for go reply, 
    // unless this is the root
    if(me != 0){ ready(me)!y; y = go(me)?() }
    // Send go signals to children
    if(child1 < n) go(child1)!y
    if(child2 < n) go(child2)!y
    y
  }
}
