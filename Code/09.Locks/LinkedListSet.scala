package tacp.datatypes

import ox.scl._

class LinkedListSet[A](implicit ord: A => Ordered[A]) extends Set[A]{
  /** Nodes from which the linked list is constructed.
    * Any access to the next field must be done while holding the lock on this 
    * node.  */
  private class Node(val datum: A, var next: Node){
    /** A lock used to protect the next field. */
    private val l = new Lock

    /** Lock this node. */
    def lock() = l.acquire()

    /** Unlock this node. */
    def unlock() = l.release()
  }

  /** A dummy header node. */
  private val header = new Node(null.asInstanceOf[A], null)

  /* DTI: the nodes from header.next onwards are in strictly increasing order of
   * datum fields. */ 

  /** Find the first node p that such that p.next = null or p.next.datum >= x.
    * p will be locked on return. */  
  private def find(x: A): Node = {
    var p = header; p.lock()
    // Invariant: all nodes n before p have n.next.datum < x; p is locked.
    while(p.next != null && p.next.datum < x){
      val n = p.next; n.lock(); p.unlock(); p = n
    }
    p
  }

  /** Does this contain x? */
  def contains(x: A): Boolean = {
    val p = find(x); val n = p.next; p.unlock()
    n != null && n.datum == x
  }

  /** Add x to this.  Return true if x was not already in the set. */
  def add(x: A): Boolean = {
    val p = find(x); val n = p.next
    if(n == null || n.datum > x){ 
      p.next = new Node(x, n); p.unlock(); true 
    }
    else{ assert(n.datum == x); p.unlock(); false }
  }

  /** Remove x from this.  Return true if x was previously in the set. */
  def remove(x: A): Boolean = {
    val p = find(x); val n = p.next
    if(n != null && n.datum == x){
      n.lock(); p.next = n.next; p.unlock(); true
    }
    else{ p.unlock(); false }
  }
}
