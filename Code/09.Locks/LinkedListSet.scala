package tacp.datatypes

import ox.scl._

class LinkedListSet[A](implicit ev: A => Ordered[A]) extends Set[A]{
  /** Nodes from which the linked list is constructed. */
  private class Node(val datum: A, var next: Node){
    private val l = new Lock

    def lock() = l.acquire()

    def unlock() = l.release()
  }

  // private def lock(n: Node) = if(n != null) n.lock()

  // private def unlock(n: Node) = if(n != null) n.unlock()

  /** A dummy header node. */
  private var header = new Node(null.asInstanceOf[A], null)

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

  def contains(x: A): Boolean = {
    val p = find(x); val n = p.next; p.unlock()
    n != null && n.datum == x
  }

  def add(x: A): Boolean = {
    val p = find(x); val n = p.next
    if(n == null || n.datum > x){ 
      p.next = new Node(x, n); p.unlock(); true 
    }
    else{ assert(n.datum == x); p.unlock(); false }
  }

  def remove(x: A): Boolean = {
    val p = find(x); val n = p.next
    if(n != null && n.datum == x){
      n.lock(); p.next = n.next; p.unlock(); true
    }
    else{ p.unlock(); false }
  }

}
