package tacp.datatypes

import ox.scl._

/** An implementation of a Map, using a linked list. */
class LinkedListMap[K,V] (implicit ord: K => Ordered[K]) extends Map[K,V]{
  /** Nodes from which the linked list is constructed.
    * Any access to the next field must be done while holding the lock on this 
    * node.  */
  private class Node(val key: K, var value: V, var next: Node){
    /** A lock used to protect the next field. */
    private val l = new Lock

    /** Lock this node. */
    def lock() = l.acquire()

    /** Unlock this node. */
    def unlock() = l.release()
  }

  /** A dummy header node. */
  private val header = new Node(null.asInstanceOf[K], null.asInstanceOf[V], null)

  /* DTI: the nodes from header.next onwards are in strictly increasing order of
   * key fields. */ 

  /** Find the first node p that such that p.next = null or p.next.key >= k.
    * p will be locked on return. */  
  private def find(k: K): Node = {
    var p = header; p.lock()
    // Invariant: all nodes n before p have n.next.key < k; p is locked.
    while(p.next != null && p.next.key < k){
      val n = p.next; n.lock(); p.unlock(); p = n
    }
    p
  }

  /** Optionally get the value associated with k. */
  def get(k: K): Option[V] = {
    val p = find(k); val n = p.next
    if(n != null && n.key == k){ 
      n.lock(); p.unlock(); val res = n.value; n.unlock(); Some(res) 
    }
    else{ p.unlock(); None }
  }

  /** Add the association k -> v.  Optionally return the value previously
    * associated with k. */
  def put(k: K, v: V): Option[V] = {
    val p = find(k); val n = p.next
    if(n != null && n.key == k){
      n.lock(); p.unlock(); val res = n.value; n.value = v; n.unlock(); Some(res)
    }
    else{ p.next = new Node(k, v, n); p.unlock(); None }
  }

  /** Remove any association for k.  Optionally return the value previously
    * associated with k. */
  def remove(k: K): Option[V] = {
    val p = find(k); val n = p.next
    if(n != null && n.key == k){
      n.lock(); p.next = n.next; p.unlock(); Some(n.value)
    }
    else{ p.unlock(); None }
  }
}
