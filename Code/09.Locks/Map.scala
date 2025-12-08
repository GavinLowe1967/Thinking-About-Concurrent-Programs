package tacp.datatypes

import ox.scl._

trait Map[K,V]{
  /** Optionally get the value associated with k. */
  def get(k: K): Option[V] 

  /** Add the association k -> v.  Optionally return the value previously
    * associated with k. */
  def put(k: K, v: V): Option[V] 

  /** Remove any association for k.  Optionally return the value previously
    * associated with k. */
  def remove(k: K): Option[V] 
}
