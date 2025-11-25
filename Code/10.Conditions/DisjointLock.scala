package tacp.monitors

import ox.scl._

/* There are two types of threads, type A and type B, that share a resource.
 * Multiple threads of the same type may use the resource simultaneously; but
 * threads of different types may not. */ 

/** Trait for a disjoint usage lock. */
trait DisjointLock{
  /** An A-thread enters the critical section. */
  def aEnter(): Unit
  /** An A-thread leaves the critical section. */
  def aExit(): Unit
  /** A B-thread enters the critical section. */
  def bEnter(): Unit
  /** A B-thread leaves the critical section. */
  def bExit(): Unit
}


