package tacp.monitors

import ox.scl._

class MonitorDisjointLock extends DisjointLock{
  /** The number of threads of the two types using the resource.  Invariant: 
    * aIn = 0 or bIn = 0. */
  private var aIn, bIn = 0

  /** The number of threads of the two types waiting. */
  private var aWaiting, bWaiting = 0

  /** The number of threads of the two types that have just received a signal
    * and are about to enter. */
  private var aToEnter, bToEnter = 0

  private val lock = new Lock

  /** Conditions for signalling to the two types of threads.  A signal on
    * aSignal indicates bIn = 0, and similarly with bSignal. */
  private val aSignal, bSignal = lock.newCondition

  def aEnter() = lock.mutex{
    if(bIn > 0 || bWaiting > 0 || bToEnter > 0){ // Thread has to wait.
      aWaiting += 1; aSignal.await() // Wait for B threads to leave (1).
      assert(bIn == 0 && aToEnter > 0); aToEnter -= 1
    }
    aIn += 1
  }

  def aExit() = lock.mutex{ 
    aIn -= 1
    if(aIn == 0 && aToEnter == 0 && bWaiting > 0){
      assert(bToEnter == 0); bToEnter = bWaiting; bWaiting = 0
      bSignal.signalAll() // Signal to B threads at (2).
    }
  }

  def bEnter() = lock.mutex{
    if(aIn > 0 || aWaiting > 0 || aToEnter > 0){ // Thread has to wait.
      bWaiting += 1 ; bSignal.await() // Wait for A threads to leave (2).
      assert(aIn == 0 && bToEnter > 0); bToEnter -= 1
    }
    bIn += 1
  }

  def bExit() = lock.mutex{
    bIn -= 1
    if(bIn == 0 && bToEnter == 0 && aWaiting > 0){ // Last to leave
      assert(aToEnter == 0); aToEnter = aWaiting; aWaiting = 0
      aSignal.signalAll() // Signal to A threads at (1).
    }
  }
}
