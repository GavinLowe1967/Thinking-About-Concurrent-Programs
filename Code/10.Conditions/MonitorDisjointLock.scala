package tacp.monitors

import ox.scl._

/** A disjoint usage lock, implemented using an SCL monitor. */
class MonitorDisjointLock extends DisjointLock{
  /** The number of threads of the two types using the resource.  Invariant: 
    * aIn = 0 or bIn = 0. */
  private var aIn, bIn = 0

  /** The number of threads of the two types waiting. */
  private var aWaiting, bWaiting = 0

  private val lock = new Lock

  /** Conditions for signalling to the two types of threads.  A signal on
    * aSignal indicates bIn = 0, and similarly with bSignal. */
  private val aSignal, bSignal = lock.newCondition

  def aEnter() = lock.mutex{
    if(bIn > 0 || bWaiting > 0){ // Thread has to wait.
      aWaiting += 1
      aSignal.await(bIn == 0) // Wait for B threads to leave (1).
      aWaiting -= 1
    }
    aIn += 1
  }

  def aExit() = lock.mutex{ 
    aIn -= 1
    if(aIn == 0) bSignal.signalAll() // Signal to B threads at (2).
  }

  def bEnter() = lock.mutex{
    if(aIn > 0 || aWaiting > 0){ // Thread has to wait.
      bWaiting += 1
      bSignal.await(aIn == 0) // Wait for A threads to leave (2).
      bWaiting -= 1
    }
    bIn += 1
  }

  def bExit() = lock.mutex{
    bIn -= 1
    if(bIn == 0) aSignal.signalAll() // Signal to A threads at (1).
  }

  /* Note: it is necessary for a thread to re-check the relevant condition when
   * it receives a signal.  Consider the following scenario.
   * 1. An A-thread is using the resource; two B-threads are waiting at (2).
   * 2. The A-thread exists and signals to both B-threads.
   * 3. One B-thread is scheduled and enters the critical section.
   * 4. An A-thread calls aEnter and waits at (1).
   * 5. The first B-thread exits and signals to the A-thread.
   * 6. The A-thread enters.
   * 7. The second B-thread is scheduled.
   */
}
