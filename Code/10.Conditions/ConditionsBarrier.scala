package tacp.monitors

import ox.scl._
import tacp.dataParallel.BarrierT

class ConditionsBarrier(n: Int) extends BarrierT{
  /** The number of threads currently waiting. */
  private var count = 0

  /** Are we in the leaving phase of a synchronisation? */
  private var leaving = false

  /** Lock for mutual exclusion. */
  private val lock = new Lock

  /** Condition to indicate that the synchronisation has happened and threads
    * should continue. */
  private val continue = lock.newCondition

  /** Condition to indicate that the previous synchronisation has finished, and
    * threads can enter the main part of the synchronisation. */
  private val enter = lock.newCondition

  def sync(me: Int) = lock.mutex{
    enter.await(!leaving) // Wait for previous synchronisation to end. (1)
    if(count == n-1){      // Start leaving phase
      leaving = true; continue.signalAll() // Signal to threads at (2).
    }
    else{      // Have to wait
      count += 1; continue.await() // Wait for the others. (2)
      assert(leaving); count -= 1
    }
    if(count == 0){      // Start entering phase.
      leaving = false; enter.signalAll() // Signal to threads at (1).
    }
  }
}
