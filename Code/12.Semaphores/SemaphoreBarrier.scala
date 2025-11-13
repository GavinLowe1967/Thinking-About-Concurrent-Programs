package tacp.semaphores

import ox.scl._

class SemaphoreBarrier(n: Int) extends tacp.dataParallel.BarrierT{
  require(n > 1)
  /** Number of threads currently waiting. */
  private var waiting = 0 
  /** Semaphore to signal to waiting threads. */
  private val waitSem = new SignallingSemaphore()
  /** Semaphore for mutual exclusion. */
  private val mutex = new MutexSemaphore()

  def sync(me: Int) = {
    mutex.down()
    if(waiting == n-1) waitSem.up()
    else{ 
      waiting += 1; mutex.up(); waitSem.down() // Wait until woken.
      waiting -= 1
      if(waiting==0) mutex.up() else waitSem.up() // Pass the baton.
    }
  }
}
    
