package tacp.semaphores

import ox.scl._
import tacp.jvmMonitors.Mod3

class Mod3S extends Mod3{
  /** sum of identites of threads currently in critical region. */
  private var current = 0

  /** Number of threads waiting. */
  private var waiting = 0

  /** Semaphore for blocked threads to wait on. 
    * A signal implies current%3 = 0. */ 
  private val entry = new SignallingSemaphore

  /** Semaphore for mutual exclusion. */
  private val mutex = new MutexSemaphore
    
  /** Pass the baton, either signalling for another thread to enter, or lifting
    * the mutex. */
  private def signal() = 
    if(current%3 == 0 && waiting > 0) entry.up()
    else mutex.up()

  def enter(id: Int) = {
    mutex.down()
    if(current%3 != 0){
      waiting += 1; mutex.up(); entry.down() // Wait for a signal.
      assert(current%3 == 0); waiting -= 1
    }
    current += id
    signal()
  }

  def exit(id: Int) = {
    mutex.down()
    current -= id
    signal()
  }
}

// =======================================================

class Mod3SA extends Mod3{
  private val entry = new MutexSemaphore
  // Invariant: entry is up iff the current sum of identities is divisible by 3. 

  def enter(id: Int) = {
    entry.down()
    // Current sum is divisible by 3, so this thread can enter.
    if(id%3 == 0) entry.up() // Preserves invariant.
  }

  def exit(id: Int) = if(id%3 != 0) entry.up()
  // Note: the current threads can include at most one whose identity is not
  // divisible by 3.  Hence, when id%3 != 0 above, when this thread leaves,
  // the sum is divisible by 3 again, so the entry.up() preserves the
  // invariant.  If id%3 == 0, then the invariant is preserved trivially. 
}
