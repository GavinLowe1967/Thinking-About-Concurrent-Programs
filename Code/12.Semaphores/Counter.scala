package tacp.semaphores

import ox.scl._

object Counter{
  private var x = 0
  
  private var mutex = new MutexSemaphore
    
  def inc() = { mutex.down(); x = x+1; mutex.up() }
  def dec() = { mutex.down(); x = x-1; mutex.up() }
  def get = { mutex.down(); val result = x; mutex.up(); result }
}
