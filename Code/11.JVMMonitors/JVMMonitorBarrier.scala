package tacp.jvmMonitors

import tacp.dataParallel.BarrierT

class JVMMonitorBarrier(n: Int) extends BarrierT{
  /** The number of threads currently waiting. */
  private var count = 0

  /** Are we in the leaving phase of a synchronisation? */
  private var leaving = false

  def sync(me: Int) = synchronized{
    while(leaving) wait() // Wait for previous synchronisation to end (1).
    if(count == n-1){      // Start leaving phase
      leaving = true; notifyAll() // Signal to threads at (2).
    }
    else{      // Have to wait
      count += 1
      while(!leaving) wait() // Wait for the others (2).
      count -= 1
    }
    if(count == 0){      // Start entering phase.
      leaving = false; notifyAll() // Signal to threads at (1).
    }
  }
} 
