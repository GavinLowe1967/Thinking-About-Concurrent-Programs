package tacp.alternation

import ox.scl._
import scala.util.Random

/** Simulation of the Dining Philosophers example. */
object PhilsLog{
  val N = 5 // Number of philosophers

  // Simulate basic actions
  def eat() = Thread.sleep(500)
  def think() = Thread.sleep(Random.nextInt(900))
  def pause() = Thread.sleep(500)

  // Each philosopher will send "pick" and "drop" commands to her forks, which
  // we simulate using the following values.
  type Cmd = Boolean
  val Pick = true; val Drop = false

  val log = new Log[String](N)
 
  /** A single philosopher. */
  def phil(me: Int, left: !![Cmd], right: !![Cmd]) = thread(s"Phil $me"){
    repeat{
      think()
      log.add(me, s"$me sits"); pause()
      left!Pick; log.add(me, s"$me picks up left fork"); pause()
      right!Pick; log.add(me, s"$me picks up right fork"); pause()
      log.add(me, s"$me eats"); eat()
      left!Drop; pause(); right!Drop; pause()
      log.add(me, s"$me leaves")
      if(me == 0) print(".")
    }
  }

  /** A single fork. */
  def fork(me: Int, left: ??[Cmd], right: ??[Cmd]) = thread("Fork"+me){
    serve(
      left =?=> {
        x => assert(x == Pick); val y = left?(); assert(y == Drop)
      }
      |
      right =?=> {
        x => assert(x == Pick); val y = right?(); assert(y == Drop)
      }
    )
  }

  /** The complete system. */
  val system = {
    // Channels to pick up and drop the forks:
    val philToLeftFork, philToRightFork = Array.fill(N)(new SyncChan[Cmd])
    // philToLeftFork(i) is from Phil(i) to Fork(i);
    // philToRightFork(i) is from Phil(i) to Fork((i-1)%N).
    val allPhils = || ( 
      for (i <- 0 until N)
      yield phil(i, philToLeftFork(i), philToRightFork(i))
    )
    val allForks = || ( 
      for (i <- 0 until N) yield
        fork(i, philToRightFork((i+1)%N), philToLeftFork(i))
    )
    allPhils || allForks
  }

  /** Run the system. */
  def main(args : Array[String]) = {
    log.writeToFileOnShutdown("philsLog.txt")
    run(system)
  }
}

  
