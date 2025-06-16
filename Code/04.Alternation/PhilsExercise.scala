package tacp.alternation

import ox.scl._
import scala.util.Random

/** Object encapulating common code between the exercises. */
object PhilsExercise{
  val N = 5 // Number of philosophers

  // Simulate basic actions
  def eat() = Thread.sleep(500)
  // Remove randomness in thinking, to get philosophers more in sync, and make
  // deadlocks more likely.
  def think() = Thread.sleep(Random.nextInt(900))
  def pause() = Thread.sleep(500)

  // Each philosopher will send "pick" and "drop" commands to their forks,
  // which we simulate using the following values.
  type Cmd = Boolean
  val Pick = true; val Drop = false
}

// =======================================================

import PhilsExercise._

/** Simulation of the Dining Philosophers example, with one right-handed
  * Philosopher. */
object PhilsLeft{
  /** A single philosopher. */
  private 
  def phil(me: Int, left: !![Cmd], right: !![Cmd]) = thread("Phil"+me){
    repeat{
      think()
      println(s"$me sits"); pause()
      if(me == 0){
        right!Pick; println(s"$me picks up right fork"); pause()
        left!Pick; println(s"$me picks up left fork"); pause()
      }
      else{
        left!Pick; println(s"$me picks up left fork"); pause()
        right!Pick; println(s"$me picks up right fork"); pause()
      }
      println(s"$me eats"); eat()
      left!Drop; pause(); right!Drop; pause()
      println(s"$me leaves")
    }
  }

  /** A single fork. */
  private 
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
  private def system = {
    // Channels to pick up and drop the forks:
    val philToLeftFork, philToRightFork = Array.fill(5)(new SyncChan[Cmd])
    // philToLeftFork(i) is from phil(i) to fork(i);
    // philToRightFork(i) is from phil(i) to fork((i-1)%N)
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
  def main(args : Array[String]) = run(system) 
}

// =======================================================  

/** Simulation of the Dining Philosophers example, using a butler thread. */
object PhilsButler{
  /** Channels whereby a philosopher sits or leaves, as allowed by the
    * butler. */
  private val sit, leave = new SyncChan[Unit]
 
  /** A single philosopher. */
  private 
  def phil(me: Int, left: !![Cmd], right: !![Cmd]) = thread("Phil"+me){
    repeat{
      think()
      sit!(); println(s"$me sits"); pause()
      left!Pick; println(s"$me picks up left fork"); pause()
      right!Pick; println(s"$me picks up right fork"); pause()
      println(s"$me eats"); eat()
      left!Drop; pause(); right!Drop; pause()
      leave!(); println(s"$me leaves")
    }
  }

  /** A single fork. */
  private 
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

  /** The butler, who allows at most N-1 philosophers to sit at a time. */
  private def butler = thread("butler"){
    var seated = 0 // Number currently seated
    serve(
      seated < N-1 && sit =?=> { _ => seated += 1 }
      | leave =?=> { _ => assert(seated > 0); seated -= 1 }
    )
  }

  /** The complete system. */
  private def system = {
    // Channels to pick up and drop the forks:
    val philToLeftFork, philToRightFork = Array.fill(5)(new SyncChan[Cmd])
    // philToLeftFork(i) is from phil(i) to fork(i);
    // philToRightFork(i) is from phil(i) to fork((i-1)%N).
    val allPhils = || ( 
      for (i <- 0 until N)
      yield phil(i, philToLeftFork(i), philToRightFork(i))
    )
    val allForks = || ( 
      for (i <- 0 until N) yield
        fork(i, philToRightFork((i+1)%N), philToLeftFork(i))
    )
    allPhils || allForks || butler
  }

  /** Run the system. */
  def main(args: Array[String]) = run(system) 
}

  
// =======================================================

/** Simulation of the Dining Philosophers example, using a timeout. */
object PhilsTO{

  /** Time to wait for second fork (in ms). */
  private def waitTime = 300+Random.nextInt(200)

  /** Time to wait after failing to get second fork.  A larger amount of
    * randomness here means they get out of sync faster. */
  private def backoffTime = 200+Random.nextInt(800)

  /** A single philosopher. */
  private def phil(me: Int, left: !![Cmd], right: !![Cmd]) = thread("Phil"+me){
    repeat{
      think()
      println(s"$me sits"); pause()
      var done = false
      while(!done){
        left!Pick; println(s"$me picks up left fork"); pause()
        if(right.sendWithin(waitTime)(Pick)){
          println(s"$me picks up right fork"); pause()
          println(s"$me eats"); eat()
          left!Drop; pause(); right!Drop; pause()
          println(s"$me leaves"); done = true
        }
        else{
          println(s"$me fails to get right fork"); pause()
          // Pause for a random amount of time so that philosophers get out of
          // sync. 
          left!Drop; Thread.sleep(backoffTime)
        }
      } // end of while loop
    }
  }

  /** A single fork. */
  private def fork(me: Int, left: ??[Cmd], right: ??[Cmd]) = thread("Fork"+me){
    serve(
      left =?=> { x => assert(x == Pick); val y = left?(); assert(y == Drop) }
      |
      right =?=> { x => assert(x == Pick); val y = right?(); assert(y == Drop) }
    )
  }

  /** The complete system. */
  private def system: ThreadGroup = {
    // Channels to pick up and drop the forks, indexed by forks' identities
    val philToLeftFork, philToRightFork = Array.fill(N)(new SyncChan[Cmd])
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
  def main(args: Array[String]) = run(system) 
}

  
