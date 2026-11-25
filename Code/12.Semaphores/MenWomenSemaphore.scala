package tacp.semaphores

import ox.scl._
import tacp.clientServer.MenWomenT

class MenWomenSemaphore extends MenWomenT{
  /** Name of the current (or last) woman. */
  private var woman = ""

  /** Name of the current (or last) man. */
  private var man = ""

  /** Semaphore where a man waits initially.  A signal indicates that the
    * previous round is over. */
  private val roundOver = new MutexSemaphore

  /** Semaphore where a woman waits until signalled.  A signal indicates that
    * the man has written his name. */
  private val manWritten = new SignallingSemaphore

  /** Semaphore where the current man waits for a woman.  A signal indicates
    * that a woman has written her name. */
  private val womanWritten = new SignallingSemaphore

  // The above three semaphores collectively ensure mutual exclusion.

  def manSync(me: String): String = {
    roundOver.down()    // Wait my turn (1).
    man = me            // Store my name.
    manWritten.up()     // Signal to a woman at (2).
    womanWritten.down() // Wait for an acknowledgement (3).
    val her = woman     // Get her name.
    roundOver.up()      // Signal to the next man at (1).
    her
  }

  def womanSync(me: String): String = {
    manWritten.down()         // Wait for a man (2).
    woman = me; val him = man // Store my name, get his.
    womanWritten.up()         // Signal to him at (3).
    him
  }
}

// =======================================================

class FaultyMenWomenSemaphore extends MenWomenT{
  /** Name of the current (or last) woman. */
  private var woman = ""

  /** Name of the current (or last) man. */
  private var man = ""

  private val manMutex, womanMutex = new MutexSemaphore

  private val manSignal, womanSignal = new SignallingSemaphore

  def manSync(me: String) = {
    manMutex.down()
    man = me; manSignal.up() // Signal to woman at (2).
    womanSignal.down() // Wait for signal from woman (1).
    val her = woman; womanMutex.up() // Allow another woman to run.
    her
  }

  def womanSync(me: String) = {
    womanMutex.down()
    woman = me; womanSignal.up() // Signal to man at (1).
    manSignal.down() // Wait for signal from man (2).
    val him = man; manMutex.up() // Allow another man to run.
    him
  }
}
