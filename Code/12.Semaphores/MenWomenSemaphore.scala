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
