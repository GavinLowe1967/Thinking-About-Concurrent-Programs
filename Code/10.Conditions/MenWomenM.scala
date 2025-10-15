package tacp.monitors

import ox.scl._


// Old version: I prefer the one below.

// class MenWomenM extends tacp.clientServer.MenWomenT{
//   /** Information about who is currently waiting. */
//   private abstract class Status

//   /** The man "him" is currently waiting. */
//   private case class ManWaiting(him: String) extends Status

//   /** The woman "her" has just paired with the current man. */
//   private case class WomanDone(her: String) extends Status

//   /** Nobody is currently waiting. */
//   private case object Nobody extends Status

//   /** A slot which holds information about the current status of the exchange. */
//   private var slot: Status = Nobody

//   /** Monitor to control the synchronization. */
//   private val lock = new Lock 

//   /** Condition to signal that the slot has been cleared, ready for the next
//     * couple. */
//   private val slotEmpty = lock.newCondition

//   /** Condition to signal that a man's name is in the slot, and he is waiting
//     * for a woman. */
//   private val manWaiting = lock.newCondition

//   /** Condition to signal that a man has written her name, and is paired with
//     * the waiting man. */
//   private val womanDone = lock.newCondition

//   /* A typical execution of a couple will be:
//    * - man waits for slot to become empty, writes his name, signals to woman, 
//    *   (on manWaiting), and waits;
//    * - woman waits for signal, reads man's name, writes her name, signals to
//    *   man (on womanDone), and finishes;
//    * - man reads woman's name, clears name, signals to next man (on SlotEmpty), 
//    *   and finishes.
//    */

//   /** A man tries to find a partner. */
//   def manSync(me: String): String = lock.mutex{
//     // Wait to write my name
//     slotEmpty.await(slot == Nobody)          // wait for signal (1)
//     slot = ManWaiting(me); manWaiting.signal() // signal to woman waiting at (2)
//     womanDone.await()                        // wait for woman (3)
//     val WomanDone(her) = slot
//     slot = Nobody; slotEmpty.signal()       // signal to man waiting at (1)
//     her
//   }

//   /** A woman tries to find a partner. */
//   def womanSync(me: String): String = lock.mutex{
//     manWaiting.await(slot.isInstanceOf[ManWaiting]) // wait for signal (2)
//     val ManWaiting(him) = slot
//     slot = WomanDone(me); womanDone.signal()   // signal to man waiting at (3)
//     him
//   }
// }


class MenWomenM extends tacp.clientServer.MenWomenT{
  private val Empty = 0; private val ManStored = 1; private val WomanStored = 2

  /** The stage of the current exchange. */
  private var stage = Empty

  /** The name of the current man when stage = ManStored; the name of the
    * current woman when stage = WomanStored. */
  private var name = ""

  private val lock = new Lock

  private val nameCleared, manWritten, womanWritten = lock.newCondition

  def manSync(me: String): String = lock.mutex{
    nameCleared.await(stage == Empty) // Wait for previous round to finish. (1)
    name = me; stage = ManStored
    manWritten.signal() // Signal to woman at (2).
    womanWritten.await() // Wait for woman. (3)
    stage = Empty; nameCleared.signal() // Signal to next man at (1).
    name
  }

  def womanSync(me: String): String = lock.mutex{
    manWritten.await(stage == ManStored) // Wait for man. (2)
    val him = name; name = me; stage = WomanStored
    womanWritten.signal() // Signal to man at (3).
    him
  }
}
