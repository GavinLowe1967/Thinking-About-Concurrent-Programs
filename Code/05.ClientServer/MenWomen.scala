package tacp.clientServer

import ox.scl._

trait MenWomenT{
  def manSync(me: String): String

  def womanSync(me: String): String

  def shutdown(): Unit = {}
}
 
class MenWomen extends MenWomenT{
  private type ReplyChan = Chan[String]

  /** Channels sending proposals from a man, resp., a woman. */
  private val manProp, womanProp = new SyncChan[(String, ReplyChan)]

  /** A man tries to find a partner. */
  def manSync(me: String): String = {
    val c = new OnePlaceBuffChan[String]; manProp!(me,c); c?()
  }

  /** A woman tries to find a partner. */
  def womanSync(me: String): String = {
    val c = new OnePlaceBuffChan[String]; womanProp!(me,c); c?()
  }

  /** The server. */
  private def server = thread{
    repeat{
      // Wait for a man and woman, and pair them off. 
      val (him,hisC) = manProp?(); val (her,herC) = womanProp?()
      hisC!her; herC!him
    }
    manProp.close(); womanProp.close()
  }

  fork(server)

  /** Shut down this object (so the server thread terminates). */
  override def shutdown() = { manProp.close(); womanProp.close() }
}
