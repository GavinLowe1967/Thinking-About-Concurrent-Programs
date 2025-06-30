package tacp.clientServer

import ox.scl._

class MenWomen{
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
  def shutdown() = { manProp.close(); womanProp.close() }
}



// =======================================================

object MenWomenTest{
  /* In each test, each man/woman writes the identity of his/her partner into an
   * array.  We then test that the two arrays agree. */

  // Arrays that hold the id of each man/woman's partner
  var partnerOfMan, partnerOfWoman: Array[Int] = null

  /** Thread for a man. */
  def man(me: Int, mw: MenWomen) = thread{
    partnerOfMan(me) = mw.manSync(me.toString).toInt
  }

  /** Thread for a woman. */
  def woman(me: Int, mw: MenWomen) = thread{
    partnerOfWoman(me) = mw.womanSync(me.toString).toInt
  }

  /** Do a single test. */
  def doTest() = {
    val n = scala.util.Random.nextInt(10) // Number of men and women.
    partnerOfMan = new Array[Int](n); partnerOfWoman = new Array[Int](n)
    val mw = new MenWomen 
    val men = || (for(i <- 0 until n) yield man(i, mw))
    val women = || (for(i <- 0 until n) yield woman(i, mw))
    run(men || women)
    mw.shutdown()
    for(m <- 0 until n)
      assert(partnerOfWoman(partnerOfMan(m)) == m,
             partnerOfMan.mkString(", ")+"\n"+
               partnerOfWoman.mkString(", ")+"\n"+m)
  }

  /** Main method. */
  def main(args: Array[String]) = {
    for(i <- 0 until 10000){ doTest(); if(i%200 == 0) print(".") }
    println()
  }
}
