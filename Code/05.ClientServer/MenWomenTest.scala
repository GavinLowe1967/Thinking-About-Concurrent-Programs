package tacp.clientServer

import ox.scl._

import tacp.monitors.MenWomenM

object MenWomenTest{
  /* In each test, each man/woman writes the identity of his/her partner into an
   * array.  We then test that the two arrays agree. */

  // Arrays that hold the id of each man/woman's partner
  var partnerOfMan, partnerOfWoman: Array[Int] = null

  /** Thread for a man. */
  def man(me: Int, mw: MenWomenT) = thread{
    partnerOfMan(me) = mw.manSync(me.toString).toInt
  }

  /** Thread for a woman. */
  def woman(me: Int, mw: MenWomenT) = thread{
    partnerOfWoman(me) = mw.womanSync(me.toString).toInt
  }

  private var choice = "cs"

  /** Do a single test. */
  def doTest() = {
    val n = scala.util.Random.nextInt(10) // Number of men and women.
    partnerOfMan = new Array[Int](n); partnerOfWoman = new Array[Int](n)
    val mw: MenWomenT = 
      if(choice == "cs") new MenWomen 
      else{ assert(choice == "monitor"); new MenWomenM }
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
    var i = 0
    while(i < args.length) args(i) match{
      case "--monitor" => choice = "monitor"; i += 1
    }

    for(i <- 0 until 10000){ doTest(); if(i%200 == 0) print(".") }
    println()
  }
}
