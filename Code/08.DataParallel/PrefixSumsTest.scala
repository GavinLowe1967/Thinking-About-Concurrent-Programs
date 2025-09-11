package tacp.dataParallel

import ox.scl._

import scala.util.Random

object PrefixSumsTest{
  val reps = 10000

  // Representation of different choices of tester.
  val MessagePass = 0; val Shared = 1; val Shared2 = 2

  /** Do a single test. */
  def doTest(choice: Int) = {
    // Pick random n and array.
    val n = 1+Random.nextInt(20)
    val a = Array.fill(n)(Random.nextInt(100))
    // Calculate prefix sums sequentially.
    val mySum = new Array[Int](n)
    var s = 0
    for(i <- 0 until n){ s += a(i); mySum(i) = s }
    // Calculate them concurrently.
    val summer: PrefixSumsT =
      if(choice == MessagePass) new PrefixSums(n, a)
      else if(choice == Shared) new PrefixSumsShared(n, a) 
      else if(choice == Shared2) new PrefixSumsShared2(n, a)
      else ???
    // Note: the above assumes classes PrefixSumsShared and PrefixSumsShared2,
    // solutions for an exercise.  Comment out relevant lines if these are not
    // available.
    val sum = summer()
    // Compare.
    assert(sum.sameElements(mySum),
           "a = "+a.mkString(", ")+"\nsum = "+sum.mkString(", ")+
             "\nmySum = "+mySum.mkString(", "))
  }

  def main(args : Array[String]) = {
    var choice = 0; var i = 0
    while(i < args.length) args(0) match{
      case "--shared" => choice = Shared; i += 1
      case "--shared2" => choice = Shared2; i += 1
    }

    for(r <- 0 until reps){ doTest(choice); if(r%100 == 0) print(".") }
    println()
  }
}

