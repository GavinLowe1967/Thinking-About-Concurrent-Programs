package tacp.trapezium

import ox.scl._

/** Estimate pi, using the fact that if x, y are random numbers in [0,1), then
  * P(x^2+y^2 < 1) = pi/4. */
object Pi{
  /* Each task will involve generating taskSize pairs (x,y), counting the number
   * such that x^2+y^2 < 1.0, and sending the result to the controller. */

  val taskSize = 400000 // Size of a single task.
  val numTasks = 200    // Number of tasks.
  val numWorkers = 8    // Number of workers.

  /** Channel from workers to the controller. */
  val toController = new BuffChan[Int](numWorkers)

  /*
   This version has to channel from the controller to workers.  It involves
   each worker performing one extra task.

  def worker = thread("worker"){
    val random = new scala.util.Random
    repeat{
      var count = 0
      for(i <- 0 until taskSize){
	val x = random.nextDouble(); val y = random.nextDouble()
	if(x*x + y*y < 1.0) count += 1
      }
      toController!count
    }
  }

  def controller = thread("controller"){
    var count = 0
    for(i <- 0 until numTasks) count += (toController?())
    toController.close()
    println(4.0*count.toDouble/(taskSize*numTasks))
  }

  def system = controller || (|| ( for(i <- 0 until numWorkers) yield worker))
   */

  val toWorkers = new BuffChan[Unit](numWorkers)

  def worker = thread("worker"){
    val random = new scala.util.Random; var count = 0
    repeat{
      toWorkers?()
      for(i <- 0 until taskSize){
	val x = random.nextDouble(); val y = random.nextDouble()
	if(x*x + y*y < 1.0) count += 1
      }
    } // end of repeat
    toController!count
  }

  def controller = thread("controller"){
    for(i <- 0 until numTasks) toWorkers!()
    toWorkers.endOfStream()
    var count = 0
    for(i <- 0 until numWorkers) count += (toController?())
    println(4.0*count.toDouble/(taskSize*numTasks))
  }

  def system = controller || (|| ( for(i <- 0 until numWorkers) yield worker))

  def main(args: Array[String]) = {
    val t0 = java.lang.System.currentTimeMillis()
    run(system)
    println("Time taken: "+(java.lang.System.currentTimeMillis()-t0)/1000.0+"ms")
  }
}
