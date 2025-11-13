package tacp.trapezium

import ox.scl._

import Trapezium.Task

/** The trait for the bag of tasks objects. */
trait BagOfTasks{
  def getTask(): Task
}

// =================================================================

/** A collector object that receives sub-results from the workers, and adds
  * them up. */
trait Collector{
  /** Add x to the result. */
  def add(x: Double): Unit

  /** Get the result. */
  def get: Double
}

// ------------------------------------------------------------------
