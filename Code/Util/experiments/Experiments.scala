package tacp.util.experiments

import java.io._

/** Code to support experiments. */
object Experiments{

  private val rt = java.lang.Runtime.getRuntime()

  /** Flush the error stream of proc, returning true if there were any
    * errors. */ 
  private def flushErr(proc: java.lang.Process) : Boolean = {
    val isrErr = new InputStreamReader(proc.getErrorStream, "ISO-8859-1")
    val errProc = new BufferedReader(isrErr)
    var err = errProc.readLine
    if(err != null){
      do{ println(err); err = errProc.readLine } while(err != null)
      true
    }
    else false
  }
 
  /** Run a process given by cmd, and return the final numeric string it
    * produces, which is expected to be its time.  If `verbose` is true, all
    * other strings produced by the process are echoed to standard out.  Any
    * errors produced by the process are also echoed, leading to an exception
    * being thrown.
    */
  def timeProc(cmd: String, verbose: Boolean = false) : Long = {
    val proc = rt.exec(cmd)
    val isr = new InputStreamReader(proc.getInputStream, "ISO-8859-1")
    val fromProc = new BufferedReader(isr)
    val isrErr = new InputStreamReader(proc.getErrorStream, "ISO-8859-1")
    val errProc = new BufferedReader(isrErr)
    var t = -1L // last value that looked like an integer
    var tstr = fromProc.readLine
    while(tstr != null){
      if(tstr.nonEmpty && tstr.forall(_.isDigit)) t = tstr.toLong
      if(verbose) print("*"+tstr+"; ")
      tstr = fromProc.readLine
    }
    // Flush error stream
    var err = errProc.readLine
    if(err != null){
      do{ println(err); err = errProc.readLine } while(err != null);
      assert(false)
    }
    // assert(!flushErr(proc)) // check for errors
    assert(t > 0, "No time string detected\nLast string: "+tstr)
    proc.waitFor; t    
  }

  /** Parameters of statistical analysis of experiments
    * @param invocsMin minimum number of measurements. (e.g. 2).
    * @param invocsMax maximum number of measurements. (e.g. 50).
    * @param significance the desired significance levels. (e.g. 0.05).
    * @param desiredConfInterval the desired size of the confidence 
    * half-interval as a proportion of the mean. (e.g. 0.01).
    */
  class Params(val invocsMin: Int, val invocsMax: Int, 
	       val significance: Double, val desiredConfInterval: Double)
  {
    def this(invocsMax: Int, significance: Double, 
	     desiredConfInterval: Double) 
    = this(2, invocsMax, significance, desiredConfInterval)

    assert(invocsMin >= 2 && invocsMin <= invocsMax)
  }

  /** Any measurement giving a value of ErrorSignal is taken to represent
    * an error. */
  val ErrorSignal = Long.MaxValue 

  /** Iterate measurement measure, trying to find statistically significant 
    * average.  After each iteration except the first, the mean and confidence
    * intervals with significance level significance are calculated.  This
    * is repeated as specified by params.  If measure returns a value of at
    * least ErrorSignal, this is taken to represent an error, and the 
    * measurements stop. 
    * @return the mean and significance. */
  def iterateMeasurement(measure: => Double, params: Params) : (Double, Double)
  = {
    val observations = new Array[Double](params.invocsMax)
    var invoc = 0

    while(true){
      val m = measure
      if(m == ErrorSignal) return (m, 0.0)
      observations(invoc) = m
      if(invoc+1 >= params.invocsMin){
        // Find confidence level
        val (mean,delta) = 
          ConfidenceIntervals(observations.take(invoc+1), params.significance)
        if(delta < mean*params.desiredConfInterval || 
	   invoc+1 == params.invocsMax) 
	  return (mean, delta)
      }
      invoc += 1
    }
    sys.error("unreachable")
  }

}
