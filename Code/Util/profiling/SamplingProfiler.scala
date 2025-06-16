package tacp.util.profiling

import scala.jdk.CollectionConverters._
import java.lang.StackTraceElement
import scala.collection.mutable.ArrayBuffer

/** A sampling profiler.
  * 
  * @param interval the interval (in ms) between samples; it is recommended that
  * the number of samples is of the order of a few thousand.
  * @param print a function to produce the output; by default, 
  * SamplingProfiler.print */
class SamplingProfiler(
  interval: Int = 20, 
  print: ArrayBuffer[SamplingProfiler.StackTrace] => String =
    SamplingProfiler.print(length = 20)(_),
  busyWait: Boolean = false
){
  /** Has the profiler been signalled to finish? */
  private var done_ = false

  /** Indicate to the profiler that it should finish, because the profiled code
    * has finished. */
  def done = done_ = true

  var iters = 0L

  private type StackTrace = SamplingProfiler.StackTrace

  /** A string representing the output of the profile. */
  private var output = ""

  /** Run the profile until done is called. */
  def run() = {
    // Observed StackTraces are stored in the following
    val store = new ArrayBuffer[java.util.Collection[StackTrace]] 
    output = ""
    var lastSample = System.currentTimeMillis
    //var lateSamples = 0

    // Monitor the stacktraces
    while(!done_){
      val stackTraces : java.util.Collection[StackTrace] =
        Thread.getAllStackTraces().values // .iterator.asScala.toList
      store += stackTraces
      // Wait until lastSample+interval
      if(busyWait){
        val endTime = lastSample+interval; lastSample = System.currentTimeMillis
        while(endTime-System.currentTimeMillis > 0){ iters += 1 }
      }
      else{
        val now = System.currentTimeMillis
        val sleepTime = lastSample+interval-now; lastSample = now
        // Avoid sleeping for very short intervals
        if(sleepTime > 10) Thread.sleep(sleepTime)
        else if(sleepTime > 0) lastSample -= sleepTime
      }
    }

    //println("lateSamples = "+lateSamples)
    output = print(store.map(_.iterator.asScala.toList).flatten) 
  }

  /** Create a thread that performs comp. */
  private def mkThread(comp: => Unit) : Thread = 
    new Thread(new Runnable{ def run = comp })  

  /** Create a system from `p` and `q`; run the system, terminating when both
    * terminate. */
  private def runParallel(p: => Unit, q: => Unit) = {
    val threads = Array(mkThread(p), mkThread(q))
    threads.foreach(_.start)
    threads.foreach(_.join)
  }

  /** Run the computation comp in parallel with this profiler.
    * @param print should the output be printed?
    * @return the output.
    */
  def apply(comp: => Unit, print: Boolean = true) : String = {
    runParallel(this.run(), try{comp} finally{done})
    if(print) println(output)
    output
  }

}

// ==================================================================

object SamplingProfiler{
  type StackTrace = Array[java.lang.StackTraceElement]

  /** String representing the stacktraces in store.
    * @param length the number of entries to output.
    * @param filter a filtering function to decide if a given StackTraceElement 
    * should be included. */
  def print(
    length: Int = 20,
    filter: StackTraceElement => Boolean = SamplingProfiler.defaultFilter)
    (store: ArrayBuffer[StackTrace])
      : String = {
    println("Total samples: "+store.length)
    var nonempty = 0

    // Extract information from store into map from (class name, method name)
    // to elapsed time with this as the top (non-filtered) frame, or anywhere
    // in the stack.
    val table =
      new scala.collection.mutable.HashMap[(String, String), (Int, Int)]
    for(stackTrace <- store){
      val filteredTrace = stackTrace.filter(filter).map(getKey)
      if(filteredTrace.nonEmpty){
        nonempty += 1
        // Top frame in stack
        val key = filteredTrace(0)
        table.get(key) match{
          case Some((n1,n2)) => table.put(key, (n1+1,n2+1))
          case None => table.put(key, (1,1))
        }
        // Remaining frames
        for(i <- 1 until filteredTrace.length){
          val key = filteredTrace(i)
          // test if this is the first occurrence of key
          var found = false
          for(j <- 0 until i; if !found) found = filteredTrace(j) == key
          if(!found) table.get(key) match{
            case Some((n1,n2)) => table.put(key, (n1,n2+1))
            case None => table.put(key, (0,1))
          }
        }
      }
    }

    println("Filtered samples: "+nonempty)

    // Produce output
    val arrayTable = table.toArray
    val sortedTable1 = arrayTable.sortBy(_._2._1).takeRight(length).reverse
    val sortedTable2 = arrayTable.sortBy(_._2._2).takeRight(length).reverse
    val sortedTable = sortedTable1++sortedTable2
    val nameColWidth = // width of name column
      (sortedTable.map{case ((c,m),_) => c.length+m.length}.max + 2) min 70
    val topColWidth = sortedTable.map(_._2._1).max.toString.length + 1
    def showEntry(entry: ((String, String), (Int, Int))) : String = {
      val ((c,m),(n1,n2)) = entry; val name = c+"."+m
      truncateLeft(name, nameColWidth) +
        //name.takeRight(nameColWidth) + " "*(nameColWidth-name.length) +
        n1 + " "*(topColWidth-n1.toString.length) + n2
    }

    sortedTable1.map(showEntry).mkString("\n") + "\n\n" +
      sortedTable2.map(showEntry).mkString("\n")
  }

  /** Print st in a column of width width, truncating on the left. */
  def truncateLeft(st: String, width: Int) =
    st.takeRight(width) + " "*(width-st.length)

  def printTree(
    filter: StackTraceElement => Boolean = SamplingProfiler.defaultFilter,
    expand: ProfilerSummaryTree => Unit = ProfilerSummaryTree.expand(3))
    (store: ArrayBuffer[StackTrace])
      : String = {
    // Apply filter to traces and reverse
    val filteredTraces =
      store.map(_.filter(filter)).filter(_.nonEmpty).map(_.reverse).toList
    val trees = mkProfilerSummaryTrees(filteredTraces)
    for(tree <- trees) expand(tree)
    for(tree <- trees) println(tree)
    ""
  }

  def mkProfilerSummaryTrees(stacks: List[StackTrace])
      : List[ProfilerSummaryTree] = {
    val grouped: Map[(String, String), List[StackTrace]] =
      stacks.filter(_.nonEmpty).groupBy(st => SamplingProfiler.getKey(st.head))
    for((cName, mName) <- grouped.keys.toList) yield{
      val callees1 = grouped((cName, mName)).map(_.tail)
      new ProfilerSummaryTree(cName, mName, callees1)
    }

  }


  /** Extract (class-name, method-name) from stack frame. */
  def getKey(f: StackTraceElement) = (f.getClassName, f.getMethodName)


  /** Is frame not an API call, i.e. from a class not starting with "scala.",
    * "java." or "ox.cads."?  This is the default filter used when
    * constructing SamplingProfilers. */
  def notAPIFrame(frame: StackTraceElement) : Boolean = {
    val c = frame.getClassName;
    !(c.startsWith("scala.") || c.startsWith("java.") ||
        c.startsWith("ox.cads.") || c.startsWith("ox.gavin") ||
        c.startsWith("sun"))
  }

  /** Is frame not an anonymous function (function name containing
    * "\$anonfun\$"), nor a lambda abstraction (class name containing
    * "\$\$Lambda\$")? */
  def notFunction(frame: StackTraceElement) : Boolean = 
    !frame.getMethodName.contains("$anonfun$") &&
      !frame.getClassName.contains("$$Lambda$")

  /** Default filter, that frame is not an API call, an anonymous function or a
    * lambda. */
  def defaultFilter(frame: StackTraceElement) : Boolean =
    notAPIFrame(frame) && notFunction(frame)
}


// ==================================================================

/** A node in a tree summarising profiling information, giving information
  * about all method calls that have been reached by the same path of method
  * calls.
  * @param className the name of the class of the current call.
  * @param methodName the name of the method of the current call.
  * @param callees stack traces onwards from this method (in reverse order). */
class ProfilerSummaryTree(
  className: String, methodName: String,
  val callees: List[SamplingProfiler.StackTrace]){ 

  type StackTrace = SamplingProfiler.StackTrace

  /** The time spent in this method itself. */
  private val localTime = callees.filter(_.isEmpty).length

  /** The time spent in this method or in subsequent calls. */
  private val totalTime = callees.length

  /** The children nodes of this; initially undefined. */
  private var children: List[ProfilerSummaryTree] = null

  /** Expand one level, initialising children. */
  def expand: Unit = 
    children = SamplingProfiler.mkProfilerSummaryTrees(callees) 

  /** Expand d levels. */
  def expand(d: Int): Unit =
    if(d > 0){ expand; for(c <- children) c.expand(d-1) }

  /** Recursively expand this and all descendent nodes that satisfy p. */
  def expandWhile(p: ProfilerSummaryTree => Boolean): Unit =
    if(p(this)){
      expand
      for(c <- children) c.expandWhile(p)
    }

  /** Produce string output. */
  override def toString: String = toString(65, "", "", "", "")

  /** Width of column to print method name. */
  // private val nameColWidth = 65

  /** Width of each column of numbers. */
  private val countColWidth = 5

  def rjustify(n: Int, w: Int, c: String = " ") = {
    val st = n.toString; c*(w-st.length)+st
  }

  /** String representing this. 
    * @param prefixFirst string with which to prefix the first line.
    * @param prefix string with which to prefix each subsequent line with.
    * @param numsPrefixFirst string with which to prefix the numbers on the
    * first line.
    * @param numsPrefix string with which to prefix numbers on subsequent lines. 
    */
  def toString(
    nameColWidth: Int = 65,
    prefixFirst: String, prefix: String,
    numsPrefixFirst: String, numsPrefix: String)
      : String = {
    // Helper functions to print all except last child, and last child.
    def printInitChild(c: ProfilerSummaryTree) =
      c.toString(
        nameColWidth,
        prefix+"  |", prefix+"  |", numsPrefix+"    |", numsPrefix+"    |")
    def printLastChild(c: ProfilerSummaryTree) =
      c.toString(
        nameColWidth,
        prefix+"  |", prefix+"   ", numsPrefix+"    |", numsPrefix+"     ")

    // Method name
    prefixFirst+"--"+
      SamplingProfiler.truncateLeft(
        className+"."+methodName+": ", nameColWidth-prefixFirst.length)+
      // Total and local time
      numsPrefixFirst+
      rjustify(totalTime, countColWidth, "-")+
      rjustify(localTime, countColWidth)+"\n"+
      // Children
      (if(children != null && children.nonEmpty)
         children.init.map(printInitChild).mkString +
         printLastChild(children.last)
       else "")
  }

}

object ProfilerSummaryTree{

  def expand(d: Int)(pst: ProfilerSummaryTree) = pst.expand(d)

  def expandWhile(p: ProfilerSummaryTree => Boolean)(pst: ProfilerSummaryTree) =
    pst.expandWhile(p)

  def expandToThreshold(ratio: Double = 0.05)(pst: ProfilerSummaryTree) = {
    val thresh = pst.totalTime * ratio
    expandWhile(_.totalTime >= thresh)(pst)
  }

}
