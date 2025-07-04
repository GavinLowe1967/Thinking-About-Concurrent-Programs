package tacp.util.experiments
import java.io._

/** Code for producing graphs, using PGFPlots.  
  * See [[http://pgfplots.sourceforge.net/pgfplots.pdf PGFPlots manual]].
  *
  * Typical usage might be something like the following.
  * {{{
  *   val options = 
  *     Array[String](
  *       "title = Comparison of linearizability testers on a queue", 
  *       "xlabel = Number of operations per run", 
  *       "ylabel = Time (s)", 
  *       "log basis x=2", "log basis y = 10", 
  *       "legend pos = north west",
  *       "height = 0.9\\textheight", "width = 0.9\\textwidth"
  *     )
  *   val graphString = 
  *     Graphs.makeLogXGraph(options, testersNames, opsNumbers, results)
  *   Graphs.writeStandAloneFile("queue.tex", graphString)
  * }}}
  */
object Graphs{
  /** Make a single plot.
    * @param plotName the names of the label for the plot.
    * @param xLabels the labels for the x-axis.
    * @param data the actual data: data(xi) gives data for x-axis label 
    * xLabels(xi), namely the value to plot and the confidence level.
    */
  private def makePlot[A](plotName: String, xLabels: Array[A], 
	       		  data: Array[(Double,Double)]) : String
  = {
    assert(xLabels.length == data.length, 
      "Plot data length does not match number of x-labels"+ data.mkString(", ") )
    // Make a single entry
    def mkEntry(t: (A,(Double,Double))) = t match {
      case (x,(datum,conf)) => 
	if(datum < Experiments.ErrorSignal)
	  "  "+(x,datum).toString+" +- (0,"+conf+")"
	else ""
      case (x,null) => ""
    }
    "\\addplot+[error bars/.cd, y dir=both,y explicit] coordinates {\n"+
    xLabels.zip(data) . map(mkEntry) . filter(_ != "") . mkString("\n")+
    "\n};\n"+
    "\\addlegendentry{"+plotName+"}"
  }


  /** Make plots (for pgfplots).
    * @param plotNames the names of labels for the plots
    * @param xLabels the labels for the x-axis
    * @param data the actual data: data(pi)(xi) gives data for plot 
    * plotNames(pi) and x-axis label xLabels(xi), namely the value to plot and
    * the confidence level
    */
  private def makePlots[A](plotNames: Array[String], xLabels: Array[A], 
	 		   data: Array[Array[(Double,Double)]])
  = {
    assert(plotNames.length == data.length,
	   "makePlots: plotNames has length "+plotNames.length+
	   "; data has length "+data.length)
    plotNames.zip(data).map{
      case (plotName, d) => makePlot(plotName, xLabels, d)
    }.mkString("\n")
  }

  //   assert(plotNames.length == data.length)
  //   assert(data.forall(_.length == xLabels.length))
  //   // Make a single entry
  //   def mkEntry(t: (Int,(Double,Double))) = {
  //     val (x,(datum,conf)) = t; "  "+(x,datum).toString+" +- (0,"+conf+")"
  //   }
  //   (for(i <- 0 until plotNames.length) yield 
  //     "\\addplot+[error bars/.cd, y dir=both,y explicit] coordinates {\n"+
  //     xLabels.zip(data(i)) . map(mkEntry) . mkString("\n")+
  //     "\n};\n"+
  //     "\\addlegendentry{"+plotNames(i)+"}"
  //   ).mkString("\n")
  // }

  /** Like makePlots, but swapping X and Y axes */
  private def makeSwappedPlots[A](plotNames: Array[String], xLabels: Array[A], 
				  data: Array[Array[(Double,Double)]])
  = {
    assert(plotNames.length == data.length)
    assert(data.forall(_.length == xLabels.length))
    // Make a single entry
    def mkEntry(t: (A,(Double,Double))) : String = t match{
      case (x,(datum,conf)) => 
	if(datum < Experiments.ErrorSignal)
	  "  "+(datum,x).toString+" +- ("+conf+",0)"
	else ""
      case (x,null) => ""
    }
    (for(i <- 0 until plotNames.length) yield 
      "\\addplot+[error bars/.cd, x dir=both,x explicit] coordinates {\n"+
      xLabels.zip(data(i)) . map(mkEntry) . mkString("\n")+
      "\n};\n"+
      "\\addlegendentry{"+plotNames(i)+"}"
    ).mkString("\n")
  }


  /** Make plots (for pgfplots).
    * @param axisType the type of axes to use, e.g. "semilogxaxis",
    * "loglogaxis"
    * @param extraOptions options to be added to the graph; examples might
    * include "title=...", "width=...", "height=...", "xlabel=...", 
    * "ylabel=...", "log basis x=2", "legend pos=..."; see the pgfplots 
    * manual. 
    * @param plotNames the names of labels for the plots
    * @param xLabels the labels for the x-axis
    * @param data the actual data: data(pi)(xi) gives data for plot 
    * plotNames(pi) and x-axis label xLabels(xi), namely the value to plot and
    * the confidence level; if the value to plot is Experiments.ErrorSignal
    * then it is taken to represent an error and this item is omitted. 
    */
  def makeGraph[A](axisType: String)
	(extraOptions: Array[String], plotNames: Array[String], 
	 xLabels: Array[A], data: Array[Array[(Double,Double)]]) 
  = {
    "\\begin{tikzpicture}\n"+
    "\\begin{"+axisType+"}[\n"+
    extraOptions.map("  "+_).mkString(",\n")+"\n"+
    "]\n"+
    makePlots(plotNames, xLabels, data)+"\n"+
    "\\end{"+axisType+"}\n"+
    "\\end{tikzpicture}\n"	
  }

  /** Make a log-X graph (i.e. logarithmic in the x-axis, and linear in the
    * y-axis).  Parameters as for makeGraph */
  def makeLogXGraph[A]
      (extraOptions: Array[String], plotNames: Array[String], 
       xLabels: Array[A], data: Array[Array[(Double,Double)]]) = 
    makeGraph("semilogxaxis") (extraOptions, plotNames, xLabels, data)

  /** Make a log-log graph (i.e. logarithmic in both axes).
    * Parameters as for makeGraph */  
  def makeLogLogGraph[A]
      (extraOptions: Array[String], plotNames: Array[String], 
       xLabels: Array[A], data: Array[Array[(Double,Double)]]) = 
    makeGraph("loglogaxis") (extraOptions, plotNames, xLabels, data)

  /** Make a linear graph (i.e. linear in both axes).
    * Parameters as for makeGraph */  
  def makeLinearGraph[A]
      (extraOptions: Array[String], plotNames: Array[String], 
       xLabels: Array[A], data: Array[Array[(Double,Double)]]) = 
    makeGraph("axis") (extraOptions, plotNames, xLabels, data)

  /** Make a graph with axes swapped.  
    * Parameters as for makeGraph. */
  def makeSwappedGraph[A](extraOptions: Array[String], 
		plotNames: Array[String], labels: Array[A], 
		data: Array[Array[(Double,Double)]]) 
  = {
    "\\begin{tikzpicture}\n"+
    "\\begin{semilogyaxis}[\n"+
    extraOptions.map("  "+_).mkString(",\n")+"\n"+
    "]\n"+
    makeSwappedPlots(plotNames, labels, data)+"\n"+
    "\\end{semilogyaxis}\n"+
    "\\end{tikzpicture}\n"	
  }

  /** Write file fname with contents output */
  def writeFile(fname: String, output: String) = {
    val outFile = new FileWriter(fname)
    outFile.write(output); outFile.close
  }

  /** Write a LaTeX file fname enclosing body */
  def writeStandAloneFile(fname: String, body: String) = {
    val preamble = 
      "\\documentclass[a4paper]{article}\n"+
      "\\usepackage{pgfplots}\n"+
      "\\pgfplotsset{compat=1.16}\n"+
      "\\begin{document}\n\n"
    writeFile(fname, preamble+body+"\\end{document}\n")
  }
}
