package tacp.datatypes

import scala.collection.mutable.{Set,ArrayBuffer}

/** A graph of words, corresponding to the dictionary file fName.  Two words
  * are neighbours if they differ by a single character. */
class WordGraph(fName: String) extends Graph[String]{
  /** A dictionary. */
  private val dict = Set[String]()

  // Initialise dict
  for(w <- scala.io.Source.fromFile(fName).getLines()) dict += w

  def succs(w: String): List[String] = {
    var result = new ArrayBuffer[String]
    for(i <- 0 until w.length; c <- 'a' to 'z'; if c != w(i)){
      val w1 = w.patch(i,List(c),1) // replace ith character of w with c
      if(dict.contains(w1)) result += w1
    }
    result.toList
  }
}

// -------------------------------------------------------

object WordPath{
  val numWorkers = 8 // Number of workers.

  def main(args: Array[String]) = {
    // Parse arguments. 
    var start = ""; var target = ""; var searcherT = "conc"
    var i = 0
    while(i < args.length) args(i) match{
      case "--seq" => searcherT = "seq"; i += 1
      case "--barrier" => searcherT = "barrier"; i += 1
      case "--jvm" => searcherT = "jvm"; i += 1
      case w => 
        if(start.isEmpty) start = w 
        else if(target.isEmpty) target = w
        else{ println(s"Extra argument: $w"); sys.exit() }
        i += 1
    }
    if(target.isEmpty){ println("Missing argument(s)"); sys.exit() }

    val g = new WordGraph("knuth_words.txt")
    val searcher: GraphSearch[String] = searcherT match{
      case "conc" => new ConcBFGraphSearch(g, numWorkers) 
      case "barrier" => new tacp.dataParallel.BFSGraphSearch(g, numWorkers)
      case "jvm" => new tacp.jvmMonitors.MonitorGraphSearch(g, numWorkers)
      case "seq" => new SeqBFGraphSearch(g)
    }
    def isTarget(w: String) = w == target
    searcher(start, isTarget) match{
      case Some(p) => println(p.mkString(", "))
      case None => println("No solution found.")
    }
  }
}
