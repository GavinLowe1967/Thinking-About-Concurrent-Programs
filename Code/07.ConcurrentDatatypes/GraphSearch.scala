package tacp.datatypes

/** A trait representing an unlabelled graph with nodes of type N. */
trait Graph[N]{
  /** The successors of node n. */
  def succs(n: N): List[N]
}

// =======================================================

/** Trait representing a graph search problem in graph g. */
abstract class GraphSearch[N](g: Graph[N]){
  type Path = List[N]

  /** Try to find a path in g from start to a node that satisfies isTarget. */
  def apply(start: N, isTarget: N => Boolean): Option[Path]
}
