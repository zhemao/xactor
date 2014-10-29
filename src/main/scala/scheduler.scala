package Xactor

import Chisel._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scalax.collection.mutable.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import scalax.collection.edge.LUnDiEdge
import scalax.collection.edge.Implicits._
import scalax.io._
/*
object GNode {
  def apply(index:Int, color:Int = 0) {
    new GNode(index)
  }
}
class GNode(val index: Int) {
  var color = 0
}*/

class Scheduler {
  private var nodeList = scalax.collection.mutable.Graph[Int,LUnDiEdge]()
  implicit val factory = scalax.collection.edge.LUnDiEdge

  def initialize(numActions : Int) : Unit = {
    (0 until numActions).foreach(node => nodeList.add(node))
  }

  def registerDepList(depList: HashMap[String, ArrayBuffer[Int]], typ:String) : Unit = {
  for ((k,v) <- depList) {
    for (parent <- v.tails; if parent != Nil){
      for(child <- parent.tail) {
        nodeList.addLEdge(parent.head,child)(typ)
        }
      }
    }
  }

  def getNumNodes: Int = nodeList.nodes.size
  def getNumEdges: Int = nodeList.edges.size

  /*def maskedMaxClique() : ArrayBuffer[Int] = {
  }*/
  /*def maxIssueSchedule(): Vec[UInt] = {
    val schedule = new Array(getNumNodes);
  }*/

  def pruneGraph(graph:Graph[Int,LUnDiEdge], actions: ArrayBuffer[Boolean]) :
  Graph[Int,LUnDiEdge] = {
    val nodesToRemove = (0 until actions.length).filter(act => actions(act) == false)
    var reducedDepGraph = nodeList -- nodesToRemove
    reducedDepGraph

  }

  def isNonConflicting(graph:Graph[Int,LUnDiEdge]) : Boolean = {
    (graph.nodes.toList).map(node => (node.degree == 0)).reduce(_ & _)}

  def isNonConflicting(actions:ArrayBuffer[Boolean]) : Boolean = {
    isNonConflicting(pruneGraph(nodeList, actions))
  }

  def isFullyConnected(graph:Graph[Int,LUnDiEdge],
    nodes:ArrayBuffer[Int]) : Boolean = {
    nodes.forall(x => {
      nodes.filter(_ != x).forall(y => graph.get(x)~|graph.get(y))})
  }

  def complementGraph(graph:Graph[Int,LUnDiEdge]) : Graph[Int,LUnDiEdge] = {
    var compGraph = scalax.collection.mutable.Graph[Int,LUnDiEdge]()
    for(a <- graph.nodes.toList){
      compGraph.add(a.value)
      for(b <- graph.nodes.toList.filter(
          b => (a.value != b.value && !(graph.get(a.value)~|graph.get(b.value))))){
        compGraph.addLEdge(a.value, b.value)("None")
      }
    }
    compGraph
  }

  def growClique(graph:Graph[Int,LUnDiEdge], currentClique:ArrayBuffer[Int],
                candidates:ArrayBuffer[Int]) : ArrayBuffer[Int] = {
    //If the new graph is not a clique, return 0 
    if (!isFullyConnected(graph,currentClique))
    //Otherwise, if we cannot grow further, we're done
      return ArrayBuffer[Int]()
    if (candidates == Nil)
      return currentClique

    //Add all candidates individuall to currentClique an reinvoke growClique
    //fold: compare each result, take the largest
    candidates.map(x => growClique(graph, ArrayBuffer[Int](x)++currentClique,
                                   candidates-x)).fold(currentClique)(
      (prev,cur) => if (cur.length > prev.length) cur else prev)
  }

  def maxClique(graph:Graph[Int,LUnDiEdge]) : ArrayBuffer[Int] = {
    val result = graph.nodes.toList.map(head =>
      growClique(graph, ArrayBuffer(head.value),
      ArrayBuffer[Int]()++(head.neighbors.toList.map(x => x.value)))
      ).foldLeft(ArrayBuffer[Int]())((prev, cur) =>{
        if (cur.length > prev.length) cur else prev})
    result
  }

  def generateSchedule() : ArrayBuffer[UInt] = {
    val scheduleBits = ArrayBuffer[UInt]()
    for(i <- 0 until 1 << getNumNodes) {
      //Mask the index to decide which nodes to prune 
      val predicateMask = ArrayBuffer[Boolean]()++(0 until getNumNodes).
        map(b => (i>>b)%2 == 1)
      //Take the complement: MaxClique is the complement of Max Independent Set
      val compGraph = complementGraph(pruneGraph(nodeList, predicateMask))
      val clique = maxClique(compGraph)
      scheduleBits+=UInt(clique.foldLeft(0)((mask, cur) =>
        mask + (1<<cur)),width=getNumNodes)
    }
    scheduleBits
  }
  def generateDot(graph:Graph[Int,LUnDiEdge], filename:String) : Unit = {
    var dot = "graph Actor_Dependency_Graph {\n"
    for(node <- graph.nodes.toList) {
      dot = dot + "  " + node.value + ";\n"
    }
    for(edgeInst <- graph.edges.toList) {
      val outerEdge = edgeInst.edge
      dot = dot + "  " + outerEdge._1 + " -- " + outerEdge._2
      val label = outerEdge.label.asInstanceOf[String]
      if (label.nonEmpty) dot = dot + " [label=" + label + "]"
      dot = dot + ";\n"
    }
    dot = dot + "}\n"
    val outputFile:Output = Resource.fromFile(filename)
    outputFile.write(dot)(Codec.UTF8)
  }
  def generateDot(filename:String = "depList.out") : Unit = generateDot(nodeList,filename)
}
