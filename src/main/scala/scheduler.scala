package Xactor

import Chisel._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scalax.collection.mutable.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import scalax.collection.edge.LUnDiEdge
import scalax.collection.edge.Implicits._
import scalax.io._

class Scheduler {
  private var nodeList = scalax.collection.mutable.Graph[Int,LUnDiEdge]()
  implicit val factory = scalax.collection.edge.LUnDiEdge

  def registerDepList(depList: HashMap[String, ArrayBuffer[Int]], typ:String) : Unit = {
  for ((k,v) <- depList) {
    for (parent <- v.tails; if parent != Nil){
      for(child <- parent.tail) {
        nodeList.addLEdge(parent.head,child)(typ)
        }
      }
    }
  }

  def generateDot() : Unit = {
    //Until someone figures out how to make the library work
    var dot = "graph Actor_Dependency_Graph {\n"
    for(node <- nodeList.nodes.toList) {
      dot = dot + "  " + node.value + ";\n"
    }
    for(edgeInst <- nodeList.edges.toList) {
      val outerEdge = edgeInst.edge
      dot = dot + "  " + outerEdge._1 + " -- " + outerEdge._2
      val label = outerEdge.label.asInstanceOf[String]
      if (label.nonEmpty) dot = dot + " [label=" + label + "]"
      dot = dot + ";\n"
    }
    dot = dot + "}\n"

    val outputFile:Output = Resource.fromFile("dot.out")
    outputFile.write(dot)(Codec.UTF8)
  }
}


