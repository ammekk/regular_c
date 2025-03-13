import scala.collection.immutable.HashMap
import java.io.{BufferedWriter, File, FileWriter}

case class IntermediateGraph(private val idToNode: HashMap[String, Node],
                             private val transitions: Set[(String, String, EdgeLabel)], private val startNode: String){
  def getIdtoNodes: HashMap[String, Node] = {
    idToNode
  }
  def getTransitions: Set[(String, String, EdgeLabel)] = {
    transitions
  }
  def getStartNode(): String = {
    return idToNode.filter{case (id, _) => id.contains("Start")}.keys.head
  }
  def findNumNodes(): Integer = {
    return idToNode.size
  }
  def visualizeGraph(filename: String): Unit = {
    def getLabelExpression(expr: Expression): String = {
      expr match {
        case InputExpression() =>
          "input"
        case CharExpression(value) =>
          value.toString
        case EOFExpression() =>
          "EOF"
        case NumericExpression(value) =>
          value.toString
        case StringExpression(value) =>
          value
      }
    }

    def getLabelBool(bool: EdgeLabel): String = {
      bool match {
        case ParenBool(value) =>
          "(" + getLabelBool(value) + ")"
        case ComplementBool(value) =>
          "not (" + getLabelBool(value) + ")"
        case OrBool(lhs, rhs) =>
          getLabelBool(lhs) + " or " + getLabelBool(rhs)
        case AndBool(lhs, rhs) =>
          getLabelBool(lhs) + " and " + getLabelBool(rhs)
        case EqualityBool(lhs, rhs) =>
          getLabelExpression(lhs) + " == " + getLabelExpression(rhs)
        case FlipCoinBool() =>
          "flip_coin()"
        case ValueBool(value) =>
          value.toString
        case Epsilon() => "\u03B5"
      }
    }

    def getLabel(connection: EdgeLabel): String = {
      val cond = connection
      " [label = \"" + getLabelBool(connection) + "\"]"
    }

    val file = new File(filename +".gv.txt")
    val bw = new BufferedWriter(new FileWriter(file))
    var hm = new HashMap[String, List[String]]()
    var concatenate_graph = "digraph " + filename + " {\n \tfontname=\"Helvetica\"\n" +
      "\tnode [fontsize=100 fontname=\"Helvetica\" style=\"filled\" color=\"plum1\" shape = star]\n" +
      "\tedge [fontsize=100 fontname=\"Helvetica\" style=bold arrowsize=6 penwidth=6 arrowhead=vee]\n" +
      "\trankdir=LR;\n"
    var set = ""
    transitions.foreach { case (a, b, c) => println(s"Trans connects $a to $b with cond $c") }
    transitions.foreach{ case (node, nextNode, cond) =>
      set += node + " -> " + nextNode + getLabel(cond) + ";\n"}
    bw.write(concatenate_graph + set + "}")
    bw.close()
  }
}



case class ReadNode(id: String, cond: EdgeLabel) extends Node
case class BranchNode(id: String, cond:EdgeLabel) extends Node
case class StartNode(id: String, cond: EdgeLabel) extends Node
case class ReturnNode(id: String, cond: EdgeLabel) extends Node
class Node
