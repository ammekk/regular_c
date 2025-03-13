
import scala.collection.immutable.HashMap
import scala.io.Source
object Main extends App{
  def parseNFA(code: String): Seq[NFAParsed] = {
    val parsed = Parser(code)
    parsed.left.foreach(error => println(s"\'$code\' parsing error: $error"))
    //parsed.toSeq.foreach(expr => println(expr))
    parsed.toSeq
  }
  def readFile(file: String): String = {
    val source = Source.fromFile(file)
    source.getLines().mkString("")
  }
  def getAlphabet(domainArg: String): List[String] = {
    domainArg.substring(1, domainArg.length() - 1).split(',').toList
  }
  val parsed = parseNFA(readFile(args(0))).head
  val CFG = ControlFlowGraphFunctions.makeGraph(parsed.statements)
  CFG.visualizeGraph("ControlFlowGraph")
}
