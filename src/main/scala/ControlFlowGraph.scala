import scala.collection.immutable.HashMap

object ControlFlowGraphFunctions{

  def makeGraph(stmts: List[Statement]): IntermediateGraph = {
      val (idToNode, statementToId, numNodes) = nameCreateNodesAndPairStatements(stmts, HashMap[String, Node](),
        HashMap[Statement, String](), 1)
      val (firstNode, transitions) = connectGraph(stmts, statementToId)
      // Assumes there is only one firstNode
      print()
      val startId = "Start0"
      IntermediateGraph(idToNode +
        (startId -> StartNode(startId, Epsilon())), transitions + ((startId, firstNode, Epsilon())), startId)
  }

  def findMostInnerStatementForBranching(stmts: List[Statement],
                                         statementToId: HashMap[Statement, String]): List[(String, EdgeLabel)] = {
//   def findFirstIfsToConnectNegToOuterStmts(stmtsFirst: List[Statement]) : List[(String, EdgeLabel)] = {
//    if (stmtsFirst.isEmpty && ) return List[(String, EdgeLabel)]()
//    stmtsFirst.head match {
//      case a@IfElseStatement(_, cond, ifStmts, elseStmts) =>
//        if (elseStmts.isEmpty){
//          val curFirstInnerIf = List((statementToId(a), ComplementBool(cond)))
//          val ifFirstInnerIfs = findFirstIfsToConnectNegToOuterStmts(ifStmts)
//          val elseFirstInnerIfs = findFirstIfsToConnectNegToOuterStmts(elseStmts)
//          curFirstInnerIf ++ ifFirstInnerIfs ++ elseFirstInnerIfs
//        }
//        else Nil
//      case _ => Nil
//    }
//  }

    if (stmts.isEmpty) return List()

    stmts.last match {
      case a@ReadInputStatement(_) => List((statementToId(a), ValueBool(true)))
      case a@AssertStatement(_, cond) => List((statementToId(a), cond))
      case a@IfElseStatement(_, cond, ifStmts, elseStmts) =>
        val newNestedVals =
          if (ifStmts.isEmpty && elseStmts.isEmpty) List((statementToId(a), ComplementBool(cond)), (statementToId(a), cond))
          else if (ifStmts.isEmpty) List((statementToId(a), cond))
          else if (elseStmts.isEmpty) List((statementToId(a), ComplementBool(cond)))
          else List()

        val processedInnerStatements = findMostInnerStatementForBranching(ifStmts, statementToId)
        val processedElseStatements = findMostInnerStatementForBranching(elseStmts, statementToId)

        newNestedVals ++ processedInnerStatements ++ processedElseStatements

      case a@WhileStatement(_, cond, _) => List((statementToId(a), ComplementBool(cond)))
      case _ => Nil
    }
}


  private def nameCreateNodesAndPairStatements(stmts: List[Statement],
                         nodesToId: HashMap[String, Node],
                         statementToId: HashMap[Statement, String], numNodes: Integer):
  (HashMap[String, Node], HashMap[Statement, String], Int) = {
    stmts match {
      case Nil => (nodesToId, statementToId, numNodes)
      case stmt :: rest =>
        val id = stmt match {
          case _: ReadInputStatement => s"Read$numNodes"
          case _: AssertStatement => s"Assert$numNodes"
          case IfElseStatement(_, _, _, elseStmts) =>
            if (elseStmts.isEmpty) s"If$numNodes" else s"IfElse$numNodes"
          case _: WhileStatement => s"While$numNodes"
          case _: ReturnStatement => s"Return$numNodes"
        }
        def processBranch(stmts: List[Statement], numNodes: Int): (HashMap[String, Node], HashMap[Statement, String], Int) = {
          nameCreateNodesAndPairStatements(stmts, nodesToId, statementToId, numNodes)
        }
        stmt match {
          case a@ReadInputStatement(_) =>
            val (tempNtoId, tempStToId, newNum) = nameCreateNodesAndPairStatements(rest, nodesToId, statementToId, numNodes + 1)
            (tempNtoId + (id -> ReadNode(id, ValueBool(true))), tempStToId + (a -> id), newNum)

          case a@AssertStatement(_, cond) =>
            val (tempNtoId, tempStToId, newNum) = nameCreateNodesAndPairStatements(rest, nodesToId, statementToId, numNodes + 1)
            (tempNtoId + (id -> BranchNode(id, cond)), tempStToId + (a -> id), newNum)

          case a@IfElseStatement(_, cond, ifStmts, elseStmts) =>
            val (ifNodes, ifStmtsToId, num1) = processBranch(ifStmts, numNodes + 1)
            val (elseNodes, elseStmtsToId, num2) = processBranch(elseStmts, num1)
            val (restNodes, restStmtsToId, num3) = nameCreateNodesAndPairStatements(rest, nodesToId, statementToId, num2)

            (ifNodes ++ elseNodes ++ restNodes + (id -> ReadNode(id, cond)),
              ifStmtsToId ++ elseStmtsToId ++ restStmtsToId + (a -> id),
              num3)

          case a@WhileStatement(_, cond, whileStmts) =>
            val (whileNodes, whileStmtsToId, num1) = processBranch(whileStmts, numNodes + 1)
            val (restNodes, restStmtsToId, num2) = nameCreateNodesAndPairStatements(rest, nodesToId, statementToId, num1)

            (whileNodes ++ restNodes + (id -> BranchNode(id, cond)),
              whileStmtsToId ++ restStmtsToId + (a -> id),
              num2)

          case a@ReturnStatement(_, cond) =>
            (nodesToId + (id -> ReturnNode(id, cond)), statementToId + (a -> id), numNodes + 1)
        }
      }
    }

  def connectGraph(stmts: List[Statement], statementToId: HashMap[Statement, String]):
  (String, Set[(String, String, EdgeLabel)]) = {

    def transToNode(node: String, cond: EdgeLabel, rest: List[Statement], isConnected: Boolean = true)
    : Set[(String, String, EdgeLabel)] = {
      if (!isConnected) connectGraph(rest,statementToId)._2
      else {
        val (nextNode, transitions) = connectGraph(rest, statementToId)
        val extraEdge = if (nextNode.nonEmpty) Set((node, nextNode, cond)) else Set.empty
        transitions ++ extraEdge
      }
    }

    stmts match {
      case Nil => ("", Set.empty[(String, String, EdgeLabel)])

      case stmt :: rest =>
        val id = statementToId(stmt)
        stmt match {
          case a@ReadInputStatement(_) =>
            val stmtId = statementToId(a)
            val nextTransitions = transToNode(stmtId, ValueBool(true), rest)
            (stmtId, nextTransitions)

          case a@AssertStatement(_, cond) =>
            val stmtId = statementToId(a)
            val nextTransitions = transToNode(stmtId, cond, rest)
            (stmtId, nextTransitions)

          case a@IfElseStatement(_, cond, ifStmts, elseStmts) =>
            val stmtId = statementToId(a)

            val posBranchTrans = transToNode(stmtId, cond, ifStmts)

            val negBranchTrans = if (elseStmts.nonEmpty)  transToNode(stmtId, ComplementBool(cond), elseStmts)
            else transToNode(stmtId, ComplementBool(cond), rest)
            val restTrans = if (elseStmts.nonEmpty) transToNode("", cond, rest, false)
            else Set.empty[(String, String, EdgeLabel)]
            val (connectToNextStmtIf, connectToNextStmtElse) = (
              ControlFlowGraphFunctions.findMostInnerStatementForBranching(ifStmts, statementToId),
              ControlFlowGraphFunctions.findMostInnerStatementForBranching(elseStmts, statementToId)
            )

            val innerStmtsTrans = rest.headOption.map { nextStmt =>
              (connectToNextStmtIf ++ connectToNextStmtElse)
                .map { case (id, bool) => (id, statementToId(nextStmt), bool) }.toSet
            }.getOrElse(Set.empty)
            (stmtId, posBranchTrans ++ negBranchTrans ++ innerStmtsTrans ++ restTrans)

          case a@WhileStatement(_, cond, whileStmts) =>
            val stmtId = statementToId(a)

            val posBranchTrans = transToNode(stmtId, cond, whileStmts)
            val negBranchTrans = transToNode(stmtId, ComplementBool(cond), rest)

            val connectToWhileNodes = findMostInnerStatementForBranching(whileStmts, statementToId)
            val transToWhileInnerBranching = connectToWhileNodes
              .map { case (node, cond) => (node, stmtId, cond) }.toSet

            (stmtId, posBranchTrans ++ negBranchTrans ++ transToWhileInnerBranching)

          case a@ReturnStatement(_, cond) =>
            val stmtId = statementToId(a)
            val selfLoop: Set[(String, String, EdgeLabel)] = Set((stmtId, stmtId, ValueBool(true)))
            (stmtId, selfLoop)
        }
    }
  }
}
