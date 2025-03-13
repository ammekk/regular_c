case class InputExpression() extends Expression
case class CharExpression(value: Char) extends Expression
case class StringExpression(value: String) extends Expression
case class NumericExpression(value: Int) extends Expression
case class EOFExpression() extends Expression
class Expression
case class WhileStatement(id: String, cond: Bool, stmts: List[Statement]) extends Statement
case class IfElseStatement(id: String, cond: Bool, ifStmts: List[Statement], elseStmts: List[Statement]) extends Statement
case class AssertStatement(id: String, cond: Bool) extends Statement
case class ReadInputStatement(id: String) extends Statement
case class ReturnStatement(id: String, cond: Bool) extends Statement
class Statement {
  override def hashCode(): Int = System.identityHashCode(this)
}
