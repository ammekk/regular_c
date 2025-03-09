import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharSequenceReader

object Parser extends RegexParsers {
  private val outputChar = "[A-Za-z0-9_-]".r // Want to have regex that matches any non whitespace character but can't find one that works
  private val outputNumera = "[0-9]".r

  def nfa: Parser[NFAParsed] = rep(statement) ^^ (sts => NFAParsed(sts))

  def statement: Parser[Statement] = {
    val readInput = "input = read();" ^^ (_ => ReadInputStatement())
    val assert = "assert(" ~> orBool ~ ")" <~ ";" ^^ { case bl~_ => AssertStatement(bl)}
    val ifElse = "if(" ~ andBool ~ ")" ~ "{" ~ rep(statement) ~ "}" ~ opt("else"~> "{" ~ rep(statement) ~ "}") ^^
      { case _ ~ ifBool ~ _ ~ _~ ifStmts ~ _ ~ Some(_ ~ elseStmts ~ _) => IfElseStatement(ifBool, ifStmts, elseStmts)
      case _ ~ ifBool ~ _ ~ _ ~ ifStmts ~ _ ~ None => IfElseStatement(ifBool, ifStmts, List()) }
    val whileStmt = "while(" ~ andBool ~ ")" ~ "{" ~ rep(statement) ~ "}" ^^ { case _~cond~_~_~stmts~_ =>
      WhileStatement(cond, stmts) }
    val returnStmt = "return" ~ andBool ~";" ^^ { case _ ~ returnBool ~ _ => ReturnStatement(returnBool) }
    readInput | assert | ifElse | whileStmt | returnStmt
  }

  def andBool: Parser[Bool] = {
    def and: Parser[Bool] = rep1sep(bool, "and".r) ^^ (_.reduceLeft(AndBool))
    val parenBool = "(" ~> orBool <~ ")" ^^ (pb => ParenBool(pb))
    val complement = "not " ~> orBool ^^ (not => ComplementBool(not))
    and | bool | parenBool | complement
  }
  def orBool: Parser[Bool] = {
    def or: Parser[Bool] = rep1sep(andBool, "or".r) ^^ (_.reduceLeft(OrBool))
    or | andBool
  }

  def bool: Parser[Bool] = {
    val trueValue = "true" ^^ { case value => ValueBool(value.toBoolean)}
    val falseValue = "false" ^^ { case value => ValueBool(value.toBoolean) }
    val flipCoin = "flip_coin()" ^^ { case flpcn => FlipCoinBool() }
    val equality = expression ~ "==" ~ expression ^^ { case l ~_~ r => EqualityBool(l, r)}
    trueValue | falseValue | flipCoin | equality  // or | and
  }

  def expression: Parser[Expression] = {
    val charExp = "'" ~> outputChar <~ "'" ^^ { case charValue => CharExpression(charValue.toCharArray.head) }
    val numExp = outputNumera ^^ { case numValue => NumericExpression(numValue.toInt)}
    val inputExp = "input" ^^ { case _ => InputExpression() }
    val EOFExp = "EOF" ^^ { case _ => EOFExpression() }
    val StringExp = "\"" ~> outputChar <~ "\"" ^^ { case stringValue => StringExpression(stringValue) }
    charExp | numExp | inputExp | EOFExp
  }


  def apply(code: String): Either[ParserError, NFAParsed] = {
    parseAll(nfa, new CharSequenceReader(code)) match {
      case Success(result, next) => Right(result)
      case NoSuccess(msg, next) => Left(ParserError(msg))
    }
  }
  case class ParserError(msg: String)
}

case class InputExpression() extends Expression
case class CharExpression(value: Char) extends Expression
case class StringExpression(value: String) extends Expression
case class NumericExpression(value: Int) extends Expression
case class EOFExpression() extends Expression
trait Expression
case class ParenBool(value: Bool) extends Bool
case class ComplementBool(value: Bool) extends Bool
case class OrBool(lhs: Bool, rhs: Bool) extends Bool
case class AndBool(lhs: Bool, rhs: Bool) extends Bool
case class EqualityBool(lhs: Expression, rhs: Expression) extends Bool
case class FlipCoinBool() extends Bool
case class ValueBool(value: Boolean) extends Bool
trait Bool
case class WhileStatement(cond: Bool, stmts: List[Statement]) extends Statement
case class IfElseStatement(cond: Bool, ifStmts: List[Statement], elseStmts: List[Statement]) extends Statement
case class AssertStatement(cond: Bool) extends Statement
case class ReadInputStatement() extends Statement
case class ReturnStatement(cond: Bool) extends Statement
trait Statement
case class NFAParsed(statements: List[Statement])

