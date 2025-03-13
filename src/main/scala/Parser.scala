import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharSequenceReader

object Parser extends RegexParsers {
  private val outputChar = "[A-Za-z0-9_-]".r // Want to have regex that matches any non whitespace character but can't find one that works
  private val outputNumera = "[0-9]".r
  private var numStatement = 0

  def nfa: Parser[NFAParsed] = rep(statement) ^^ (sts => NFAParsed(sts))

  def expression: Parser[Expression] = {
    val charExp = "'" ~> outputChar <~ "'" ^^ { case charValue => CharExpression(charValue.toCharArray.head) }
    val numExp = outputNumera ^^ { case numValue => NumericExpression(numValue.toInt) }
    val inputExp = "input" ^^ { case _ => InputExpression() }
    val EOFExp = "EOF" ^^ { case _ => EOFExpression() }
    val StringExp = "\"" ~> outputChar <~ "\"" ^^ { case stringValue => StringExpression(stringValue) }
    charExp | numExp | inputExp | EOFExp
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
    val trueValue = "true" ^^ { case value => ValueBool(value.toBoolean) }
    val falseValue = "false" ^^ { case value => ValueBool(value.toBoolean) }
    val flipCoin = "flip_coin()" ^^ { case flpcn => FlipCoinBool() }
    val equality = expression ~ "==" ~ expression ^^ { case l ~ _ ~ r => EqualityBool(l, r) }
    trueValue | falseValue | flipCoin | equality // or | and
  }

  def statement: Parser[Statement] = {
    val readInput = ("input = read();" ^^ { _ =>
      val stmt = ReadInputStatement(numStatement.toString)
      numStatement += 1
      stmt
    })

    val assert = ("assert(" ~> orBool <~ ")" <~ ";") ^^ { bl =>
      val stmt = AssertStatement(numStatement.toString, bl)
      numStatement += 1
      stmt
    }

    val returnStmt = ("return" ~> orBool <~ ";") ^^ { returnBool =>
      val stmt = ReturnStatement(numStatement.toString, returnBool)
      numStatement += 1
      stmt
    }

    val ifElse = ("if" ~ "(" ~ andBool ~ ")" ~ "{" ~ rep(statement) ~ "}" ~ opt("else" ~ "{" ~ rep(statement) ~ "}")) ^^ {
      case _ ~ _ ~ ifBool ~ _ ~ _ ~ ifStmts ~ _ ~ None =>
        numStatement += 1
        IfElseStatement(numStatement.toString, ifBool, ifStmts, List())

      case _ ~ _ ~ ifBool ~ _ ~ _ ~ ifStmts ~ _ ~ Some(_ ~ _ ~ elseStmts ~ _) =>
        numStatement += 1
        IfElseStatement(numStatement.toString, ifBool, ifStmts, elseStmts)
    }

    val whileStmt = ("while" ~ "(" ~ andBool ~ ")" ~ "{" ~ rep(statement) ~ "}") ^^ {
      case _ ~ _ ~ cond ~ _ ~ _ ~ stmts ~ _ =>
        val stmt = WhileStatement(numStatement.toString, cond, stmts)
        numStatement += 1
        stmt
    }

    readInput | assert | returnStmt | ifElse | whileStmt
  }


  def apply(code: String): Either[ParserError, NFAParsed] = {
    parseAll(nfa, new CharSequenceReader(code)) match {
      case Success(result, next) => Right(result)
      case NoSuccess(msg, next) => Left(ParserError(msg))
    }
  }
  case class ParserError(msg: String)
}

case class NFAParsed(statements: List[Statement])

