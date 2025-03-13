trait EdgeLabel
case class ParenBool(value: Bool) extends Bool
case class ComplementBool(value: Bool) extends Bool
case class OrBool(lhs: Bool, rhs: Bool) extends Bool
case class AndBool(lhs: Bool, rhs: Bool) extends Bool
case class EqualityBool(lhs: Expression, rhs: Expression) extends Bool
case class FlipCoinBool() extends Bool
case class ValueBool(value: Boolean) extends Bool
class Bool extends EdgeLabel

case class Epsilon() extends EdgeLabel



