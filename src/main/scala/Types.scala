sealed trait Kll
sealed trait Line    extends Kll
sealed trait Elem    extends Kll
sealed trait Formula extends Line
sealed trait Ignorable

case class Comment(value: String) extends Line with Ignorable

case class KeyMap(trigger: Trigger, result: Result) extends Formula
case class Variable(key: String, value: String)     extends Formula

sealed trait Trigger extends Elem
sealed trait Result  extends Elem

case class Key(value: Int, analog: Option[Int] = None)

case class ScanCode(value: List[Key], analog: Int = Analog.MAX) extends Trigger
case class USBCode(value: List[Key], analog: Int = Analog.MAX)  extends Trigger with Result
case class KllRange()                                           extends Trigger
case class Combination()                                        extends Trigger with Result
case class KllNone()                                            extends Result
case class CCC()                                                extends Result
case class SCC()                                                extends Result
case class KllSeq()                                             extends Result
case class Capability()                                         extends Result

sealed trait KllError extends Ignorable {
  val msg: String
}
case class TriggerError(msg: String) extends Trigger with KllError
case class ResultError(msg: String)  extends Result with KllError

object Analog extends Trigger {
  val MIN = 0
  val MAX = 100
}
