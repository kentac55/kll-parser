import scala.util.control.Exception._
import scala.util.matching.Regex
import scala.util.parsing.combinator.{JavaTokenParsers, RegexParsers}

import KeyTable._

object KllParser extends RegexParsers with JavaTokenParsers {

  val commentRegex: Regex  = ".*".r
  val variableRegex: Regex = "[A-Za-z0-9]+".r
  val hexRegex: Regex      = "0x[A-Fa-f0-9]+".r
  val decRegex: Regex      = "\\d+".r
  val analogRegex: Regex   = "(100|[1-9]?[0-9])".r

  def line: Parser[Line] = formula <~ ";" | comment

  def formula: Parser[Formula] = variables | keymap

  def comment: Parser[Comment] = "#" ~> commentRegex ^^ Comment

  def variables: Parser[Variable] =
    (variableRegex | stringLiteral) ~ "=" ~ (variableRegex | stringLiteral) ^^ {
      case key ~ "=" ~ value =>
        Variable("\"(.*)\"".r.replaceAllIn(key, "$1"), "\"(.*)\"".r.replaceAllIn(value, "$1"))
    }

  def defines: Parser[Any] = ???

  def capability: Parser[Capability] = ???

  def keymap: Parser[KeyMap] = trigger ~ ":" ~ result ^^ {
    case trigger ~ ":" ~ result => KeyMap(trigger, result)
  }

  def trigger: Parser[Trigger] = (scanCode | usbCode) ^^ {
    case Right(c) => c
    case Left(e)  => TriggerError(e.getMessage)
  }

  def scanCode: Parser[Either[Throwable, ScanCode]] =
    "S" ~> (hex | dec) ~ opt(analog) ^^ {
      case s ~ a => s.map(v => ScanCode(Key(v, None) :: Nil, a.getOrElse(Analog.MAX)))
    }

  def usbCode: Parser[Either[Throwable, USBCode]] =
    "U" ~> (hex | dec | word) ~ opt(analog) ^^ {
      case u ~ a => u.map(v => USBCode(Key(v, None) :: Nil, a.getOrElse(Analog.MAX)))
    }

  def analog: Parser[Int] =
    "(" ~> analogRegex <~ ")" ^^ { Integer.parseInt }

  private def hex: Parser[Either[Throwable, Int]] =
    hexRegex ^^ (x => allCatch.either(Integer.parseInt(x.drop(2), 16)))
  private def dec: Parser[Either[Throwable, Int]] =
    decRegex ^^ (x => allCatch.either(Integer.parseInt(x)))
  private def word: Parser[Either[Throwable, Int]] =
    stringLiteral ^^ (x => allCatch.either(name2UsbCode(x.drop(1).dropRight(1))))

  def result: Parser[Result] = ???

}
