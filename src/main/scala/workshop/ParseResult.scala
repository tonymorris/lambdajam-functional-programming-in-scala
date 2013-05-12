sealed trait ParseResult[A] {
  def isFail: Boolean =
    this match {
      case ParseFail(_) => true
      case ParseValue(_) => false
    }

  def isValue: Boolean =
    !isFail

  def map[B](f: A => B): ParseResult[B] =
    this match {
      case ParseFail(m) => ParseFail(m)
      case ParseValue(v) => ParseValue(f(v))
    }

  def flatMap[B](f: A => ParseResult[B]): ParseResult[B] =
    this match {
      case ParseFail(m) => ParseFail(m)
      case ParseValue(v) => f(v)
    }

  def ap[B](f: ParseResult[A => B]): ParseResult[B] =
    for {
      ff <- f
      aa <- this
    } yield ff(aa)

  def message: Option[String] =
    this match {
      case ParseFail(m) => Some(m)
      case ParseValue(_) => None
    }

  def value: Option[A] =
    this match {
      case ParseFail(_) => None
      case ParseValue(r) => Some(r)
    }

  def |(a: => A): A =
    value getOrElse a
}
case class ParseFail[A](m: String) extends ParseResult[A]
case class ParseValue[A](v: A) extends ParseResult[A]

object ParseResult {
  def apply[A](a: A): ParseResult[A] =
    ParseValue(a)
}
