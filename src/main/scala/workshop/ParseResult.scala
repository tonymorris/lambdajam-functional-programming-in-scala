package workshop

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

  def withfail(e: String => String): ParseResult[A] =
    this match {
      case ParseFail(m) => ParseFail(e(m))
      case ParseValue(r) => ParseValue(r)
    }

  def fail(e: => String): ParseResult[A] =
    withfail(_ => e)

}
case class ParseFail[A](m: String) extends ParseResult[A]
case class ParseValue[A](v: A) extends ParseResult[A]

object ParseResult {
  def sequence[A](a: List[ParseResult[A]]): ParseResult[List[A]] =
    a match {
      case Nil => ParseValue(Nil)
      case h::t => for {
                     q <- h
                     r <- sequence(t)
                   } yield q :: r
    }

}
