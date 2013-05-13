package workshop

case class ParseFail[A](m: String) extends ParseResult[A]
case class ParseValue[A](v: A) extends ParseResult[A]
/*
A ParseResult[A] is either a fail String or value A.
*/
sealed trait ParseResult[A] {
  /*
  Exercise 1
  ----------
  Return whether or not this value is a fail.
  */
  def isFail: Boolean =
    this match {
      case ParseFail(_) => true
      case ParseValue(_) => false
    }

  /*
  Exercise 2
  ----------
  Return whether or not this value is a fail.
  ~~~ use isFail.
  */
  def isValue: Boolean =
    !isFail

  /*
  Exercise 3
  ----------
  Map a function across this parse result.
  Two laws must satisfy:
    1) r.map(z => z) == r
    2) r.map(z => f(g(z))) == r.map(g).map(f)
  Ensure these laws are satisfied in the implementation by code review.
  */
  def map[B](f: A => B): ParseResult[B] =
    this match {
      case ParseFail(m) => ParseFail(m)
      case ParseValue(v) => ParseValue(f(v))
    }

  /*
  Exercise 4
  ----------
  FlatMap a function across this parse result.
  One law must satisfy:
    * r.flatMap(f).flatMap(g) == r.flatMap(z => f(z).flatMap(g))
  Ensure this law is satisfied in the implementation by code review.
  */
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

  /*
  Exercise 5
  ----------
  Zip this parse result with the given parse result to produce a parse result of pair.
  There are several ways of achieving this:
  ~~~ Use flatMap and map.
  ~~~ Use ap and map.
  */
  def zip[B](b: ParseResult[B]): ParseResult[(A, B)] =
    flatMap(a => b map ((a, _)))

  /*
  Exercise 6
  ----------
  Return the possible fail message held by this parse result.
  */
  def message: Option[String] =
    this match {
      case ParseFail(m) => Some(m)
      case ParseValue(_) => None
    }

  /*
  Exercise 7
  ----------
  Return the possible value held by this parse result.
  */
  def value: Option[A] =
    this match {
      case ParseFail(_) => None
      case ParseValue(r) => Some(r)
    }

  /*
  Exercise 8
  ----------
  If this is a fail, modify the message with the given function. Otherwise, leave unchanged.
  */
  def withfail(e: String => String): ParseResult[A] =
    this match {
      case ParseFail(m) => ParseFail(e(m))
      case ParseValue(r) => ParseValue(r)
    }

  /*
  Exercise 9
  ----------
  If this is a fail, set the message to the given parameter.
  Otherwise, leave unchanged.
  */
  def fail(e: => String): ParseResult[A] =
    withfail(_ => e)

}

object ParseResult {
  /*
  Exercise 10
  -----------
  Given a list of parse results, return a parse result of list.
  This can be done by "sequencing"; look at the parse result on the list head,
  and sequence its value to the tail.

  ~~~ Use flatMap, map. Use explicit recursion.
  */
  def sequence[A](a: List[ParseResult[A]]): ParseResult[List[A]] =
    a match {
      case Nil => ParseValue(Nil)
      case h::t => for {
                     q <- h
                     r <- sequence(t)
                   } yield q :: r
    }

}
