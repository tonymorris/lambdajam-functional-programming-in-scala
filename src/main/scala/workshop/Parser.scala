package workshop

import Parser._

case class Parser[A](run: In => ParseResult[(In, A)]) {
  def parse(i: In): ParseResult[A] =
    run(i) map (_._2)

  def map[B](f: A => B): Parser[B] =
    Parser(run(_) map {
      case (j, a) => (j, f(a))
    })

  def flatMap[B](f: A => Parser[B]): Parser[B] =
    Parser(i => run(i) match { 
      case ParseFail(m) => ParseFail(m)
      case ParseValue((rest, a)) => f(a) run rest
    })

  def ap[B](f: Parser[A => B]): Parser[B] =
    for {
      ff <- f
      aa <- this
    } yield ff(aa)

  def |(p: => Parser[A]): Parser[A] =
    Parser(i => run(i) match {
      case ParseFail(_) => p run i
      case ParseValue(a) => ParseValue(a)
    })
} 

object Parser {
  type In = List[Char]

  def apply[A](a: => A): Parser[A] =
    Parser(i => ParseResult(i, a))
}
