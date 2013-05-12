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

  def zip[B](b: Parser[B]): Parser[(A, B)] =  
    flatMap (a => b map ((a, _)))

  def |(p: => Parser[A]): Parser[A] =
    Parser(i => run(i) match {
      case ParseFail(_) => p run i
      case ParseValue(a) => ParseValue(a)
    })

  def many: Parser[List[A]] =
    Parser(i => run(i) match {
      case ParseFail(_) => ParseValue((Nil, Nil))
      case ParseValue((j, a)) => many map (a :: _) run j
    })

  def many1: Parser[List[A]] =
    for {
      h <- this
      t <- this.many
    } yield h :: t
  

} 

object Parser {
  type In = List[Char]

  def value[A](a: => A): Parser[A] =
    Parser(i => ParseResult.value(i, a))
}
