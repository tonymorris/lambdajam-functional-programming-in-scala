package workshop

import Parser._

case class Parser[A](run: In => ParseResult[(In, A)]) {
  def parse(i: String): ParseResult[A] =
    run(i.toList) map (_._2)

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

  def separation[B](p: Parser[B]): Parser[List[A]] = {
    val s = for {
              h <- this
              t <- flatMap(_ => this).many
            } yield h::t
    s | Parser.value(Nil)
  }


  def mapresult(r: ParseResult[(In, A)] => ParseResult[(In, A)]): Parser[A] =
    Parser(r compose run)

  def withfail(e: String => String): Parser[A] = 
    mapresult(_ withfail e)

  def setfail(e: => String): Parser[A] =
    withfail(_ => e)
} 

object Parser {
  type In = List[Char]

  def value[A](a: => A): Parser[A] =
    Parser(i => ParseResult.value(i, a))

  def fail[A](m: => String): Parser[A] =  
    Parser(_ => ParseFail(m))

  def character: Parser[Char] =
    Parser {
      case Nil => ParseFail("Unexpected end of stream")
      case h::t => ParseValue((t, h))
    }

  def characters: Parser[List[Char]] =
    character.many

  def satisfyoption(p: Char => Option[String]): Parser[Char] = 
    character flatMap (c => 
      p(c) match {
        case None => value(c)
        case Some(m) => fail(m)
      })

  def satisfy(p: Char => Boolean): Parser[Char] =
    satisfyoption(c => if(p(c)) None else Some("Unexpected character '" + c + "'"))

  def is(x: Char): Parser[Char] =
    satisfyoption(c => if(c == x) None else Some("Unexpected character '" + c + "' Expecting '" + x + "'"))

  def space: Parser[Char] =
    satisfyoption(c => if(c.isWhitespace) None else Some("Unexpected character '" + c + "' Expecting whitespace"))

  def spaces: Parser[List[Char]] =
    space.many

  def spaces1: Parser[List[Char]] = 
    space.many1

  def lower: Parser[Char] =
    satisfyoption(c => if(c.isLower) None else Some("Unexpected character '" + c + "'. Expecting lowercase"))

  def upper: Parser[Char] =
    satisfyoption(c => if(c.isUpper) None else Some("Unexpected character '" + c + "'. Expecting uppercase"))

  def letter: Parser[Char] =
    satisfyoption(c => if(c.isLetter) None else Some("Unexpected character '" + c + "'. Expecting letter"))

  def sequence[A](ps: List[Parser[A]]): Parser[List[A]] =
    ps match {
      case Nil => value(Nil)
      case h::t => for {
                     q <- h
                     r <- sequence(t)
                   } yield q :: r
    }

  def replicate[A](n: Int, p: Parser[A]): Parser[List[A]] =
    sequence(List.fill(n)(p))

  def list(s: List[Char]): Parser[List[Char]] = 
    sequence(s map is)

  def string(s: String): Parser[String] =
    list(s.toList) map (_.mkString)

}
