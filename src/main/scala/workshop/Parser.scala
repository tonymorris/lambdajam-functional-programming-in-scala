package workshop

import Parser._

/*
A parser is a function that takes a list of characters (input).
The function returns either a fail parse result, or a 
value containing the remaining input and a value.
*/
case class Parser[A](run: In => ParseResult[(In, A)]) {
  def parse(i: String): ParseResult[A] =
    run(i.toList) map (_._2)

  /*
  Exercise 11
  -----------
  Map a function across this parser.
  Two laws must satisfy:
    1) r.map(z => z) == r
    2) r.map(z => f(g(z))) == r.map(g).map(f)
  Ensure these laws are satisfied in the implementation by code review.
  */
  def map[B](f: A => B): Parser[B] =
    Parser(run(_) map {
      case (j, a) => (j, f(a))
    })

  /*
  Exercise 12
  -----------
  FlatMap a function across this parser.
  One law must satisfy:
    * r.flatMap(f).flatMap(g) == r.flatMap(z => f(z).flatMap(g))
  Ensure this law is satisfied in the implementation by code review.
  */
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

  /*
  Exercise 13
  -----------
  Zip this parser with the given parser to produce a parser of pair.

  There are several ways of achieving this:
  ~~~ Use flatMap and map.
  ~~~ Use ap and map.
  */
  def zip[B](b: Parser[B]): Parser[(A, B)] =  
    flatMap (a => b map ((a, _)))

  /*
  Exercise 14
  -----------
  Try this parser. If it fails, try the given parser with the same input.
  */
  def |(p: => Parser[A]): Parser[A] =
    Parser(i => run(i) match {
      case ParseFail(_) => p run i
      case ParseValue(a) => ParseValue(a)
    })

  /*
  Exercise 15
  -----------
  Run this parser until it fails and collect all the values in a list.

  ~~~ Use map with explicit recursion.
  */
  def many: Parser[List[A]] =
    Parser(i => run(i) match {
      case ParseFail(_) => ParseValue((i, Nil))
      case ParseValue((j, a)) => many map (a :: _) run j
    })

  /*
  Exercise 16
  -----------
  Run this parser successfully once, then until it fails and collect all the values in a list.

  ~~~ Use flatMap, map and many.
  */
  def many1: Parser[List[A]] =
    for {
      h <- this
      t <- this.many
    } yield h :: t

  /*
  Exercise 17
  -----------
  Return a parser that intersperses the given parser (ignoring its result).

  ~~~ Use flatMap, map, value and many.
  */
  def separation[B](p: Parser[B]): Parser[List[A]] = {
    val s = for {
              h <- this
              t <- p.flatMap(_ => this).many
            } yield h::t
    s | Parser.value(Nil)
  }

} 

object Parser {
  type In = List[Char]

  /*
  Exercise 18
  -----------
  Return a parser that consumes no input and always produces the given value.
  */
  def value[A](a: => A): Parser[A] =
    Parser(i => ParseValue(i, a))

  /*
  Exercise 19
  -----------
  Return a parser that always fails with the given message.
  */
  def fail[A](m: => String): Parser[A] =  
    Parser(_ => ParseFail(m))

  /*
  Exercise 20
  -----------
  Return a parser that consumes a character from input and fails if the input is empty.
  */
  def character: Parser[Char] =
    Parser {
      case Nil => ParseFail("Unexpected end of stream")
      case h::t => ParseValue((t, h))
    }

  /*
  Exercise 21
  -----------
  Return a parser that consumes a character and subjects that character to the given predicate.
  If the predicate returns Some with a message, the parser fails with that message.
  If the predicate returns None, the parser succeeds with the character.

  ~~~ Use character and flatMap
  */
  def satisfy(p: Char => Option[String]): Parser[Char] = 
    character flatMap (c => 
      p(c) match {
        case None => value(c)
        case Some(m) => fail(m)
      })

  def satisfyPred(p: Char => Boolean): Parser[Char] =
    satisfy(c => if(p(c)) None else Some("Unexpected character '" + c + "'"))

  /*
  Exercise 22
  -----------
  Return a parser that consumes a character that is equal to the given character.

  ~~~ Use satisfy
  */
  def is(x: Char): Parser[Char] =
    satisfy(c => if(c == x) None else Some("Unexpected character '" + c + "'. Expecting '" + x + "'"))

  /*
  Exercise 23
  -----------
  Return a parser that consumes a whitespace character.

  ~~~ Use satisfy and Char#isWhitespace
  */
  def space: Parser[Char] =
    satisfy(c => if(c.isWhitespace) None else Some("Unexpected character '" + c + "'. Expecting whitespace"))

  def spaces: Parser[List[Char]] =
    space.many

  def lower: Parser[Char] =
    satisfy(c => if(c.isLower) None else Some("Unexpected character '" + c + "'. Expecting lowercase"))

  def upper: Parser[Char] =
    satisfy(c => if(c.isUpper) None else Some("Unexpected character '" + c + "'. Expecting uppercase"))

  def letter: Parser[Char] =
    satisfy(c => if(c.isLetter) None else Some("Unexpected character '" + c + "'. Expecting letter"))

  /*
  Exercise 24
  -----------
  Given a list of parsers, return a parser of list.
  This can be done by "sequencing"; look at the parser on the list head,
  and sequence its value to the tail.

  ~~~ Use flatMap and map with explicit recursion.
  ~~~ Alternatively, use ap and map with explicit recursion.
  */
  def sequence[A](a: List[Parser[A]]): Parser[List[A]] =
    a match {
      case Nil => value(Nil)
      case h::t => for {
                     q <- h
                     r <- sequence(t)
                   } yield q :: r
    }

  /*
  Exercise 25
  -----------
  Run the given parser the given number of times.

  ~~~ Use sequence and List#fill
  */
  def replicate[A](n: Int, p: Parser[A]): Parser[List[A]] =
    sequence(List.fill(n)(p))

  /*
  Exercise 26
  -----------
  Return a parser that consumes characters of the given list.

  ~~~ Use sequence and is
  */
  def list(s: List[Char]): Parser[List[Char]] = 
    sequence(s map is)

  def string(s: String): Parser[String] =
    list(s.toList) map (_.mkString)

}
