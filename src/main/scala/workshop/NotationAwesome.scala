package workshop

sealed trait NotationAwesome
case object AwesomeTrue extends NotationAwesome
case object AwesomeFalse extends NotationAwesome
case class AwesomeString(s: String) extends NotationAwesome
case class TotallyAwesome(a: List[NotationAwesome]) extends NotationAwesome

object NotationAwesome {
  import Parser._

  def trueP: Parser[NotationAwesome] =
    string("true") map (_ => AwesomeTrue)

  def falseP: Parser[NotationAwesome] =
    string("false") map (_ => AwesomeFalse)

  def stringP: Parser[NotationAwesome] =
    for {
      _ <- is('"')
      c <- satisfyPred(_ != '"').many
      _ <- is('"')
    } yield AwesomeString(c.mkString)

  def totallyP: Parser[NotationAwesome] =
    for {
      _ <- is('[')
      _ <- spaces
      a <- awesomeP separation is(',')
      _ <- spaces
      _ <- is(']')
    } yield TotallyAwesome(a)

  def awesomeP: Parser[NotationAwesome] =
    for {
      _ <- spaces
      a <- totallyP | stringP | falseP | trueP
      _ <- spaces
    } yield a
}