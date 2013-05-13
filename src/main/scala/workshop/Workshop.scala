package workshop

object Workshop {
  import Parser._

  val p =
    for {
      _ <- spaces
      a <- string("true")
    } yield a

  val s = "true"
  val t = p parse s

  def main(args: Array[String]) {
    println(t)
  }
}

sealed trait NotationAwesome
case object AwesomeTrue extends NotationAwesome
case object AwesomeFalse extends NotationAwesome
case class AwesomeString(s: String) extends NotationAwesome
case class TotallyAwesome(a: List[NotationAwesome]) extends NotationAwesome

object NotationAwesomeParser {
  import Parser._

  def trueP: Parser[NotationAwesome] =
    string("true") map (_ => AwesomeTrue)

  def falseP: Parser[NotationAwesome] =
    string("false") map (_ => AwesomeFalse)

  def stringP: Parser[NotationAwesome] =
    for {
      _ <- is('"')
      c <- characters
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
      a <- trueP //  totallyP | stringP | falseP | trueP
      _ <- spaces
    } yield a
}

object Data {

}
