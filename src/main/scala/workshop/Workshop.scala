package workshop

object Tests {
  val tests: List[(String, Option[NotationAwesome], String)] =
    List(
      (
        "true"
      , Some(AwesomeTrue)
      , "true"
      )
    )
}

object Workshop {
  import NotationAwesomeParser.awesomeP
  import Tests.tests

  val q = tests map {
    case (name, result, in) => {
      val r = awesomeP parse in
      result match {
        case None => r match {
          case ParseFail(_) => "✔ " + name
          case ParseValue(v) => "✖ " + name + " > Expected parser failure, but succeeded with " + v
        }
        case Some(w) => r match {
          case ParseFail(_) => "✖ " + name + " > Expected parser to succeed with " + w + ", but failed"
          case ParseValue(v) => if(w == v) "✔ " + name else "✖ " + name + " Expected parser to succeed with " + w + ", but succeeded with " + v
        }
      }
    }
  }

  def main(args: Array[String]) {
    q foreach println
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
