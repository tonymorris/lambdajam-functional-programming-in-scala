package workshop

object Workshop {
  import Parser._

  val p =
    for {
      u <- upper
      l <- lower.many
      _ <- spaces1
    } yield u :: l

  def main(args: Array[String]) {
    val r = p parse "Abc"
    println(r)
  }
}
