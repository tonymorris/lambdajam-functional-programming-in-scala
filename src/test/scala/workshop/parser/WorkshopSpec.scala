package workshop
package parser

object WorkshopSpec extends Spec {
  import NotationAwesome._

  "sole true" should {
    awesomeP parse "true" must_== ParseValue(AwesomeTrue)
  }

  "list with single value" should {
    awesomeP parse "[false]" must_== ParseValue(TotallyAwesome(List(AwesomeFalse)))
  }

  "empty list" should {
    awesomeP parse "[]" must_== ParseValue(TotallyAwesome(List()))
  }

  "sole string" should {
    awesomeP parse "\"awesome\"" must_== ParseValue(AwesomeString("awesome"))
  }

  "sole string containing brackets" should {
    awesomeP parse "\"awe[some]\"" must_== ParseValue(AwesomeString("awe[some]"))
  }

  "list with several top-level values" should {
    awesomeP parse "[\"awesome\", false, true]" must_== ParseValue(TotallyAwesome(List(AwesomeString("awesome"), AwesomeFalse, AwesomeTrue)))
  }

  "list with embedded values" should {
    awesomeP parse "[\"awesome\", false, [ true, false, \"more awesome\"]]" must_== ParseValue(TotallyAwesome(List(AwesomeString("awesome"), AwesomeFalse, TotallyAwesome(List(AwesomeTrue, AwesomeFalse, AwesomeString("more awesome"))))))
  }

  "list with values interspersed with whitespace" should {
    awesomeP parse "  [  \"awesome\"  , false, [ true   , false, \"more awesome\"]  ]  " must_== ParseValue(TotallyAwesome(List(AwesomeString("awesome"), AwesomeFalse, TotallyAwesome(List(AwesomeTrue, AwesomeFalse, AwesomeString("more awesome"))))))
  }

  "arbitrary characters fail" should {
    awesomeP.parse("awesome").isFail
  }

  "unbalanced quotes fail" should {
    awesomeP.parse("\"awesome").isFail
  }

  "unbalanced left bracket fail" should {
    awesomeP.parse("[ true").isFail
  }

  "unbalanced right bracket fail" should {
    awesomeP.parse("],  ").isFail
  }

  "misplaced comma fail" should {
    awesomeP.parse(",  ").isFail
  }
}
