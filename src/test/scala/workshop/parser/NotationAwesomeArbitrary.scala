package workshop
package parser

import org.scalacheck.{Arbitrary, Gen}, Arbitrary._, Gen._

object NotationAwesomeArbitrary {
  implicit def ArbitraryNotationAwesome: Arbitrary[NotationAwesome] = {
    val bool: Gen[NotationAwesome] = arbitrary[Boolean] map {
      case true => AwesomeTrue
      case false => AwesomeFalse
    }

    val string: Gen[NotationAwesome] = arbitrary[String] map AwesomeString

    def totally: Gen[NotationAwesome] = listOf(awesome) map TotallyAwesome

    def awesome: Gen[NotationAwesome] = oneOf(bool, string, totally)

    Arbitrary(awesome)
  }

  /*
  at workshop.parser.NotationAwesomeArbitrary$$anonfun$totally$1$1.apply(NotationAwesomeArbitrary.scala:15)
  at workshop.parser.NotationAwesomeArbitrary$$anonfun$totally$1$1.apply(NotationAwesomeArbitrary.scala:15)
  at org.scalacheck.Gen$.listOf(Gen.scala:340)
  at workshop.parser.NotationAwesomeArbitrary$.totally$1(NotationAwesomeArbitrary.scala:15)
  at workshop.parser.NotationAwesomeArbitrary$.workshop$parser$NotationAwesomeArbitrary$$awesome$1(NotationAwesomeArbitrary.scala:17)
  at workshop.parser.NotationAwesomeArbitrary$$anonfun$totally$1$1.apply(NotationAwesomeArbitrary.scala:15)
  at workshop.parser.NotationAwesomeArbitrary$$anonfun$totally$1$1.apply(NotationAwesomeArbitrary.scala:15)
  at org.scalacheck.Gen$.listOf(Gen.scala:340)
  */
}
