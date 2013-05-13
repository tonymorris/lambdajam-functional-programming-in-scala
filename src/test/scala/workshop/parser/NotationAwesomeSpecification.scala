package workshop
package parser

import org.scalacheck.Properties
import org.scalacheck.Prop._
import NotationAwesomeArbitrary._

object NotationAwesomeSpecification extends Properties("NotationAwesome") {
  property("append is associative") = forAll(
    (w: NotationAwesome) =>

      true
  )

}
