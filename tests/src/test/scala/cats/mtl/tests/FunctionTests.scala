package cats
package mtl
package tests

import cats._
import cats.instances.all._
import cats.laws.discipline.SerializableTests
import cats.laws.discipline.eq._
import cats.mtl.laws.discipline.ApplicativeLocalTests

class FunctionTests extends BaseSuite {
  checkAll("String => ?",
    ApplicativeLocalTests[FunctionC[String]#l, String](mtl.instances.local.localFunction[String])
      .applicativeLocal[String, String])
  checkAll("FunctorLocal[String => ?, String]",
    SerializableTests.serializable(mtl.instances.local.localFunction[String]))
}
