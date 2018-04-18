package examples

import cats.Monoid
import cats.data.{ReaderT, State, WriterT}
import cats.mtl.ServiceMonad
import cats.mtl.instances.all._
import shapeless._

object Instantiate {
  import cats.mtl.ServiceMonads._

  implicit val M = new Monoid[Vector[String]] {
    def empty = Vector.empty

    def combine(x: Vector[String], y: Vector[String]) = x ++ y
  }

  val stateKeyValue = new KeyValue[Int, State[Map[String, Int], ?]]()

  val readerStateKeyValue = new KeyValue[Int, ReaderT[State[Map[String, Int], ?], String, ?]]()

  val readerWriteStateKeyValue = new KeyValue[Int, ReaderT[WriterT[State[Map[String, Int], ?], Vector[String], ?], String, ?]]()

  val serviceKeyValue = new KeyValue[Int, ServiceMonad[HNil, Int :: Map[String, Int] :: HNil, Vector[String], String, ?]]()(
    stateMonad[HNil, Int :: Map[String, Int] :: HNil, Vector[String], String, Map[String, Int]])

}
