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

//  def replace[L <: HList, L1, L2, L0 <: HList](l:HList, l2:L2)(implicit R:Replacer.Aux[L, L1, L2, L0]):L0 =
//    R(l2)

  def main(args:Array[String]): Unit = {
    val m:State[Map[String, Int], Int] = null
    m ~> ReaderT[WriterT[State[Map[String, Int], ?], Vector[String], Int]
    //println(replace[Int :: String :: HNil, String, Int, Int :: Int :: HNil](1 :: "two" :: HNil, 2))
  }
}

