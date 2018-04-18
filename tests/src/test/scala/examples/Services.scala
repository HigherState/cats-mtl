package examples

import cats.Monad
import cats.mtl.MonadState

class KeyValue[T, M[_]](implicit MS:MonadState[M, Map[String, T]]) {

  import cats.Monad.ops._
  import cats.mtl.MonadState._

  implicit def M:Monad[M] = MS.monad

  def get(key:String):M[Option[T]] = {
    for {
      state <- MS.get
    } yield state.get(key)
  }

  def +(kv:(String, T)):M[Unit] = {
    for {
      _ <- modify((m:Map[String, T]) => m + kv)
    } yield ()
  }
}

class NestedKeyValue[T, M[_]](nested:KeyValue[T, M])(implicit MS:MonadState[M, Int]) {

  import cats.Monad.ops._
  import cats.mtl.MonadState._

  implicit def M:Monad[M] = MS.monad

  def get(key:String):M[Option[T]] = {
    nested.get(key)
  }

  def +(kv:(String, T)):M[Unit] = {
    for {
      value <- nested + kv
      _ <- modify((i:Int) => i + 1)
    } yield ()
  }
}
