package cats.mtl

import cats.{Functor, Monad, Monoid}
import shapeless.HList

case class ServiceMonad[R <: HList, S <: HList, W, E, T](f:(R, S) => Either[E, (S, W, T)]) {
  def run(r:R, s:S) = f(r, s)
}

//case class EffectServiceMonad[R <: HList, S <: HList, W, E, T](f:(R,S) => Future[Either[E, (S, W, T)]])

object ServiceMonads {

  implicit def serviceMonad[R <: HList, S <: HList, W, E](implicit M:Monoid[W]):Monad[ServiceMonad[R, S, W, E, ?]] =
    new Monad[ServiceMonad[R, S, W, E, ?]] {

      def tailRecM[A, B](a: A)(f: A => ServiceMonad[R, S, W, E, Either[A, B]]): ServiceMonad[R, S, W, E, B] =
        ServiceMonad{(r:R, s:S) =>
          f(a).run(r, s) match {
            case Left(p) =>
              Left(p)
            case Right((_s, _w, Left(a))) =>
              tailRecM(a)(f).run(r, _s) match {
                case Left(_e) => Left(_e)
                case Right((__s, __w, _t)) => Right((s, M.combine(_w, __w), _t))
              }
            case Right((_s, _w, Right(b))) =>
              Right((_s, _w, b))
          }
        }

      def pure[A](x: A): ServiceMonad[R, S, W, E, A] =
        ServiceMonad((r, s) => Right((s, M.empty, x)))

      def flatMap[A, B](fa: ServiceMonad[R, S, W, E, A])(f: (A) => ServiceMonad[R, S, W, E, B]): ServiceMonad[R, S, W, E, B] =
        ServiceMonad{(r:R, s:S) =>
          fa.f(r, s) match {
            case Left(p) =>
              Left(p)
            case Right((_s, _w, t)) =>
              f(t).f(r, _s) match {
                case Left(_e) => Left(_e)
                case Right((__s, __w, _t)) => Right((s, M.combine(_w, __w), _t))
              }
          }
        }
    }

  implicit def stateMonad[R <: HList, S <: HList, W, E, S2]
  (implicit
   S: shapeless.ops.hlist.Selector[S, S2],
   R: shapeless.ops.hlist.Replacer.Aux[S, S2, S2, S],
   M:Monoid[W]):MonadState[ServiceMonad[R, S, W, E, ?], S2] =
    new MonadState[ServiceMonad[R, S, W, E, ?], S2] {

      val monad: Monad[ServiceMonad[R, S, W, E, ?]] = serviceMonad[R, S, W, E](M)

      def inspect[A](f: S2 => A): ServiceMonad[R, S, W, E, A] = ???

      def modify(f: S2 => S2): ServiceMonad[R, S, W, E, Unit] = ???

      def get: ServiceMonad[R, S, W, E, S2] =
        ServiceMonad((r, s) => Right((s, M.empty, S(s))))

      def set(s2: S2): ServiceMonad[R, S, W, E, Unit] =
        ServiceMonad((r, s) => Right((R(s, s2), M.empty, ())))

    }

  implicit def applicativeAsk[R <: HList, S <: HList, W, E, R2]
  (implicit
   S: shapeless.ops.hlist.Selector[R, R2],
   R: shapeless.ops.hlist.Replacer.Aux[R, R2, R2, R],
   M:Monoid[W]):ApplicativeAsk[ServiceMonad[R, S, W, E, ?], R2] =
    new ApplicativeAsk[ServiceMonad[R, S, W, E, ?], R2]{

      val applicative = serviceMonad[R, S, W, E](M)

      def reader[A](f: R2 => A) = ???

      def monoid = M

      def ask: ServiceMonad[R, S, W, E, R2] =
        ServiceMonad((r, s) => Right((s, M.empty, S(r))))

      def local[A](f:(R2) => R2)(fa: ServiceMonad[R, S, W, E, A]): ServiceMonad[R, S, W, E, A] =
        ServiceMonad((r, s) => fa.f(R(r, f(S(r))), s))
    }


  implicit def functorTell[R <: HList, S <: HList, W, E](implicit M:Monoid[W]):FunctorTell[ServiceMonad[R, S, W, E, ?], W] =
    new FunctorTell[ServiceMonad[R, S, W, E, ?], W] {

      val functor: Functor[ServiceMonad[R, S, W, E, ?]] =
        serviceMonad[R, S, W, E](M)

      def tell(l: W): ServiceMonad[R, S, W, E, Unit] =
        writer((), l)

      def writer[A](a: A, l: W): ServiceMonad[R, S, W, E, A] =
        ServiceMonad((r, s) => Right((s, l, a)))

      def tuple[A](ta: (W, A)): ServiceMonad[R, S, W, E, A] =
        writer(ta._2, ta._1)
    }

  implicit def functorRaise[R <: HList, S <: HList, W, E](implicit M:Monoid[W]):FunctorRaise[ServiceMonad[R, S, W, E, ?], E] =
    new FunctorRaise[ServiceMonad[R, S, W, E, ?], E] {

      val functor: Functor[ServiceMonad[R, S, W, E, ?]] =
        serviceMonad[R, S, W, E](M)

      def raise[A](e: E): ServiceMonad[R, S, W, E, A] =
        ServiceMonad((r, s) => Left(e))

      def raiseError[A](e: E): ServiceMonad[R, S, W, E, A] =
        ServiceMonad((r,s) => Left(e))

    }
}