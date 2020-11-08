package io.chrisdavenport.selection

import cats._
import cats.implicits._

object CompileSpec {
  def mapSelection[F[_]: Functor, A, B](fa: Selection[F, A, A])(f: A => B): Selection[F, A, B] =
    fa.map(f)

  def flatMapSelection[F[_]: Monad, A, B](fa: Selection[F, A, A])(
      f: A => Selection[F, A, B]
  ): Selection[F, A, B] =
    fa.flatMap(f)

  def bifoldableSelection[F[_]: Foldable, A, B, C](
      fa: Selection[F, A, B]
  )(c: C, f: (C, A) => C, g: (C, B) => C): C =
    fa.bifoldLeft(c)(f, g)

  def bitraverseSelection[F[_]: Traverse, G[_]: Applicative, A, B, C, D](
      fab: Selection[F, A, B]
  )(f: A => G[C], g: B => G[D]): G[Selection[F, C, D]] =
    fab.bitraverse(f, g)

  def foldableSelection[F[_]: Foldable, A, B, C](fa: Selection[F, C, A], b: B)(f: (B, A) => B): B =
    fa.foldLeft(b)(f)

  def traverseSelection[F[_]: Traverse, G[_]: Applicative, C, A, B](fa: Selection[F, C, A])(
      f: A => G[B]
  ): G[Selection[F, C, B]] = fa.traverse(f)

  def bifunctorSelection[F[_]: Functor, A, B, C, D](
      fab: Selection[F, A, B]
  )(f: A => C, g: B => D): Selection[F, C, D] =
    fab.bimap(f, g)

  def showSelection[F[_], B, A](fa: Selection[F, B, A])(implicit
      show: Show[F[Either[B, A]]]
  ): String =
    fa.show

  def eqSelection[F[_], B, A](sa: Selection[F, B, A], sb: Selection[F, B, A])(implicit
      eq: Eq[F[Either[B, A]]]
  ): Boolean =
    sa === sb
}
