package io.chrisdavenport.selection

import cats._
import cats.implicits._

/**
 * A selection wraps a Functor f and has an unselected type b and a selected type a
 */
final case class Selection[F[_], B, A](unwrap: F[Either[B, A]]) extends AnyVal {

  /**
   * Modify the underlying representation of a selection
   */
  def modifySelection[G[_], C, D](f: F[Either[B, A]] => G[Either[C, D]]): Selection[G, C, D] =
    Selection(f(unwrap))

  /**
   * Flip the selection, all selected are now unselected and vice versa
   */
  def invertSelection(implicit F: Functor[F]): Selection[F, A, B] =
    modifySelection(_.map(switch))

  /**
   * Map over selected values.
   */
  def mapSelected[C](f: A => C)(implicit F: Functor[F]): Selection[F, B, C] =
    Selection(unwrap.map(_.map(f)))

  /**
   * Map over unselected values.
   */
  def mapUnselected[C](f: B => C)(implicit F: Functor[F]): Selection[F, C, A] =
    Selection(unwrap.map(_.leftMap(f)))

  /**
   * Collect all selected values into a list. For more complex operations use
   * foldMap.
   */
  def getSelected(implicit F: Foldable[F]): List[A] =
    unwrap.foldMap(_.fold(_ => List.empty, List(_)))

  /**
   * Collect all unselected values into a list. For more complex operations use
   * foldMap.
   */
  def getUnselected(implicit F: Foldable[F]): List[B] =
    unwrap.foldMap(_.fold(List(_), _ => List.empty))

  /**
   * Unify selected and unselected and forget the selection
   */
  def unify[C](f1: B => C)(f2: A => C)(implicit F: Functor[F]): F[C] =
    unwrap.map(_.fold(f1, f2))

  /**
   * Perform a natural transformation over the underlying container of a selectable
   */
  def mapK[G[_]](f: F ~> G): Selection[G, B, A] =
    Selection(f(unwrap))

  /**
   * Exclude Values Not Present in the codomain
   */
  def mapExclude[C](f: A => Option[C])(implicit F: Functor[F], ev: A =:= B): Selection[F, B, C] =
    modifySelection(_.map(_.flatMap(a => f(a).fold(Either.left[B, C](ev(a)))(Either.right))))

  /**
   * Similar to mapExclude but a partial Function
   */
  def collectExclude[C](
      f: PartialFunction[A, C]
  )(implicit F: Functor[F], ev: A =:= B): Selection[F, B, C] =
    mapExclude(f.lift)

  /**
   * Drops selection from your functor returning all values (selected or not).
   */
  def forgetSelection(implicit F: Functor[F], ev: B =:= A): F[A] =
    unify(ev)(identity)

  /**
   * Add items which match a predicate to the current selection
   */
  def include(f: A => Boolean)(implicit F: Functor[F], ev: B =:= A): Selection[F, A, A] =
    modifySelection(_.map(_.fold[Either[A, A]](b => choose(f)(ev(b)), Either.right)))

  /**
   *  Remove items which match a predicate to the current selection
   */
  def exclude(f: A => Boolean)(implicit F: Functor[F], ev: B =:= A): Selection[F, A, A] =
    modifySelection(_.map(_.fold(b => Either.left(ev(b)), a => switch(choose(f)(a)))))

  /**
   * Select all items in the container
   */
  def selectAll(implicit F: Functor[F], ev: B =:= A): Selection[F, A, A] =
    include(_ => true)

  /**
   * Deselect all items in the container
   */
  def deselectAll(implicit F: Functor[F], ev: B =:= A): Selection[F, A, A] =
    exclude(_ => true)

  /**
   *  Clear the selection then select only items which match a predicate.
   */
  def select(f: A => Boolean)(implicit F: Functor[F], ev: B =:= A): Selection[F, A, A] =
    deselectAll.include(f)

  /**
   * Select values based on their context within a comonad.
   */
  def selectWithContext(
      f: F[A] => Boolean
  )(implicit F: Comonad[F], ev: B =:= A): Selection[F, A, A] =
    modifySelection { w: F[Either[B, A]] =>
      val wa: F[A] = w.map(_.fold(ev, identity))
      def waB(w: F[A]): Either[A, A] = choose1[F[A], A](_.extract)(f)(w)
      wa.coflatten.map(waB)
    }

  // Helpers
  private def choose1[C, D](f: C => D)(p: C => Boolean)(a: C): Either[D, D] =
    if (p(a)) Either.right(f(a))
    else Either.left(f(a))

  private def choose[C](p: C => Boolean)(a: C): Either[C, C] =
    choose1[C, C](identity)(p)(a)

  private def switch[C, D](e: Either[C, D]): Either[D, C] =
    e.fold(Either.right, Either.left)

}

/**
 * Selection Companion Object Holds the constructor methods
 * and typeclass instances.
 */
object Selection extends SelectionInstances {

  // Constructor
  /**
   * Create a selection from a functor by selecting all values
   */
  def newSelection[F[_]: Functor, A](f: F[A]): Selection[F, A, A] =
    newSelectionB[F, A, A](f)

  /**
   * Create a selection from a functor by selecting all values,
   * demands specification of the unselected type.
   */
  def newSelectionB[F[_]: Functor, B, A](f: F[A]): Selection[F, B, A] =
    Selection(f.map(Either.right))

}

// Instance Hierarchy
private[selection] trait SelectionInstances extends SelectionInstances1 {
  implicit def eqSelection[F[_], B, A](implicit eq: Eq[F[Either[B, A]]]): Eq[Selection[F, B, A]] =
    Eq.by(_.unwrap)

  implicit def showSelection[F[_], B, A](implicit
      show: Show[F[Either[B, A]]]
  ): Show[Selection[F, B, A]] =
    Show.show(s => s"Selection(${show.show(s.unwrap)})")

  implicit def functorBifunctorSelection[F[_]: Functor]: Bifunctor[Selection[F, *, *]] =
    new Bifunctor[Selection[F, *, *]] {
      def bimap[A, B, C, D](fab: Selection[F, A, B])(f: A => C, g: B => D): Selection[F, C, D] =
        Selection(fab.unwrap.map(_.fold(f(_).asLeft, g(_).asRight)))
    }

  implicit def traversableSelection[F[_]: Traverse, C]: Traverse[Selection[F, C, *]] =
    new Traverse[Selection[F, C, *]] {
      def foldLeft[A, B](fa: Selection[F, C, A], b: B)(f: (B, A) => B): B =
        fa.unwrap.foldLeft(b) {
          case (b, Right(a)) => f(b, a)
          case (b, _) => b
        }
      def foldRight[A, B](fa: Selection[F, C, A], lb: Eval[B])(
          f: (A, Eval[B]) => Eval[B]
      ): Eval[B] =
        fa.unwrap.foldRight(lb) {
          case (Right(a), eb) => f(a, eb)
          case (_, eb) => eb
        }
      def traverse[G[_]: Applicative, A, B](fa: Selection[F, C, A])(
          f: A => G[B]
      ): G[Selection[F, C, B]] =
        fa.unwrap.traverse(_.traverse(f)).map(Selection(_))
    }

}

private[selection] trait SelectionInstances1 extends SelectionInstances2 {
  implicit def foldableSelection[F[_]: Foldable, C]: Foldable[Selection[F, C, *]] =
    new Foldable[Selection[F, C, *]] {
      def foldLeft[A, B](fa: Selection[F, C, A], b: B)(f: (B, A) => B): B =
        fa.unwrap.foldLeft(b) {
          case (b, Right(a)) => f(b, a)
          case (b, _) => b
        }
      def foldRight[A, B](fa: Selection[F, C, A], lb: Eval[B])(
          f: (A, Eval[B]) => Eval[B]
      ): Eval[B] =
        fa.unwrap.foldRight(lb) {
          case (Right(a), eb) => f(a, eb)
          case (_, eb) => eb
        }
    }

  implicit def traversableBiTraverseSelection[F[_]: Traverse]: Bitraverse[Selection[F, *, *]] =
    new Bitraverse[Selection[F, *, *]] {
      def bifoldLeft[A, B, C](fab: Selection[F, A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C =
        fab.unwrap.foldLeft(c) {
          case (c, Left(a)) => f(c, a)
          case (c, Right(b)) => g(c, b)
        }
      def bifoldRight[A, B, C](
          fab: Selection[F, A, B],
          c: Eval[C]
      )(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C] =
        fab.unwrap.foldRight(c) {
          case (Left(a), ec) => f(a, ec)
          case (Right(b), ec) => g(b, ec)
        }

      def bitraverse[G[_]: Applicative, A, B, C, D](
          fab: Selection[F, A, B]
      )(f: A => G[C], g: B => G[D]): G[Selection[F, C, D]] =
        fab.unwrap
          .traverse {
            case Left(a) => f(a).map(Either.left[C, D])
            case Right(b) => g(b).map(Either.right[C, D])
          }
          .map(Selection(_))
    }

  implicit def monadSelection[F[_]: Monad, B]: Monad[Selection[F, B, *]] =
    new Monad[Selection[F, B, *]] {
      def tailRecM[A, C](a: A)(f: A => Selection[F, B, Either[A, C]]): Selection[F, B, C] =
        Selection(
          Monad[F].tailRecM(a)(f(_).unwrap.map {
            case Left(l) => Right(Left(l))
            case Right(Left(a1)) => Left(a1)
            case Right(Right(b)) => Right(Right(b))
          })
        )
      def pure[A](x: A): Selection[F, B, A] = Selection(x.pure[F].map(Either.right))
      def flatMap[A, C](fa: Selection[F, B, A])(f: A => Selection[F, B, C]): Selection[F, B, C] =
        Selection(fa.unwrap.flatMap(_.fold(Either.left[B, C](_).pure[F], f(_).unwrap)))
    }
}

private[selection] trait SelectionInstances2 {
  implicit def foldableBiFoldableSelection[F[_]: Foldable]: Bifoldable[Selection[F, *, *]] =
    new Bifoldable[Selection[F, *, *]] {
      def bifoldLeft[A, B, C](fab: Selection[F, A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C =
        fab.unwrap.foldLeft(c) {
          case (c, Left(a)) => f(c, a)
          case (c, Right(b)) => g(c, b)
        }
      def bifoldRight[A, B, C](
          fab: Selection[F, A, B],
          c: Eval[C]
      )(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C] =
        fab.unwrap.foldRight(c) {
          case (Left(a), ec) => f(a, ec)
          case (Right(b), ec) => g(b, ec)
        }
    }

  implicit def functorSelection[F[_]: Functor, B]: Functor[Selection[F, B, *]] =
    new Functor[Selection[F, B, *]] {
      def map[A, C](fa: Selection[F, B, A])(f: A => C): Selection[F, B, C] =
        Selection(fa.unwrap.map(_.map(f)))
    }

}
