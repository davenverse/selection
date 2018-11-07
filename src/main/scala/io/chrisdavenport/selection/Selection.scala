package io.chrisdavenport.selection

import cats._
import cats.implicits._

/**
  * A selection wraps a Functor f and has an unselected type b and a selected type a
  */
final case class Selection[F[_], B, A](unwrap: F[Either[B, A]]) extends AnyVal{
  def mapK[G[_]](f: F ~> G): Selection[G, B, A] = Selection.mapK(this)(f)
}
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

  // Functions

  /**
    * Function Unwrapping, rather than method
    */
  def unwrap[F[_], B, A](s: Selection[F, B, A]): F[Either[B, A]] = s.unwrap
  
  /**
    * Modify the underlying representation of a selection
    */
  def modifySelection[F[_]: Functor, G[_], B, A, C, D](
    s: Selection[F, B, A]
  )(f:F[Either[B, A]] => G[Either[C, D]]): Selection[G, C, D] = Selection(f(s.unwrap))
  
  // Unary Functions

  /**
    * Drops selection from your functor returning all values (selected or not).
    */
  def forgetSelection[F[_]: Functor, A](s: Selection[F, A, A]): F[A] = 
    unify(s)(identity)(identity)

  /**
    * Add items which match a predicate to the current selection
    */
  def include[F[_]: Functor, A](s: Selection[F, A, A])(f: A => Boolean): Selection[F,A, A] =
    modifySelection(s)(_.map(_.fold(choose(f), Either.right)))

  /**
    *  Remove items which match a predicate to the current selection
    */
  def exclude[F[_]: Functor, A](s: Selection[F, A, A])(f: A => Boolean): Selection[F, A, A] = 
    modifySelection(s)(_.map(_.fold(Either.left, a => switch(choose(f)(a)))))

  /**
    * Select all items in the container
    */
  def selectAll[F[_]: Functor, A](s: Selection[F, A, A]): Selection[F, A, A] =
    include[F,A](s)(_ => true)
  
  /**
    * Deselect all items in the container
    */
  def deselectAll[F[_]: Functor, A](s: Selection[F, A, A]): Selection[F, A, A]=
    exclude[F, A](s)(_ => true)

  /**
    *  Clear the selection then select only items which match a predicate.
    */
  def select[F[_]: Functor, A](s: Selection[F, A, A])(f: A => Boolean): Selection[F, A, A] = 
    include(deselectAll(s))(f)

  // Binary Functions

  /**
    * Flip the selection, all selected are now unselected and vice versa
    */
  def invertSelection[F[_]: Functor, A, B](s: Selection[F, A, B]): Selection[F, B, A] = 
    modifySelection(s)(_.map(switch))

  /**
    * Map over selected values.
    */
  def mapSelected[F[_]: Functor, B, A, C](s: Selection[F, B, A])(f: A => C): Selection[F, B, C] =
    Selection(s.unwrap.map(_.map(f)))

  /**
    * Map over unselected values.
    */
  def mapUnselected[F[_]: Functor, B, A, C](s: Selection[F, B, A])(f: B => C): Selection[F, C, A] =
    Selection(s.unwrap.map(_.leftMap(f)))

  /**
    * Collect all selected values into a list. For more complex operations use
    * foldMap.
    */
  def getSelected[F[_]: Foldable, B, A](s: Selection[F, B, A]): List[A] =
    s.unwrap.foldMap(_.fold(_ => List.empty, List(_)))

  /**
    * Collect all unselected values into a list. For more complex operations use
    * foldMap.
    */
  def getUnselected[F[_]: Foldable, B, A](s: Selection[F, B, A]): List[B] =
    s.unwrap.foldMap(_.fold(List(_), _ => List.empty))

  /**
    * Unify selected and unselected and forget the selection
    */
  def unify[F[_]: Functor, A, B, C](s: Selection[F, B, A])(f1: B => C)(f2: A => C): F[C] =
    s.unwrap.map(_.fold(f1, f2))

  /**
    * Perform a natural transformation over the underlying container of a selectable
    */
  def mapK[F[_], G[_], B, A](s: Selection[F, B, A])(f: F ~> G): Selection[G, B, A] =
    Selection(f(s.unwrap))

  /**
    * Select values based on their context within a comonad.
    */
  def selectWithContext[W[_]: Comonad, A](s: Selection[W, A, A])(f: W[A] => Boolean): Selection[W, A, A] =
    modifySelection(s){w: W[Either[A, A]] => 
      val wa: W[A] = w.map(_.fold(identity, identity))
      def waB(w: W[A]): Either[A, A] = choose1[W[A], A](_.extract)(f)(w)
      wa.coflatten.map(waB)
    }

  // Helpers
  private def choose1[A, B](f: A => B)(p: A => Boolean)(a: A) : Either[B, B] = 
    if (p(a)) Either.right(f(a))
    else Either.left(f(a))

  private def choose[A](p: A => Boolean)(a: A): Either[A, A] =
    choose1[A, A](identity)(p)(a)

  private def switch[A, B](e: Either[A, B]): Either[B, A] = 
    e.fold(Either.right, Either.left)

}

// Instance Hierarchy
abstract private[selection] class SelectionInstances extends SelectionInstances1 {
  implicit def eqSelection[F[_], B, A](implicit eq: Eq[F[Either[B, A]]]): Eq[Selection[F, B, A]] =
      Eq.by(_.unwrap)

  implicit def showSelection[F[_], B,A ](implicit show: Show[F[Either[B, A]]]): Show[Selection[F, B, A]] =
    Show.show(s => 
      s"Selection(${show.show(s.unwrap)})"
    )

  implicit def functorBifunctorSelection[F[_]: Functor]: Bifunctor[Selection[F, ?,? ]] = 
    new Bifunctor[Selection[F, ?,?]]{
      def bimap[A, B, C, D](fab: Selection[F,A,B])(f: A => C, g: B => D): Selection[F,C,D] =
        Selection(fab.unwrap.map(_.fold(f(_).asLeft, g(_).asRight)))
    }

  implicit def traversableSelection[F[_]: Traverse, C]: Traverse[Selection[F, C, ?]] = 
    new Traverse[Selection[F, C, ?]]{
      def foldLeft[A, B](fa: Selection[F,C,A],b: B)(f: (B, A) => B): B = 
        fa.unwrap.foldLeft(b){
          case (b, Right(a)) => f(b, a)
          case (b, _) => b
        }
      def foldRight[A, B](fa: Selection[F,C,A],lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = 
        fa.unwrap.foldRight(lb){
          case (Right(a), eb) => f(a, eb)
          case (_ , eb) => eb
        }
      def traverse[G[_]: Applicative, A, B](fa: Selection[F,C,A])(f: A => G[B]): G[Selection[F,C,B]] = 
        fa.unwrap.traverse(_.traverse(f)).map(Selection(_))
    }

}

abstract private[selection] class SelectionInstances1 extends SelectionInstances2 {
  implicit def foldableSelection[F[_]: Foldable, C]: Foldable[Selection[F, C,?]] = 
    new Foldable[Selection[F, C, ?]]{
      def foldLeft[A, B](fa: Selection[F,C,A],b: B)(f: (B, A) => B): B = 
        fa.unwrap.foldLeft(b){
          case (b, Right(a)) => f(b, a)
          case (b, _) => b
        }
      def foldRight[A, B](fa: Selection[F,C,A],lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = 
        fa.unwrap.foldRight(lb){
          case (Right(a), eb) => f(a, eb)
          case (_ , eb) => eb
        }
    }

  implicit def traversableBiTraverseSelection[F[_]: Traverse]: Bitraverse[Selection[F, ?,? ]] =
    new Bitraverse[Selection[F, ?, ?]]{
      def bifoldLeft[A, B, C](fab: Selection[F,A,B],c: C)(f: (C, A) => C,g: (C, B) => C): C = 
        fab.unwrap.foldLeft(c){
          case (c, Left(a)) => f(c, a)
          case (c, Right(b)) => g(c, b)
        }
      def bifoldRight[A, B, C](fab: Selection[F,A,B],c: Eval[C])(f: (A, Eval[C]) => Eval[C],g: (B, Eval[C]) => Eval[C]): Eval[C] = 
        fab.unwrap.foldRight(c){
          case (Left(a), ec) => f(a, ec)
          case (Right(b), ec) => g(b, ec)
        }

      def bitraverse[G[_]: Applicative, A, B, C, D](fab: Selection[F,A,B])(f: A => G[C], g: B => G[D]): G[Selection[F,C,D]] =
        fab.unwrap.traverse{
          case Left(a) => f(a).map(Either.left[C, D])
          case Right(b) => g(b).map(Either.right[C, D])
        }.map(Selection(_))
    }

  implicit def monadSelection[F[_]: Monad, B]: Monad[Selection[F, B, ?]]  =
    new StackSafeMonad[Selection[F, B, ?]]{
      def pure[A](x: A): Selection[F,B,A] = Selection(x.pure[F].map(Either.right))
      def flatMap[A, C](fa: Selection[F,B,A])(f: A => Selection[F,B,C]):Selection[F,B,C] = 
        Selection(fa.unwrap.flatMap(_.fold(Either.left[B, C](_).pure[F], f(_).unwrap)))
    }
}

abstract private[selection] class SelectionInstances2 {
  implicit def foldableBiFoldableSelection[F[_]: Foldable]: Bifoldable[Selection[F, ?, ?]] =
    new Bifoldable[Selection[F, ?, ?]]{
      def bifoldLeft[A, B, C](fab: Selection[F,A,B],c: C)(f: (C, A) => C,g: (C, B) => C): C = 
        fab.unwrap.foldLeft(c){
          case (c, Left(a)) => f(c, a)
          case (c, Right(b)) => g(c, b)
        }
      def bifoldRight[A, B, C](fab: Selection[F,A,B],c: Eval[C])(f: (A, Eval[C]) => Eval[C],g: (B, Eval[C]) => Eval[C]): Eval[C] = 
        fab.unwrap.foldRight(c){
          case (Left(a), ec) => f(a, ec)
          case (Right(b), ec) => g(b, ec)
        }
    }

  implicit def functorSelection[F[_]: Functor, B]: Functor[Selection[F,B, ?]] = 
    new Functor[Selection[F, B, ?]]{
      def map[A, C](fa: Selection[F, B,A])(f: A => C): Selection[F, B, C] =
        Selection(fa.unwrap.map(_.map(f)))
    }

}