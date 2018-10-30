package io.chrisdavenport.selection

import cats._
import cats.implicits._

/**
  * A selection wraps a Functor f and has an unselected type b and a selected type a
  */
final case class Selection[F[_], B, A](unwrap: F[Either[B, A]]) extends AnyVal
object Selection {
  
  // Typeclasses
  // Foldable => Bifoldable
  // Traversable => Bitraversable
  def functorSelection[F[_]: Functor, B]: Functor[Selection[F,B, ?]] = 
    new Functor[Selection[F, B, ?]]{
      def map[A, C](fa: Selection[F, B,A])(f: A => C): Selection[F, B, C] =
        Selection(fa.unwrap.map(_.map(f)))
    }

  def foldableSelection[F[_]: Foldable, C]: Foldable[Selection[F, C,?]] = 
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

  def traversableSelection[F[_]: Traverse, C]: Traverse[Selection[F, C, ?]] = 
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
  

  def monadSelection[F[_]: Monad, B]: Monad[Selection[F, B, ?]]  =
    new StackSafeMonad[Selection[F, B, ?]]{
      def pure[A](x: A): Selection[F,B,A] = Selection(x.pure[F].map(Either.right))
      def flatMap[A, C](fa: Selection[F,B,A])(f: A => Selection[F,B,C]):Selection[F,B,C] = 
        Selection(fa.unwrap.flatMap(_.fold(Either.left[B, C](_).pure[F], f(_).unwrap)))
    }

  def functorBifunctorSelection[F[_]: Functor]: Bifunctor[Selection[F, ?,? ]] = 
    new Bifunctor[Selection[F, ?,?]]{
      def bimap[A, B, C, D](fab: Selection[F,A,B])(f: A => C, g: B => D): Selection[F,C,D] =
        Selection(fab.unwrap.map(_.fold(f(_).asLeft, g(_).asRight)))
    }

  // Functions
  def unwrap[F[_], B, A](s: Selection[F, B, A]): F[Either[B, A]] = s.unwrap
  
  def modifySelection[F[_]: Functor, G[_], B, A, C, D](f: F[Either[B, A]] => G[Either[C, D]])(
    s: Selection[F, B, A]
  ): Selection[G, C, D] = Selection(f(s.unwrap))

  def newSelection[F[_]: Functor,B, A](f: F[A]): Selection[F, B, A] =
    Selection(f.map(Either.right))

  def forgetSelection[F[_]: Functor, A](s: Selection[F, A, A]): F[A] = 
    unify[F, A, A, A](identity)(identity)(s)

  def include[F[_]: Functor, A](s: Selection[F, A, A])(f: A => Boolean): Selection[F,A, A] =
    modifySelection[F,F, A, A, A,A](_.map(_.fold(choose(f), Either.right)))(s)

  def exclude[F[_]: Functor, A](s: Selection[F, A, A])(f: A => Boolean): Selection[F, A, A] = 
    modifySelection[F, F, A, A, A,A](_.map(_.fold(Either.left, a => switch(choose(f)(a)))))(s)

  def selectAll[F[_]: Functor, A](s: Selection[F, A, A]): Selection[F, A, A] =
    include[F,A](s)(_ => true)
  
  def deselectAll[F[_]: Functor, A](s: Selection[F, A, A]): Selection[F, A, A]=
    exclude[F, A](s)(_ => true)

  def select[F[_]: Functor, A](s: Selection[F, A, A])(f: A => Boolean): Selection[F, A, A] = 
    select(deselectAll(s))(f)

  def invertSelection[F[_]: Functor, A, B](s: Selection[F, A, B]): Selection[F, B, A] = 
    modifySelection[F, F, A, B, B, A](_.map(switch))(s)

  def mapSelected[F[_]: Functor, B, A, C](s: Selection[F, B, A])(f: A => C): Selection[F, B, C] =
    Selection(s.unwrap.map(_.map(f)))

  def mapUnselected[F[_]: Functor, B, A, C](s: Selection[F, B, A])(f: B => C): Selection[F, C, A] =
    Selection(s.unwrap.map(_.leftMap(f)))

  def getSelected[F[_]: Foldable, B, A](s: Selection[F, B, A]): List[A] =
    s.unwrap.foldMap(_.fold(_ => List.empty, List(_)))

  def getUnselected[F[_]: Foldable: Functor, B, A](s: Selection[F, B, A]): List[B] =
    s.unwrap.foldMap(_.fold(List(_), _ => List.empty))

  def unify[F[_]: Functor, A, B, C](f1: B => C)(f2: A => C)(s: Selection[F, B, A]): F[C] =
    s.unwrap.map(_.fold(f1, f2))

  def trans[F[_], G[_], B, A](f: F ~> G)(s: Selection[F, B, A]): Selection[G, B, A] =
    Selection(f(s.unwrap))

  // Helpers
  private def choose1[A, B](f: A => B)(p: A => Boolean)(a: A) : Either[B, B] = 
    if (p(a)) Either.right(f(a))
    else Either.left(f(a))

  private def choose[A](p: A => Boolean)(a: A): Either[A, A] =
    choose1[A, A](identity)(p)(a)

  private def switch[A, B](e: Either[A, B]): Either[B, A] = 
    e.fold(Either.right, Either.left)

}