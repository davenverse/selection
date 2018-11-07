package io.chrisdavenport.selection
package syntax

import cats._

trait  selection {
  implicit class selectionCreationFunctorOps[F[_]: Functor, A](private val fa: F[A]){
    def newSelection: Selection[F, A, A] = Selection.newSelection(fa)
    def newSelectionB[B]: Selection[F, B, A] = Selection.newSelectionB(fa)
  }

  implicit class selectionNoConstraintOps[F[_], B, A](private val s: Selection[F, B, A]){
    def mapK[G[_]](f: F ~> G): Selection[G, B, A] = Selection.mapK(s)(f)
  }

  implicit class binaryFunctorOps[F[_]: Functor, B, A](private val s: Selection[F, B, A]){
    def modifySelection[G[_], C ,D](f: F[Either[B, A]] => G[Either[C, D]]): Selection[G, C, D] =
      Selection.modifySelection(s)(f)
    def invertSelection: Selection[F, A, B] = Selection.invertSelection(s)
    def mapSelected[C](f: A => C): Selection[F, B, C] = Selection.mapSelected(s)(f)
    def mapUnselected[C](f: B => C): Selection[F, C, A] = Selection.mapUnselected(s)(f)
    def unify[C](fb: B => C)(fa: A => C): F[C] = Selection.unify(s)(fb)(fa)
  }

  implicit class binaryFoldableOps[F[_]: Foldable, B, A](private val s: Selection[F, B, A]){
    def getSelected: List[A] = Selection.getSelected(s)
    def getUnselected: List[B] = Selection.getUnselected(s)
  }
  
  implicit class unaryFunctorOps[F[_]: Functor, A](private val s: Selection[F, A, A]){
    def forgetSelection: F[A] = Selection.forgetSelection(s)
    def include(f: A => Boolean): Selection[F, A, A] = Selection.include(s)(f)
    def exclude(f: A => Boolean): Selection[F, A, A] = Selection.exclude(s)(f)
    def selectAll: Selection[F, A, A] = Selection.selectAll(s)
    def deselectAll: Selection[F, A, A] = Selection.deselectAll(s)
    def select(f: A => Boolean): Selection[F, A, A] = Selection.select(s)(f)
  }

  implicit class comonadOps[F[_]: Comonad, A](private val s: Selection[F, A, A]){
    def selectWithContext(f: F[A] => Boolean): Selection[F, A, A] = Selection.selectWithContext(s)(f)
  }
}

object selection extends selection