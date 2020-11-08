package io.chrisdavenport.selection
package syntax

import cats._

trait selection {
  implicit class selectionCreationFunctorOps[F[_]: Functor, A](private val fa: F[A]) {
    def newSelection: Selection[F, A, A] = Selection.newSelection(fa)
    def newSelectionB[B]: Selection[F, B, A] = Selection.newSelectionB(fa)
  }
}

object selection extends selection
