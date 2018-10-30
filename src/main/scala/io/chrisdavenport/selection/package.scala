package io.chrisdavenport

package object selection {
  type SelectionS[F[_], A] = Selection[F, A, A]
}