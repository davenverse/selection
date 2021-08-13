package io.chrisdavenport.selection

import cats._
// import cats.data._
// import cats.implicits._
// import org.scalacheck.cats._
// import cats.tests.CatsSuite
import cats.kernel.laws.discipline._
import cats.laws.discipline._
import org.scalacheck._
import org.typelevel.discipline.specs2.Discipline


class SelectionTests extends org.specs2.Specification with Discipline {
  implicit def arbSelection[F[_]: Functor, E, A](implicit A: Arbitrary[F[A]]): Arbitrary[Selection[F, E, A]] = 
    Arbitrary{A.arbitrary.map(Selection.newSelectionB[F, E, A](_))}
  implicit def arbFunction[B, A: Arbitrary]: Arbitrary[B => A] = Arbitrary{
    for {
      a <- Arbitrary.arbitrary[A]
    } yield {_: B => a}
  }
  
  // *
  checkAll("Selection", EqTests[Selection[List, Int, Int]].eqv)

  // * -> *
  checkAll("Selection", FoldableTests[Selection[List, Int, *]].foldable[Int, Int])
  checkAll("Selection", FunctorTests[Selection[List, Int, *]].functor[Int, Int, Int])
  checkAll("Selection", TraverseTests[Selection[List, Int, *]].traverse[Int, Int, Int, Int, Id, Id])
  checkAll("Selection", MonadTests[Selection[List, Int, *]].monad[Int, Int, Int])

 // * -> * -> *
  checkAll("Selection", BifoldableTests[Selection[List, *, *]].bifoldable[Int, Int, Int])
  checkAll("Selection", BifunctorTests[Selection[List, *, *]].bifunctor[Int, Int, Int, Int, Int, Int])
  checkAll("Selection", BitraverseTests[Selection[List, *, *]].bitraverse[Id, Int, Int, Int, Int, Int, Int])
  
}
