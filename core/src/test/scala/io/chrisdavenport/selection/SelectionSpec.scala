package io.chrisdavenport.selection

import cats.implicits._
import implicits._
import org.scalacheck.Prop._

class SelectionSuite extends munit.DisciplineSuite {

  test("return original functor after creation and forget") {
    forAll { l: List[Int] =>
      assertEquals(l.newSelection.forgetSelection, l)
    }
  }
  test("return original functor after creation and getSelected") {
    forAll { l: List[Int] =>
      assertEquals(l.newSelection.getSelected, l)
    }
  }
  test("return no values unselected of a newly created selectin") {
    forAll { l: List[Int] =>
      assertEquals(l.newSelection.getUnselected, List.empty[Int])
    }
  }
  test("return all values unselected after inversion") {
    forAll { l: List[Int] =>
      assertEquals(l.newSelection.invertSelection.getUnselected, l)
    }
  }
  test("deselectAll must exclude all values") {
    forAll { l: List[Int] =>
      assertEquals(l.newSelection.deselectAll.getSelected, List.empty[Int])
    }
  }
  test("selectAll must include all values") {
    forAll { l: List[Int] =>
      assertEquals(l.newSelection.invertSelection.selectAll.getSelected, l)
    }
  }
  test("exclude must exclude values on a predicate") {
    forAll { l: List[Int] =>
      assertEquals(
        l.newSelection.exclude(_ < 100).getUnselected.forall(_ < 100),
        true
      )
    }
  }
  test("include must include values on a predicate") {
    forAll { l: List[Int] =>
      assertEquals(
        l.newSelection.invertSelection
          .include(_ < 100)
          .getSelected
          .forall(_ < 100),
        true
      )
    }
  }
  test("return only values matching a predicate with select") {
    forAll { l: List[Int] =>
      assertEquals(
        l.newSelection.select(_ > 100).getSelected,
        l.filter(_ > 100)
      )
    }
  }
  test("selected must be empty if mapExclude is None") {
    forAll { l: List[Int] =>
      assertEquals(l.newSelection.mapExclude(_ => None).getSelected, List.empty)
    }
  }
  test("excluded must be all values if mapExclude is None") {
    forAll { l: List[Int] =>
      assertEquals(l.newSelection.mapExclude(_ => None).getUnselected, l)
    }
  }
  test("selected must be all values if mapExclude is pure") {
    forAll { l: List[Int] =>
      assertEquals(l.newSelection.mapExclude(_.pure[Option]).getSelected, l)
    }
  }
  test("collect must only collect values matching the partial") {
    forAll { l: List[Option[Int]] =>
      assertEquals(
        l.newSelection.collectExclude { case Some(i) =>
          i
        }.getSelected,
        l.flattenOption
      )
    }
  }

}
