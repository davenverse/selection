package io.chrisdavenport.selection

import org.specs2._
import cats.implicits._
import implicits._

object SelectionSpec extends mutable.Specification with ScalaCheck {

  "Selection" should {
    "return original functor after creation and forget" >> prop { l: List[Int] => 
      l.newSelection.forgetSelection must_=== l
    }
    "return original functor after creation and getSelected" >> prop { l: List[Int] => 
      l.newSelection.getSelected must_=== l
    }

    "return no values unselected of a newly created selectin" >> prop { l: List[Int] => 
      l.newSelection.getUnselected must_=== List.empty[Int]
    }
    "return all values unselected after inversion" >> prop { l: List[Int] => 
      l.newSelection.invertSelection.getUnselected must_=== l
    }
    "deselectAll must exclude all values" >> prop { l: List[Int] => 
      l.newSelection.deselectAll.getSelected must_=== List.empty[Int]
    }

    "selectAll must include all values" >> prop {l : List[Int] => 
      l.newSelection.invertSelection.selectAll.getSelected must_=== l
    }

    "exclude must exclude values on a predicate" >> prop {l : List[Int] => 
      l.newSelection.exclude(_ < 100).getUnselected.forall(_ < 100) must_=== true
    }
    "include must include values on a predicate" >> prop {l: List[Int] => 
      l.newSelection.invertSelection.include(_ < 100).getSelected.forall(_ < 100) must_=== true
    }

    "return only values matching a predicate with select" >> prop { l: List[Int] => 
      l.newSelection.select(_ > 100).getSelected must_=== l.filter(_ > 100)
    }
    "selected must be empty if mapExclude is None" >> prop { l: List[Int] => 
      l.newSelection.mapExclude(_ => None).getSelected must_=== List.empty
    }
    "excluded must be all values if mapExclude is None" >> prop { l: List[Int] => 
      l.newSelection.mapExclude(_ => None).getUnselected must_=== l
    }
    "selected must be all values if mapExclude is pure" >> prop {l: List[Int] => 
      l.newSelection.mapExclude(_.pure[Option]).getSelected must_=== l
    }
    "collect must only collect values matching the partial" >> prop {l: List[Option[Int]] => 
      l.newSelection.collectExclude{ case Some(i) => i}.getSelected must_=== l.flattenOption
    }
  }

}