---
layout: home

---
# selection [![Build Status](https://travis-ci.com/ChristopherDavenport/selection.svg?branch=master)](https://travis-ci.com/ChristopherDavenport/selection) [![Maven Central](https://maven-badges.herokuapp.com/maven-central/io.chrisdavenport/selection_2.12/badge.svg)](https://maven-badges.herokuapp.com/maven-central/io.chrisdavenport/selection_2.12)

selection is a Scala library for transforming subsets of values within a functor. Inspired by [selections](https://github.com/ChrisPenner/selections)

Ever wished you could select just a few values within a functor, perform some operations on them, then flatten them back into the plain old functor again? Now you can!

Selection is a wrapper around Functors which adds several combinators and interesting instances. Wrapping a functor in Selection allows you to:

- Select specific values within your functor according to a predicate
- Expand/Contract a selection based on additional predicates using include and exclude
- Select values based on their context if your functor is also a Comonad
- Map over unselected and/or selected values using Bifunctor
- Traverse over unselected and/or selected values using Bitraverse
- Fold over unselected and/or selected values using Bifoldable
- Perform monad computations over selected values if your functor is a Monad
- Extract all unselected or selected elements to a list
- Deselect and return to your original functor using unify

## When Should/Shouldn't I Use Selection?

You can use selection whenever you've got a bunch of things and you want to operate over just a few of them at a time. You can do everything that selection provides by combining a bunch of predicates with map, but it gets messy really quick; selection provides a clean interface for this sort of operation.

You shouldn't use selections when you're looking for a monadic interface, selections works at the value level typically chaining commands together, while it can be used as a monad transformer if the underlying functor is also a monad, however at that point you may be better served using [EitherT](https://github.com/typelevel/cats/blob/master/core/src/main/scala/cats/data/EitherT.scala)

## Quick Start

To use selection in an existing SBT project with Scala 2.11 or a later version, add the following dependencies to your
`build.sbt` depending on your needs:

```scala
libraryDependencies ++= Seq(
  "io.chrisdavenport" %% "selection" % "<version>"
)
```

## Quick Example

First Imports.

```tut:silent
import cats.implicits._ // For Syntax Enhancements
import io.chrisdavenport.selection._ // Selection Type
import io.chrisdavenport.selection.implicits._ // Implicit Syntax On Functors
```

Here's how it looks.

```tut:book
val xs = List(1,2,3,4,5,6)

{
  xs.newSelection
    .select(_ % 2 === 0)
    .mapSelected(_ + 100)
    .bimap(odd => show"Odd: $odd", even => show"Even: $even")
    .forgetSelection
}

{
  Selection.newSelection(xs)
    .select(_ > 3)
    .mapSelected(_ + 10)
    .exclude(_ < 15)
    .mapSelected(_ + 10)
    .forgetSelection
}
```
