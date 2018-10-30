# selection [![Build Status](https://travis-ci.com/ChristopherDavenport/selection.svg?branch=master)](https://travis-ci.com/ChristopherDavenport/selection)

selection is a Scala library for transforming subsets of values within a functor.

Ever wished you could select just a few values within a functor, perform some operations on them, then flatten them back into the plain old functor again? Now you can!

Selection is a wrapper around Functors which adds several combinators and interesting instances. Wrapping a functor in Selection allows you to:

- Select specific values within your functor according to a predicate
- Expand/Contract a selection based on additional predicates using include and exclude
- TODO: Select values based on their context if your functor is also a Comonad
- Map over unselected and/or selected values using Bifunctor
- TODO: Traverse over unselected and/or selected values using Bitraversable
- TODO: Fold over unselected and/or selected values using Bifoldable
- Perform monad computations over selected values if your functor is a Monad
- Extract all unselected or selected elements to a list
- Deselect and return to your original functor using unify
