---
layout: page
title:  "tutorial"
section: "tutorial"
position: 1
---

To start the process lets get our imports out of the way.

```scala mdoc:silent
import cats._
import cats.syntax.all._
import cats.derived._ // For Kittens Derivation
import io.chrisdavenport.selection._
import io.chrisdavenport.selection.implicits._
import cats.effect._ // For Effect Display
```

Now let's build some data

```scala mdoc
// Weirdness in this block is for companion object behavior in tut

sealed trait Country; case object USA extends Country; case object Canada extends Country; object Country {
  implicit val showCountry: Show[Country] = semiauto.show
  implicit val eqCountry: Eq[Country] = semiauto.eq
}

final case class Account(name: String, country: Country, balance: Double); object Account {
  implicit val showCountry: Show[Account] = semiauto.show
  implicit val eqCountry: Eq[Account] = semiauto.eq
}
```

So the accounts we are modeling are fairly simple. They have a name, country of origin and account balance. Let's make a 'database' of accounts as a simple list:

```scala mdoc
val accounts: List[Account] = List(
  Account("Steve" , Canada  , 34D),
  Account("Cindy" , USA     , 10D),
  Account("Blake" , USA     , -6D),
  Account("Carl"  , Canada  , -16D)
)
```

Great! So far so good. Now we see where selections come in handy, let's say we want to accumulate interest for all accounts with a POSITIVE account balance. Normally we'd need to map over every user, check their account balance, then perform the interest calculation. This code is pretty straightforward to write, but it gets a bit clunky in more complex situations. With selections we can select the accounts we want to work with, then map over them specifically!

One additional complication! USA and Canadian accounts get different interest rates! No problem though, let's see what we can do!

```scala mdoc
type Rate = Double

def addInterest(rate: Rate)(user: Account): Account = user.copy(balance = user.balance * rate)

val usaRate = 1.25D
val canRate = 1.10D

val adjusted : List[Account] = {
  accounts
    .newSelection
    .select(_.country === USA)
    .exclude(_.balance < 0)
    .mapSelected(addInterest(usaRate)(_))
    .select(_.country === Canada)
    .exclude(_.balance < 0)
    .mapSelected(addInterest(canRate)(_))
    .forgetSelection
}
```

You can see it made the proper adjustments without touching the negative accounts! Since, quite a bit just happened, lets break it down a bit.

```scala mdoc
val americans : SelectionA[List, Account] = {
  accounts.newSelection.select(_.country === USA)
}
```

So our first step is to create a selection around the list of users. `newSelection` wraps any `Functor` in a `Selection` type, either through the implicits as shown, or `Selection.newSelection`. The Selection type is `Selection[F[_], B, A]` where `F` is the functor, `B` represents the type of unselected data, `A` represents the type of selected data. These types can diverge as you like, but in this case they are both `Account` so we use the `SelectionA` type alias, which is simply a Selection where both unselected and selected data types are the same.

Now that we have a new selection we have to tell it what we would like to select! `newSelection` selects all elements by default, which isn't entirely useful, so we use `select` with a predicate to determine which elements we want to be in focus. In this case `.select(_.country === USA)` clears the previous selection, then selects only the american accounts

As we can see above, we can see that the accounts in the USA are all wrapped in `Right` whereas the others are wrapped in `Left`. As users of the library, you don't need to worry about that, the interface manages those details for you, but seeing it working is cool!

If we wanted to select the Canadians we could write a predicate for that, or since we know that we are tracking only 2 countries right now, we could use `invertSelection` to flip the selection so that Canadians are focused and Americans are unselected.

We can now use `getSelected` and `getUnselected` to get a list of USA or Canadian accounts respectively, not how the `Right`'s and `Left`'s disappear whenever we stop working within a Selection:

```scala mdoc
americans.getSelected

americans.getUnselected
```

We've got our americans selected, but Blake has a negative account balance! Let's `exclude` any accounts with a negative balance. `exclude` keeps the current selection, but removes any elements that fail the predicate. There's also an `include` combinator which will add any unselected elements which do pass the predicate(assuming you want that).

```scala mdoc
val americansPositive = americans.exclude(_.balance < 0)
```

Now we can finally make our transformation, `mapSelected` is provided if you want a nicely named combinator, however its just a synonym for `map`.

```scala mdoc
val americansAdjusted = americansPositive.mapSelected(addInterest(usaRate)(_))
```

All selections are `Bifunctors`, so you can `bimap` over the unselected and selected values respectively if you like. Let's say there was a banking error in our user's favour (it happens all the time I swear). All Americans get a $10 credit, all Canadians get a $7 credit!

```scala mdoc
def adjustBalance(adjustment: Double => Double)(account: Account): Account = {
  account.copy(balance = adjustment(account.balance))
}

val withCredit : List[Account] = {
  accounts
    .newSelection
    .select(_.country === USA)
    .bimap(adjustBalance(_ + 7D)(_), adjustBalance(_ + 10D)(_))
    .forgetSelection
}
```

They're also Bitraversable and Bifoldable, so we can perform operations with effects over each segement independently or
perform different effectful operations over each type. Let's print out a warning to all users with a negative balance!

```scala mdoc

val warnDelinquents = {
  def warn(user: Account): IO[Unit] = IO(println(user.name ++ ": get your act together!"))
  def congrats(user: Account): IO[Unit] = IO(println(user.name ++ " you're doing great!"))
  
  accounts
    .newSelection
    .select(_.balance < 0)
    .bitraverse(congrats, warn)
    .void
}


import cats.effect.unsafe.implicits.global
warnDelinquents.unsafeRunSync()
```

You can use the Bifoldable instance to do similarly interesting things, getSelected and getUnselected are provided as helpers which return lists of the selected and unselected items.

That's it for this tutorial!
