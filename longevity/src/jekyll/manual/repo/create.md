---
title: repo.create
layout: page
---

`Repo.create` takes an unpersisted object as argument, persists it, and returns the persistent
state. Here's a simplified version of its signature:

```scala
def create[P](unpersisted: P): F[PState[P]]
```

We left out one thing: the implicit `longevity.model.PEv[M, P]`. The "ev" in `PEv` stands for
"evidence". This implicit is evidence that the type `P` is actually a type of something that you
have declared as persistent in model `M`. This evidence will be found by your compiler in the
companion object for class `P`, assuming it is actually a `longevity.model.PType[M, P]`. It will be
if you annotated you persistent class with `@longevity.model.annotations.persistent[M]`, as
recommended in the section on [persistent objects](../model/persistents.html). Requiring this
evidence allows us to make repository methods typesafe without requiring you to provide a marker
trait on your persistent classes.

Here's how we would persist a user:

```scala
val user = User("smithy", "John Smith", "smithy@john-smith.ninja")
val futureUserState: Future[PState[User]] = userRepo.create(user)
```

`Repo.create` gives back a `PState`, which you can in turn manipulate
and pass to [`Repo.update`](repo-update.html) and
[`Repo.delete`](repo-delete.html).

When you attempt to create a persistent object that has matching
values to an existing entity for a key defined in the `PType`, the
results are currently backend-specific. For MongoDB, in the absence of
a [primary key](../ptype/primary-keys.html), a
`DuplicateKeyValException` will be thrown. If you have a primary
key, a `DuplicateKeyValException` will only be thrown for the
primary key, and only when the primary key is not hashed.

On Cassandra, no check for duplicate key values is made. We
[plan](https://www.pivotaltracker.com/story/show/107958610) to give
the user finer control over this behavior in the future.

{% assign prevTitle = "persistent state" %}
{% assign prevLink  = "persistent-state.html" %}
{% assign upTitle   = "the repository" %}
{% assign upLink    = "." %}
{% assign nextTitle = "repo.retrieve" %}
{% assign nextLink  = "retrieve.html" %}
{% include navigate.html %}
