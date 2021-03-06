---
title: feature list
layout: page
---

Current features are described in detail in the [user manual](manual). They include:

- A [finally tagless](https://pchiusano.github.io/2014-05-20/scala-gadts.html) API that allows you
  to do persistence with the effect of your choice, including [Scala
  futures](http://docs.scala-lang.org/overviews/core/futures.html), [IO
  monads](https://github.com/typelevel/cats-effect), or a purely synchronous, blocking approach.
- Streaming query results with any of four different Scala streaming libraries.
- Cassandra, MongoDB, SQLite, and in-memory back ends.

- Supports flexible construction of your domain model using standard
  Scala types such as case classes, options, sets and lists.
- Compile-time DSL to express keys, queries, and indexes.
- User control of [sharding](https://docs.mongodb.com/manual/sharding/#shard-keys)/[partitioning](https://docs.datastax.com/en/cql/3.1/cql/cql_reference/refCompositePk.html).
- Polymorphic traits and controlled vocabularies.
- Boilerplate-free description of your domain model.

- Pre-built repository with simple, typesafe, effectful APIs.
- Configuration-level optimistic locking.
- Pre-built integration tests that exercise your repositories
  against a real database.
- Fully featured in-memory repositories for use in other integration
  testing.
- Test data generation.

## Upcoming Features

The current high-priority features and improvements are:

- Schema migration framework.
- Replace home-grown reflection utilities with shapeless/circe.
- First-class support for optional values.

For more information, please see the [story
board](https://www.pivotaltracker.com/n/projects/1231978). Tickets are
roughly ordered in order of current priority.
