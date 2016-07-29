---
title: feature list
layout: page
---

## Design Principles

- Supports [Domain Driven Design methodologies](manual/ddd-basics).
- Built to support multiple NoSQL back ends.
- Fully encapsulates persistence concerns.
- [Asynchronous persistence API](manual/repo/repo-api.html) using [Scala
  futures](http://docs.scala-lang.org/overviews/core/futures.html) and
  [Akka
  streams](http://doc.akka.io/docs/akka/current/scala/stream/index.html). 
- Agnostic about what kinds of data you want to persist.
  - That said, longevity provides built-in support for common DDD
    persistence patterns such as aggregate roots, events, and read
    views.

## Current Features

Current features are described in detail in the [user
manual](manual). They include:

- Mongo, Cassandra, and in-memory back ends.
- Supports construction of persistent types using:
  - Basic values such as `Int`, `String`, `DateTime`, etc.
  - Collection types such as `Option`, `Set`, and `List`.
  - Case classes.
  - Polymorphic traits.
- Supports domain-level keys and indexes.
- Provides pre-built repositories with simple, reactive APIs.
- Provides a query DSL for retrieval or streaming of multiple records.
- Provides pre-built integration tests that exercise your repositories
  against a real database.
- Fully featured in-memory repositories for use in other integration
  testing.

## Features planned for 1.0 release

These features are tracked on the [story
board](https://www.pivotaltracker.com/n/projects/1231978) under the
label [longevity 1.0 post
MMP](https://www.pivotaltracker.com/epic/show/1769462) (please follow
the "add/view stories" link on the right side of the linked page):

- Enhanced query support.
- Optimistic locking.
- Soft deletes.
- Schema generation.
- Logging.