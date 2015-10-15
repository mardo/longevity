---
title: what is longevity?
layout: page
---

Longevity is a [Scala](http://www.scala-lang.org/) framework designed
to assist you to do [Domain Driven Design](http://dddcommunity.org/)
well. Its main focus is on the persistence layer, targetting NoSQL
document databases. For the moment,
[MongoDB](https://www.mongodb.org/) is the only database supported.

One of the core goals of Longevity is to encapsulate persistence
concerns within the persistence layer, so details such as database IDs
and version columns for optimistic locking don't leak out into other
layers of our application. This allows the implementation of our
domain classes and services to more closely resemble the entities in
our domain models.

Document databases allow us to shift our focus from the entity to the
[aggregate](http://martinfowler.com/bliki/DDD_Aggregate.html). The
aggregate plays a fundamental role in DDD, and Longevity employs a
transparent and intuitive translation between aggregates, documents,
and Scala classes.

Domain Driven Design has historically been an Object Oriented
endeavor. While we continue this tradition in Scala, we also leverage
functional concepts such as immutability and higher ordered functions
to improve on the relationship between OO and DDD.

Longevity will do for you what an ORM would, and more. It puts your
focus back on your domain, instead of mapping your objects to and from
the database.