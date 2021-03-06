---
title: opening and closing the connection
layout: page
---

Before you do much work with your repository, you will need to connect to the database. Also, it is
a good idea to close the connection when you are done, to conserve resources. You can open the
connection like so:

```scala
val openResult: Future[Unit] = repo.openConnection
```

You can close the connection like so:

```scala
val closeResult: Future[Unit] = repo.closeConnection
```

An alternative way to open the connection is to set the [configuration flag](../context/config.html)
`longevity.autoOpenConnection` to true. In this case, the connection will be opened when the `Repo`
is first accessed. Unlike calling `Repo.openConnection`, automatically opening the connection
happens in a synchronous, blocking manner (i.e., outside of your [effect](../context/effects.html)).
 
{% assign prevTitle = "the repository" %}
{% assign prevLink  = "." %}
{% assign upTitle   = "the repository" %}
{% assign upLink    = "." %}
{% assign nextTitle = "schema creation" %}
{% assign nextLink  = "schema-creation.html" %}
{% include navigate.html %}
