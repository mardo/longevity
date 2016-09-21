---
title: repo crud spec
layout: page
---

The `RepoCrudSpec` iterates over all of the repositories in your
`LongevityContext`, and tests basic CRUD operations for each of
them. This includes testing `Repo.retrieve` method against every
[key](../ptype/keys.html) defined in your [persistent
types](../ptype).

Two `RepoCrudSpecs` are provided for you in the `LongevityContext`:
`repoCrudSpec`, and `inMemRepoCrudSpec`. The latter uses an in-memory
database. The former executes tests using a test database. The
database connection details are specified in the [longevity
config](../context/config.html). The database back end is specified by
the longevity context's [persistence strategy](../context/pstrat.html).

The repo crud specs are [ScalaTest](http://www.scalatest.org/) suites:

```scala
val spec: org.scalatest.Suite = longevityContext.repoCrudSpec
```

In a typical evocation of ScalaTest, for instance from within SBT, a
classpath scan is performed, and only top-level `Suites` -
non-abstract classes that are defined directly within a package - are
found. So to get one of these repo crud specs to run, you have to nest
it in a top-level class. For example, to run both in-memory and
against a real database:

```scala
import org.scalatest.Suites
import scala.concurrent.ExecutionContext.Implicits.global

class BlogCrudSpec extends Suites(
  longevityContext.repoCrudSpec,
  longevityContext.inMemRepoCrudSpec)
```

ScalaTest is an optional dependency in longevity, so you'll need to
declare a dependency on ScalaTest in your own project to use the repo
crud specs.

<div class = "blue-side-bar">

Please note that due to a regression in ScalaTest 3.0.0, test suites
defined as shown here will still run, but won't produce the expected
output. We are looking into a fix, but for now, if you are using
<code>LongevityContext.repoCrudSpec</code>, you'll probably want to
use ScalaTest version 2.2.6.

</div>

You need to provide an execution context when calling
`LongevityContext.repoCrudSpec`. The easiest way to do this is to
include `import scala.concurrent.ExecutionContext.Implicits.global` at
the top of your file.

The default test data generator will not work out of the box in the
face of constraint violations causing exceptions to be thrown from
your persistent and embeddable constructors. In this case, you need to
provide custom generators for your types. This is described in the
section on [enforcing constraints](constraints.html).

{% assign prevTitle = "enforcing constraints" %}
{% assign prevLink = "constraints.html" %}
{% assign upTitle = "testing" %}
{% assign upLink = "." %}
{% assign nextTitle = "query spec" %}
{% assign nextLink = "query-spec.html" %}
{% include navigate.html %}