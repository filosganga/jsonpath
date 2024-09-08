# Yet another JSONPath implementation

This JSONPath implementation in composable, such as `foo` or `1 < 2` or `$.foo == $.bar` are all valid JSONPath expressions. The solver returns a Vector of possible match, an empty vectors if nothing matches.

## Installation

First, make sure to replace `<latestVersion>` with the actual latest version of the library you want to use. You can find the latest version number in the project's documentation or on its Maven or SBT repository page.

Once you have the latest version number, add the following lines to your `build.sbt` file:

```sbt
val jsonPathVersion = "<latestVersion>"

// For scalaJs or scalaNative cross build, use %%% instead of %%
libraryDependencies ++= List(
  "com.filippodeluca" %% "jsonpath-parser" % jsonPathVersion,
  "com.filippodeluca" %% "jsonpath-circe" % jsonPathVersion
)
```

After you add these lines to your build.sbt file, run sbt update to download the dependencies. Then, you can import the necessary classes and use them in your code:

```scala
import io.circe.literal._

import com.filippodeluca.jsonpath.parser.JsonPathParser
import com.filippodeluca.jsonpath.circe.CirceSolver

val json = json"""
{
  "store": {
    "book": [
      {
        "category": "reference",
        "author": "Nigel Rees",
        "title": "Sayings of the Century",
        "price": 8.95
      },
      {
        "category": "fiction",
        "author": "Evelyn Waugh",
        "title": "Sword of Honour",
        "price": 12.99,
        "isbn": "0-553-21311-3"
      },
      {
        "category": "fiction",
        "author": "Herman Melville",
        "title": "Moby Dick",
        "isbn": "0-553-21311-4",
        "price": 8.99
      }
    ]
  }
}
"""

val result = JsonPathParser.parse("$.store.book[*].author").map { jsonPath =>
  CirceSolver.solve(jsonPath, json)
}

println(result.map(.noSpace).mkstring)
```

## Differences from other implementations

Unlike many other JSONPath implementations, this library prioritizes composability and simplicity. Every subpath in a given expression is a valid path in and of itself, making it easier to reason about and build complex queries. For example, you might be able to query for `$.store.books[?(@.price > 10)].title`, but you wouldn't be able to query for `@.store.books[4].price > 10`, even though the latter is a valid expression in many programming languages.

In this implementation instead, you can compose paths in more flexible and powerful ways. For example, you can query for `@.store.books[4].price > 10`, or for `@.store.books[?(@.price > 10)].title`, or for `$.store.books[?(@.price > @.store.budget)].title`, and so on. This makes it easier to write complex queries and to reuse parts of queries in different contexts.

The composability of this implementation also makes it simpler to reason about, because you can break down complex paths into smaller, more understandable sub-paths. It also allows you to reuse sub-paths in other paths, making your code more modular and easier to maintain.

Additionally, the AST-based approach used by this implementation makes it easier to reason about and debug complex queries. The parsed query can be stored as an AST allowing for more efficient transmission over a network, because the query can be represented as structured data rather than a string. It also allows for caching of the query, since the AST can be reused for multiple evaluations of the same query, potentially saving processing time.

This is a significant advantage over other JSONPath implementations that evaluate the query directly from a string each time it is needed, which can be slower and less efficient.

## The AST

Under the hood there is an AST to model the expression, that can them be solved by several model. At this point in time only circe is availabel, in the circe module and CirceSolver object.

### AST nodes

#### `Root`

Represents the root of the JSON document.

Example: `$`

#### `This`

Represents the current context of the JSON document.

Example: `@`

#### `StringLiteral`

Represents a string literal.

Example: `'foo'`

#### `NumberLiteral`

Represents a number literal.

Example: `42`

#### `BooleanLiteral`

Represents a boolean literal.

Example: true

#### `NullLiteral`

Represents an absent liternal

#### `Property(name, target)`

Represents the `name` property access on the `target`

Example: `$.foo`

#### `ArrayIndex(index, target)`

Represents an array index access. The ArrayIndex node contains an Exp node representing the index to access.

Example: `$[0]`

#### `ArraySlice(start, end, step, target)`

Represents an array slice. The ArraySlice node contains three Exp nodes representing the start, the end and the step of the slice. It is inspired to python array slicing.

Examples:

- $[1:3]
- $[1:3:1]
- $[:3]
- $[:3:2]
- $[-5:-3]

#### `Wildcard(target)`

Represents a wildcard access. When a JSONPath traverse this node, it will become a projection: each following selector is applied to each result of the previous selector.

Example: `$.store.book[*].title` will return the titles of all the books in the store

#### `Filter(exp, target)`

Represents a filter expression. The Filter node contains an Exp node that represents the filter expression, and a Context node that represents the context to apply the filter to.

Example: `$[?(@.age > 30)]`

TODO Add operators, union, projection, function call

### AST translation

Examples of JSONPath expressions translated to the AST:

- `$` = `Root`
- `$.foo` - `Property(StringLiteral("foo"), Root)`
- `$[0]` - `ArrayIndex(NumberLiteral(0), Root)`
- `$[*]` - `Wildcard(Root)`
- `$[?(@.age > 30)]` - `Filter(Gt(Property(StringLiteral("age"), This), NumberLiteral(30)), Root)`
- `$.store.book[2]` - `ArrayIndex(NumberLiteral(2), Property(StringLiteral("book"), Property(StringLiteral("store"), Root)))`
- `$.store.book[*].author` - `Property(StringLiteral("author"), Wildcard(Property(StringLiteral("book"), Property(StringLiteral("store"), Root))))`

## The must-have books example

```scala
import io.circe.literal._
import com.filippodeluca.jsonpath._

val json = json"""
{
  "books": [
    {
      "title": "The Great Gatsby",
      "author": "F. Scott Fitzgerald",
      "year": 1925,
      "genre": "novel"
    },
    {
      "title": "The Catcher in the Rye",
      "author": "J.D. Salinger",
      "year": 1951,
      "genre": "novel"
    },
    {
      "title": "1984",
      "author": "George Orwell",
      "year": 1949,
      "genre": "novel"
    }
  ]
}
"""

val expression = "$.books[?(@.year > 1950)].title"

val result = JsonPathParser.parse(expression).map { jsonPath =>
  CirceSolver.solve(jsonPath, json)
}

println(result)
```
