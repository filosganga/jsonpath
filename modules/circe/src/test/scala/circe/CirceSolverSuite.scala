package circe

import io.circe.Json
import io.circe.literal.*
import io.circe.testing.instances.*
import com.filippodeluca.jsonpath.core.ast.*

import org.scalacheck.Prop._

import circe.CirceSolver

class CirceSolverSuite extends munit.ScalaCheckSuite {

  import CirceSolver._

  test("solve string literal should return the string literal") {
    forAll { (str: String, json: Json) =>
      assertEquals(solve(StringLiteral(str), json), Vector(Json.fromString(str)))
    }
  }

  test("solve boolean literal should return the boolean literal") {
    forAll { (bool: Boolean, json: Json) =>
      assertEquals(solve(BooleanLiteral(bool), json), Vector(Json.fromBoolean(bool)))
    }
  }

  test("solve this should return the source") {
    forAll { (json: Json) =>
      assertEquals(solve(This, json), Vector(json))
    }
  }

  test("solve a property should return the property value") {
    forAllNoShrink { (name: String, value: Json) =>
      assertEquals(
        solve(Property(StringLiteral(name), This), Json.obj(name -> value)),
        Vector(value)
      )
    }
  }

  test("solve a non-existant property should return Json.Nul") {
    forAll { (name: String, current: Json) =>
      val jsonWithoutProperty = current.mapObject(_.filterKeys(_ != name))
      assertEquals(solve(Property(StringLiteral(name), This), jsonWithoutProperty), Vector.empty)
    }
  }

  test("solve a nested property should return the property value") {
    forAll { (parentName: String, name: String, value: Json) =>
      val jsonObj = Json.obj(parentName -> Json.obj(name -> value))
      val exp = Property(StringLiteral(name), Property(StringLiteral(parentName), This))
      assertEquals(solve(exp, jsonObj), Vector(value))
    }
  }

  test("solve a non-existant nested property should return Json.Null") {
    forAll { (parentName: String, name: String, current: Json) =>
      val source = current.mapObject(_.filterKeys(_ != name))
      val exp = Property(StringLiteral(name), Property(StringLiteral(parentName), This))
      assertEquals(solve(exp, source), Vector.empty)
    }
  }

  test("solve array index should retun the array item") {
    forAll { (value: Json, otherValue: Json) =>
      val current = Json.arr(otherValue, value, otherValue)
      val exp = ArrayIndex(NumberLiteral(1), This)
      assertEquals(solve(exp, current), Vector(value))
    }
  }

  test("solve $.foo[1] should retun 2") {
    val source = Json.obj(
      "foo" -> Json.arr(
        Json.fromInt(1),
        Json.fromInt(2),
        Json.fromInt(3)
      )
    )

    val exp = ArrayIndex(NumberLiteral(1), Property(StringLiteral("foo"), This))
    assertEquals(solve(exp, source), Vector(Json.fromInt(2)))
  }

  test("solve $.foo.bar[2] should retun 3") {
    val source = Json.obj(
      "foo" -> Json.obj(
        "bar" -> Json.arr(
          Json.fromInt(1),
          Json.fromInt(2),
          Json.fromInt(3)
        )
      )
    )

    val exp = ArrayIndex(
      NumberLiteral(1),
      Property(StringLiteral("bar"), Property(StringLiteral("foo"), This))
    )
    assertEquals(solve(exp, source), Vector(Json.fromInt(2)))
  }

  test("solve $.foo[0].bar should retun 1") {
    val source = Json.obj(
      "foo" -> Json.arr(
        Json.obj("bar" -> Json.fromInt(1)),
        Json.obj("bar" -> Json.fromInt(2)),
        Json.obj("bar" -> Json.fromInt(3))
      )
    )

    val exp = Property(
      StringLiteral("bar"),
      ArrayIndex(NumberLiteral(0), Property(StringLiteral("foo"), This))
    )
    assertEquals(solve(exp, source), Vector(Json.fromInt(1)))
  }

  test("solve $.foo[0].bar should retun 1") {
    val source = Json.obj(
      "foo" -> Json.arr(
        Json.obj("bar" -> Json.fromInt(1)),
        Json.obj("bar" -> Json.fromInt(2)),
        Json.obj("bar" -> Json.fromInt(3))
      )
    )

    val exp = Property(
      StringLiteral("bar"),
      ArrayIndex(NumberLiteral(0), Property(StringLiteral("foo"), This))
    )
    assertEquals(solve(exp, source), Vector(Json.fromInt(1)))
  }

  test("solve $.foo.[(@.arrName)][2] should retun 3") {
    val source = Json.obj(
      "foo" -> Json.obj(
        "arrName" -> Json.fromString("bar"),
        "bar" -> Json.arr(
          Json.fromInt(1),
          Json.fromInt(2),
          Json.fromInt(3)
        )
      )
    )

    val exp = ArrayIndex(
      NumberLiteral(1),
      Property(Property(StringLiteral("arrName"), This), Property(StringLiteral("foo"), This))
    )
    assertEquals(solve(exp, source), Vector(Json.fromInt(2)))
  }

  test("solve $.foo.bar[($.foo.barindex)] should retun 3") {
    val source = Json.obj(
      "foo" -> Json.obj(
        "barIndex" -> Json.fromInt(2),
        "bar" -> Json.arr(
          Json.fromInt(1),
          Json.fromInt(2),
          Json.fromInt(3)
        )
      )
    )

    val exp = ArrayIndex(
      Property(StringLiteral("barIndex"), Property(StringLiteral("foo"), Root)),
      Property(StringLiteral("bar"), Property(StringLiteral("foo"), Root))
    )
    assertEquals(solve(exp, source), Vector(Json.fromInt(3)))
  }

  test("solve $.foo[?(@.enabled)] should retun Null") {
    val source = Json.obj(
      "foo" -> Json.obj(
        "enabled" -> Json.fromBoolean(false),
        "bar" -> Json.fromString("Baz")
      )
    )

    val exp = Filter(Property(StringLiteral("enabled"), This), Property(StringLiteral("foo"), Root))
    assertEquals(solve(exp, source), Vector.empty)
  }

  test("solve $.foo[?(@.enabled)] should retun foo") {

    val foo = Json.obj(
      "enabled" -> Json.fromBoolean(true),
      "bar" -> Json.fromString("Baz")
    )

    val source = Json.obj(
      "foo" -> foo
    )

    val exp = Filter(Property(StringLiteral("enabled"), This), Property(StringLiteral("foo"), Root))
    assertEquals(solve(exp, source), Vector(foo))
  }

  test("solve $.foo[?(@.enabled)] should return matching items") {
    val source = Json.obj(
      "foo" -> Json.obj(
        "enabled" -> Json.fromBoolean(false),
        "bar" -> Json.fromString("Baz")
      )
    )

    val exp = Filter(Property(StringLiteral("enabled"), This), Property(StringLiteral("foo"), Root))
    assertEquals(solve(exp, source), Vector.empty)
  }

  test("solve $.foo[?(@.enabled == true)] should return matching items") {
    val source = Json.obj(
      "foo" -> Json.obj(
        "enabled" -> Json.fromBoolean(false),
        "bar" -> Json.fromString("Baz")
      )
    )

    val exp = Filter(
      Eq(Property(StringLiteral("enabled"), This), BooleanLiteral(true)),
      Property(StringLiteral("foo"), Root)
    )
    assertEquals(solve(exp, source), Vector.empty)
  }

  test("solve $.foo[?(@.enabled)] should return all the array elements") {

    val expected = Json.obj(
      "enabled" -> Json.fromBoolean(true),
      "bar" -> Json.fromString("Bazzinga")
    )

    val source = Json.obj(
      "foo" -> Json.arr(
        Json.obj(
          "enabled" -> Json.fromBoolean(false),
          "bar" -> Json.fromString("Baz")
        ),
        Json.obj(
          "enabled" -> Json.fromBoolean(true),
          "bar" -> Json.fromString("Bazzinga")
        )
      )
    )

    val exp = Filter(Property(StringLiteral("enabled"), This), Property(StringLiteral("foo"), Root))

    assertEquals(solve(exp, source), Vector(expected))
  }

  test("solve $.foo[?(@.enabled)].bar should return Bazzinga") {

    val expected = Json.fromString("Bazzinga")

    val source = Json.obj(
      "foo" -> Json.arr(
        Json.obj(
          "enabled" -> Json.fromBoolean(false),
          "bar" -> Json.fromString("Baz")
        ),
        Json.obj(
          "enabled" -> Json.fromBoolean(true),
          "bar" -> expected
        )
      )
    )

    val exp = Property(
      StringLiteral("bar"),
      Filter(Property(StringLiteral("enabled"), This), Property(StringLiteral("foo"), Root))
    )

    assertEquals(solve(exp, source), Vector(expected))
  }

  test("$ == 'foo' should return true") {

    val source = Json.fromString("foo")

    val exp = Eq(This, StringLiteral("foo"))

    assertEquals(solve(exp, source), Vector(Json.True))
  }

  test("$ == 'bar' should return false") {

    val source = Json.fromString("foo")

    val exp = Eq(This, StringLiteral("bar"))

    assertEquals(solve(exp, source), Vector(Json.False))
  }

  test("[1::] on [0,1,2,3,4,5] should return [1,2,3,4,5]") {

    val source = Json.arr(
      Json.fromInt(0),
      Json.fromInt(1),
      Json.fromInt(2),
      Json.fromInt(3),
      Json.fromInt(4),
      Json.fromInt(5)
    )

    val expected = Json.arr(
      Json.fromInt(1),
      Json.fromInt(2),
      Json.fromInt(3),
      Json.fromInt(4),
      Json.fromInt(5)
    )

    val exp = ArraySlice(NumberLiteral(1), NullLiteral, NullLiteral, Root)

    assertEquals(solve(exp, source), Vector(expected))
  }

  test("[:3:] on [0,1,2,3,4,5] should return [0,1,2]") {

    val source = Json.arr(
      Json.fromInt(0),
      Json.fromInt(1),
      Json.fromInt(2),
      Json.fromInt(3),
      Json.fromInt(4),
      Json.fromInt(5)
    )

    val expected = Json.arr(
      Json.fromInt(0),
      Json.fromInt(1),
      Json.fromInt(2)
    )

    val exp = ArraySlice(NullLiteral, NumberLiteral(3), NullLiteral, Root)

    assertEquals(solve(exp, source), Vector(expected))
  }

  test("[1:3:] on [0,1,2,3,4,5] should return [1,2]") {

    val source = Json.arr(
      Json.fromInt(0),
      Json.fromInt(1),
      Json.fromInt(2),
      Json.fromInt(3),
      Json.fromInt(4),
      Json.fromInt(5)
    )

    val expected = Json.arr(
      Json.fromInt(1),
      Json.fromInt(2)
    )

    val exp = ArraySlice(NumberLiteral(1), NumberLiteral(3), NullLiteral, Root)

    assertEquals(solve(exp, source), Vector(expected))
  }

  test("[1:4:2] on [0,1,2,3,4,5] should return [1,3]") {

    val source = Json.arr(
      Json.fromInt(0),
      Json.fromInt(1),
      Json.fromInt(2),
      Json.fromInt(3),
      Json.fromInt(4),
      Json.fromInt(5)
    )

    val expected = Json.arr(
      Json.fromInt(1),
      Json.fromInt(3)
    )

    val exp = ArraySlice(NumberLiteral(1), NumberLiteral(4), NumberLiteral(2), Root)

    assertEquals(solve(exp, source), Vector(expected))
  }

  test("[-1] on [0,1,2,3,4,5] should return [5]") {

    val source = Json.arr(
      Json.fromInt(0),
      Json.fromInt(1),
      Json.fromInt(2),
      Json.fromInt(3),
      Json.fromInt(4),
      Json.fromInt(5)
    )

    val expected = Json.arr(
      Json.fromInt(5)
    )

    val exp = ArraySlice(NumberLiteral(-1), NullLiteral, NullLiteral, Root)

    assertEquals(solve(exp, source), Vector(expected))
  }

  test("[-3:-1] on [0,1,2,3,4,5] should return [3,4]") {

    val source = Json.arr(
      Json.fromInt(0),
      Json.fromInt(1),
      Json.fromInt(2),
      Json.fromInt(3),
      Json.fromInt(4),
      Json.fromInt(5)
    )

    val expected = Json.arr(
      Json.fromInt(3),
      Json.fromInt(4)
    )

    val exp = ArraySlice(NumberLiteral(-3), NumberLiteral(-1), NullLiteral, Root)

    assertEquals(solve(exp, source), Vector(expected))
  }

  test("[3::-1] on [0,1,2,3,4,5] should return [3,2,1,0]") {

    val source = Json.arr(
      Json.fromInt(0),
      Json.fromInt(1),
      Json.fromInt(2),
      Json.fromInt(3),
      Json.fromInt(4),
      Json.fromInt(5)
    )

    val expected = Json.arr(
      Json.fromInt(3),
      Json.fromInt(2),
      Json.fromInt(1),
      Json.fromInt(0)
    )

    val exp = ArraySlice(NumberLiteral(3), NullLiteral, NumberLiteral(-1), Root)

    assertEquals(solve(exp, source), Vector(expected))
  }

  test("[-2::-1] on [0,1,2,3,4,5] should return [4, 3,2,1,0]") {

    val source = Json.arr(
      Json.fromInt(0),
      Json.fromInt(1),
      Json.fromInt(2),
      Json.fromInt(3),
      Json.fromInt(4),
      Json.fromInt(5)
    )

    val expected = Json.arr(
      Json.fromInt(4),
      Json.fromInt(3),
      Json.fromInt(2),
      Json.fromInt(1),
      Json.fromInt(0)
    )

    val exp = ArraySlice(NumberLiteral(-2), NullLiteral, NumberLiteral(-1), Root)

    assertEquals(solve(exp, source), Vector(expected))
  }

  test("[:-4:-1] on [0,1,2,3,4,5] should return [3,2,1,0]") {

    val source = Json.arr(
      Json.fromInt(0),
      Json.fromInt(1),
      Json.fromInt(2),
      Json.fromInt(3),
      Json.fromInt(4),
      Json.fromInt(5)
    )

    val expected = Json.arr(
      Json.fromInt(5),
      Json.fromInt(4),
      Json.fromInt(3)
    )

    val exp = ArraySlice(NullLiteral, NumberLiteral(-4), NumberLiteral(-1), Root)

    assertEquals(solve(exp, source), Vector(expected))
  }

  test("[:3:-1] on [0,1,2,3,4,5] should return [5,4]") {

    val source = Json.arr(
      Json.fromInt(0),
      Json.fromInt(1),
      Json.fromInt(2),
      Json.fromInt(3),
      Json.fromInt(4),
      Json.fromInt(5)
    )

    val expected = Json.arr(
      Json.fromInt(5),
      Json.fromInt(4)
    )

    val exp = ArraySlice(NullLiteral, NumberLiteral(3), NumberLiteral(-1), Root)

    assertEquals(solve(exp, source), Vector(expected))
  }

  test("[4:3:-1] on [0,1,2,3,4,5] should return [5,4]") {

    val source = Json.arr(
      Json.fromInt(0),
      Json.fromInt(1),
      Json.fromInt(2),
      Json.fromInt(3),
      Json.fromInt(4),
      Json.fromInt(5)
    )

    val expected = Json.arr(
      Json.fromInt(4)
    )

    val exp = ArraySlice(NumberLiteral(4), NumberLiteral(3), NumberLiteral(-1), Root)

    assertEquals(solve(exp, source), Vector(expected))
  }

  test("[-2:-4:-1] on [0,1,2,3,4,5] should return [4,3]") {

    val source = Json.arr(
      Json.fromInt(0),
      Json.fromInt(1),
      Json.fromInt(2),
      Json.fromInt(3),
      Json.fromInt(4),
      Json.fromInt(5)
    )

    val expected = Json.arr(
      Json.fromInt(4),
      Json.fromInt(3)
    )

    val exp = ArraySlice(NumberLiteral(-2), NumberLiteral(-4), NumberLiteral(-1), Root)

    assertEquals(solve(exp, source), Vector(expected))
  }

  test("[::-2] on [0,1,2,3,4,5] should return [4,2,0]") {

    val source = Json.arr(
      Json.fromInt(0),
      Json.fromInt(1),
      Json.fromInt(2),
      Json.fromInt(3),
      Json.fromInt(4),
      Json.fromInt(5)
    )

    val expected = Json.arr(
      Json.fromInt(4),
      Json.fromInt(2),
      Json.fromInt(0)
    )

    val exp = ArraySlice(NullLiteral, NullLiteral, NumberLiteral(-2), Root)

    assertEquals(solve(exp, source), Vector(expected))
  }

//   numbers = arr.array('i', [0, 1, 2, 3, 4, 5])

// copy = numbers[:-4:-1]

  /*import array as arr

numbers = arr.array('i', [1, 2, 3, 4, 5])
copy = numbers[2::-1]
array('i', [3, 2, 1])

numbers = arr.array('i', [1, 2, 3, 4, 5])
copy = numbers[-1::-1]
array('i', [5, 4, 3, 2, 1])



   */
}
