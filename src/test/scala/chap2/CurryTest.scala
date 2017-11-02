package chap2

import org.scalatest.FlatSpec

class CurryTest extends FlatSpec {
  behavior of "Curry"

  it should "Curry.curry make function to partial applied function" in {
    val addInt: (Int, Int) => Int = _ + _
    val curry = new Curry()
    val addIntGenerator = curry.curry(addInt)

    val add10 = addIntGenerator(10)
    assertResult(11)(add10(1))
  }

  it should "Curry.uncurry make curried function to normal function" in {
    val addInt: (Int, Int) => Int = _ + _
    val curry = new Curry()
    val addIntGenerator = curry.curry(addInt)
    val uncurriedAddIntGenerator = curry.uncurry(addIntGenerator)

    assertResult(20)(uncurriedAddIntGenerator(10, 10))

  }
}
