package chap6

import org.scalatest.{FlatSpec, Matchers}

class StateTest extends FlatSpec with Matchers {
  behavior of "State"

  it should "unit generate function that return original data" in {
    val a = "1234"
    val generator = SimpleRandomGenerator(1235)

    val result = State.unit(a)(generator)

    result == (a, generator)
  }

  it should "map generate function that return mapping data" in {
    val nonNegativeInt = State.map(RandomGenerator.nonNegativeInt)(i => i - i % 2)
    val generator = SimpleRandomGenerator(42)

    def loop(n: Int, rng:RandomGenerator): Unit = {
      if(n == 0) return
      val tuple = nonNegativeInt(rng)
      tuple._1 should be > 0
      loop(n-1, tuple._2)
    }

    loop(10, generator)
  }
}
