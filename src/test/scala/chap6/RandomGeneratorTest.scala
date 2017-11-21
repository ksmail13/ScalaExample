package chap6

import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec

class RandomGeneratorTest extends FlatSpec with Matchers {

  behavior of "RandomGenerator"

  it should "nextInt" in {
    val randomGenerator = SimpleRandomGenerator(42)
    val (n1, randomGenerator2) = randomGenerator.nextInt
    val (n2, randomGenerator3) = randomGenerator2.nextInt

    println(n1, randomGenerator2)
    println(n2, randomGenerator3)

    (n1, n2) equals(16159453, -1281479697)
  }

  it should "nonNegativeInt return only positive integer or 0" in {
    val randomGenerator = SimpleRandomGenerator(42)
    val (n, _) = RandomGenerator.nonNegativeInt(randomGenerator)
    //    println(n)
    (n >= 0) && (n <= Int.MaxValue) shouldBe true
  }

  it should "nonNegativeInt return only positive integer or 0, " +
    "when random number generator generate negative number." in {
    val randomGenerator = SimpleRandomGenerator(1059025964525L)
    val (n, _) = RandomGenerator.nonNegativeInt(randomGenerator)
    //    println(n)
    (n >= 0) && (n <= Int.MaxValue) shouldBe true
  }

  it should "return decimal between 0 and 1" in {
    val generator = SimpleRandomGenerator(1239813)

    @tailrec
    def loop(n: Int, t: (Double, RandomGenerator)): Unit = {
      t._1 should be >= 0.0
      t._1 should be < 1.0
      //      println(t._1)
      if (n > 1) loop(n - 1, RandomGenerator.double(t._2))
    }

    loop(10, RandomGenerator.double(generator))
  }

  it should "return int, double tuple" in {
    val generator = SimpleRandomGenerator(1239813)
    val tuple = RandomGenerator.intDouble(generator)
    println(tuple)

    tuple._1._1 shouldBe a[Integer]
    tuple._1._2 shouldBe a[java.lang.Double]
    tuple._2 shouldBe a[RandomGenerator]
  }

  it should "return double, int tuple" in {
    val generator = SimpleRandomGenerator(1239813)
    val tuple = RandomGenerator.doubleInt(generator)
    //    println(tuple)

    tuple._1._1 shouldBe a[java.lang.Double]
    tuple._1._2 shouldBe a[Integer]
    tuple._2 shouldBe a[RandomGenerator]
  }

  it should "return double, double, double tuple" in {
    val generator = SimpleRandomGenerator(1239813)
    val tuple = RandomGenerator.double3(generator)
    //    println(tuple)

    tuple._1._1 shouldBe a[java.lang.Double]
    tuple._1._2 shouldBe a[java.lang.Double]
    tuple._1._3 shouldBe a[java.lang.Double]
    tuple._2 shouldBe a[RandomGenerator]
  }

  it should "return int list" in {
    val generator = SimpleRandomGenerator(1239813)
    val tuple = RandomGenerator.ints(3)(generator) // why curried?
    println(tuple)

    tuple._1 shouldBe a[List[Integer]]
    tuple._2 shouldBe a[RandomGenerator]
  }

  it should "unit generate function that return original data" in {
    val a = "1234"
    val generator = SimpleRandomGenerator(1235)

    val result = RandomGenerator.unit(a)(generator)

    result == (a, generator)
  }

  it should "map generate function that return mapping data" in {
    val nonNegativeInt = RandomGenerator.map(RandomGenerator.nonNegativeInt)(i => i - i % 2)
    val generator = SimpleRandomGenerator(42)

    def loop(n: Int, rng:RandomGenerator): Unit = {
      if(n == 0) return
      val tuple = nonNegativeInt(rng)
      tuple._1 should be > 0
      loop(n-1, tuple._2)
    }

    loop(10, generator)
  }

  it should "nonNegativeLessThan(n) will return positive integer under n" in {
    val g = SimpleRandomGenerator(10)
    val result = RandomGenerator.nonNegativeLessThen(10)(g)
    println(result)
    result._1 should be < 10
  }
}
