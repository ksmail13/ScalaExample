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

    (n1, n2) equals (16159453, -1281479697)
  }

  it should "nonNegativeInt return only positive integer or 0" in {
    val randomGenerator = SimpleRandomGenerator(42)
    val (n, _) = RandomGenerator.nonNegativeInt(randomGenerator)
    println(n)
    (n >= 0) && (n <= Int.MaxValue) shouldBe true
  }

  it should "nonNegativeInt return only positive integer or 0, " +
              "when random number generator generate negative number." in {
    val randomGenerator = SimpleRandomGenerator(1059025964525L)
    val (n, _) = RandomGenerator.nonNegativeInt(randomGenerator)
    println(n)
    (n >= 0) && (n <= Int.MaxValue) shouldBe true
  }

  it should "return decimal between 0 and 1" in {
    val generator = SimpleRandomGenerator(1239813)
    @tailrec
    def loop(n:Int, t:(Double, RandomGenerator)):Unit = {
      t._1 should be >= 0.0
      t._1 should be < 1.0
      println(t._1)
      if(n > 1) loop(n-1, t._2.double)
    }
    loop(10, generator.double)
  }

}
