package chap6

import scala.annotation.tailrec

trait RandomGenerator {

  // 랜덤한 숫자 값과 숫자를 생성한 후의 시드 상태를 리턴
  def nextInt: (Int, RandomGenerator)
}

case class SimpleRandomGenerator(seed: Long) extends RandomGenerator {

  private def getNextSeed(seed: Long): Long
  = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL

  def nextInt: (Int, RandomGenerator) = {
    val nextSeed = getNextSeed(seed)
    val nextRandomGenerator = SimpleRandomGenerator(nextSeed)
    ((nextSeed >>> 16).toInt, nextRandomGenerator)
  }
}

object RandomGenerator {
  type Rand[+A] = State[RandomGenerator, A]

  def int: Rand[Int] = State(_.nextInt)

  def nonNegativeLessThen(n: Int): Rand[Int] =
    nonNegativeInt.flatMap(i => {
      val mod = i % n
      if (i + (n - 1) - mod >= 0)
        State(rng => (mod, rng))
      else
        nonNegativeLessThen(n)
    })

  /**
    * generate 0 or positive integer number
    *
    * @return 0 or positive integer number generator
    */
  def nonNegativeInt: Rand[Int] =
    int.map(i => if (i < 0) i * -1 else i)

  def double: Rand[Double] =
    nonNegativeInt.map(i => (i - i / Int.MaxValue).toDouble / Int.MaxValue)

  def intDouble: Rand[(Int, Double)] =
    State.map2(int, double)((_, _))

  def doubleInt: Rand[(Double, Int)] =
    State.map2(double, int)((_, _))

  def double3: Rand[(Double, Double, Double)] =
    State.map2(State.map2(double, double)((_, _)), double)((a, b) => (a._1, a._2, b))

  def ints(count: Int): Rand[List[Int]] =
    State.sequence(List.fill(count)(int))
}


