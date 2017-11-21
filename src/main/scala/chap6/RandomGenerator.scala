package chap6

import chap6.State.State

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

  def int: Rand[Int] = _.nextInt

  def nonNegativeLessThen(n:Int):Rand[Int] =
    State.flatMap(nonNegativeInt)(i => {
      val mod = i % n
      if (i + (n - 1) - mod >= 0)
        rng => (mod, rng) // return new function
      else
        nonNegativeLessThen(n)
    })

  /**
    * generate 0 or positive integer number
    *
    * @param randomGenerator seed
    * @return 0 or positive integer number
    */
  def nonNegativeInt(randomGenerator: RandomGenerator): (Int, RandomGenerator) = {
    @annotation.tailrec
    def loop(t: (Int, RandomGenerator)): (Int, RandomGenerator) =
      if (t._1 < 0) loop(t._2.nextInt)
      else t

    loop(randomGenerator.nextInt)
  }

  def doubleNaive(rng: RandomGenerator): (Double, RandomGenerator) = {
    val t = RandomGenerator.nonNegativeInt(rng)
    val n = t._1 - (t._1 / Int.MaxValue)
    (n.toDouble / Int.MaxValue, t._2)
  }

  def double(rng: RandomGenerator): (Double, RandomGenerator) =
    State.map(nonNegativeInt)(i => (i - i / Int.MaxValue).toDouble / Int.MaxValue)(rng)

  def intDouble(rng: RandomGenerator): ((Int, Double), RandomGenerator) =
    State.map2(int, double)((_, _))(rng)

  def doubleInt(rng: RandomGenerator): ((Double, Int), RandomGenerator) =
    State.map2(double, int)((_, _))(rng)

  def double3(rng: RandomGenerator): ((Double, Double, Double), RandomGenerator) =
    State.map2(State.map2(double, double)((_, _)), double)((a, b) => (a._1, a._2, b))(rng)

  def ints(count: Int)(rng: RandomGenerator): (List[Int], RandomGenerator) =
    State.sequenceLoop(List.fill(count)(int))(rng)
}


