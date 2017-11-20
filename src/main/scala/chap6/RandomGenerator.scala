package chap6

trait RandomGenerator {

  // 랜덤한 숫자 값과 숫자를 생성한 후의 시드 상태를 리턴
  def nextInt: (Int, RandomGenerator)

}


case class SimpleRandomGenerator(seed:Long) extends RandomGenerator {

  private def getNextSeed(seed:Long):Long
      = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL

  def nextInt: (Int, RandomGenerator) = {
    val nextSeed = getNextSeed(seed)
    val nextRandomGenerator = SimpleRandomGenerator(nextSeed)
    ((nextSeed >>> 16).toInt, nextRandomGenerator)
  }
}

object RandomGenerator {
  type Rand[+A] = (RandomGenerator => (A, RandomGenerator))

  def int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  /**
    * generate 0 or positive integer number
    *
    * @param randomGenerator seed
    * @return 0 or positive integer number
    */
  def nonNegativeInt(randomGenerator: RandomGenerator): (Int, RandomGenerator) = {
    @annotation.tailrec
      def loop(t:(Int, RandomGenerator)): (Int, RandomGenerator) =
        if(t._1 < 0) loop(t._2.nextInt)
        else t
    loop(randomGenerator.nextInt)
  }

  def doubleNaive(rng: RandomGenerator):(Double, RandomGenerator) = {
    val t = RandomGenerator.nonNegativeInt(rng)
    val n = t._1 - (t._1 / Int.MaxValue)
    (n.toDouble/Int.MaxValue, t._2)
  }

  def double(rng: RandomGenerator): (Double, RandomGenerator) =
    map(nonNegativeInt)(i => (i - i/Int.MaxValue).toDouble/Int.MaxValue)(rng)

  def intDouble(rng: RandomGenerator):((Int, Double), RandomGenerator) = {
    val t1 = rng.nextInt
    val t2 = double(t1._2)
    ((t1._1, t2._1), t2._2)
  }

  def doubleInt(rng:RandomGenerator):((Double, Int), RandomGenerator) = {
    val t1 = double(rng)
    val t2 = t1._2.nextInt
    ((t1._1, t2._1), t2._2)
  }

  def double3(rng:RandomGenerator):((Double, Double, Double), RandomGenerator) = {
    val t1 = double(rng)
    val t2 = double(t1._2)
    val t3 = double(t2._2)
    ((t1._1, t2._1, t3._1), t3._2)
  }

  def ints(count:Int)(rng: RandomGenerator):(List[Int], RandomGenerator) = {
    @annotation.tailrec
    def loop(n: Int, rng:RandomGenerator, l:List[Int]):(List[Int], RandomGenerator) = n match {
      case 1 =>
        val t = rng.nextInt
        (t._1 :: l, t._2)
      case _ =>
        val t = rng.nextInt
        loop(n-1, t._2, t._1::l)
    }

    loop(count, rng, List())
  }
}


