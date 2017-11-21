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

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f:(A,B) => C): Rand[C] =
    rng => {
      val t1 = ra(rng)
      val t2 = rb(t1._2)
      (f(t1._1, t2._1), t2._2)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => {

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

  def intDouble(rng: RandomGenerator):Rand[(Int, Double)] =
    map2(int, double)((_, _))

  def doubleInt(rng:RandomGenerator):Rand[(Double, Int)] =
    map2(double, int)((_, _))

  def double3(rng:RandomGenerator):Rand[(Double, Double, Double)] =
    map2(map2(double, double)((_, _)), double)((a, b) => (a._1, a._2, b))

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


