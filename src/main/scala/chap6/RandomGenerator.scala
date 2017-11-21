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
  type Rand[+A] = (RandomGenerator => (A, RandomGenerator))

  def int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def mapNaive[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def map2Naive[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val t1 = ra(rng)
      val t2 = rb(t1._2)
      (f(t1._1, t2._1), t2._2)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => {
      def function(fs: List[Rand[A]]): (List[A], RandomGenerator) =
        fs match {
          case h :: Nil =>
            val result = h(rng)
            (List(result._1), result._2)
          case h :: t =>
            val result = function(t)
            val head = h(result._2)
            (head._1 :: result._1, head._2)
        }

      function(fs)
    }

  def sequenceLoop[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => {
      @tailrec
      def loop(fs: List[Rand[A]], rng: RandomGenerator, l: List[A]): (List[A], RandomGenerator) =
        fs match {
          case h :: Nil =>
            val result = h(rng)
            (result._1 :: l, result._2)
          case h :: t =>
            val result = h(rng)
            loop(t, result._2, result._1 :: l)
        }

      loop(fs, rng, List())
    }

  def flatMap[A, B](f:Rand[A])(g:A=> Rand[B]): Rand[B] =
    rng => {
      val result = f(rng)
      g(result._1)(result._2)
    }

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => rng => (f(a), rng))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => flatMap(rb)(b => rng => (f(a, b), rng)))

  def nonNegativeLessThen(n:Int):Rand[Int] =
    flatMap(nonNegativeInt)(i => {
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
    map(nonNegativeInt)(i => (i - i / Int.MaxValue).toDouble / Int.MaxValue)(rng)

  def intDouble(rng: RandomGenerator): ((Int, Double), RandomGenerator) =
    map2(int, double)((_, _))(rng)

  def doubleInt(rng: RandomGenerator): ((Double, Int), RandomGenerator) =
    map2(double, int)((_, _))(rng)

  def double3(rng: RandomGenerator): ((Double, Double, Double), RandomGenerator) =
    map2(map2(double, double)((_, _)), double)((a, b) => (a._1, a._2, b))(rng)

  def ints(count: Int)(rng: RandomGenerator): (List[Int], RandomGenerator) =
    sequenceLoop(List.fill(count)(int))(rng)
}


