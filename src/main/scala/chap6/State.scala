package chap6

import scala.annotation.tailrec

object State {
  type State[S, +A] = S => (A, S)

  def unit[S, A](a: A): State[S, A] = s => (a, s)

  def mapNaive[S, A, B](s: State[S, A])(f: A => B): State[S, B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def map2Naive[S, A, B, C](ra: State[S, A], rb: State[S, B])(f: (A, B) => C): State[S, C] =
    rng => {
      val t1 = ra(rng)
      val t2 = rb(t1._2)
      (f(t1._1, t2._1), t2._2)
    }

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    rng => {
      def function(fs: List[State[S, A]]): (List[A], S) =
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

  def sequenceLoop[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    rng => {
      @tailrec
      def loop(fs: List[State[S, A]], rng: S, l: List[A]): (List[A], S) =
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

  def flatMap[S, A, B](f:State[S, A])(g:A=> State[S, B]): State[S, B] =
    rng => {
      val result = f(rng)
      g(result._1)(result._2)
    }

  def map[S, A, B](s: State[S, A])(f: A => B): State[S, B] =
    flatMap(s)(a => rng => (f(a), rng))

  def map2[S, A, B, C](ra: State[S, A], rb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(ra)(a => flatMap(rb)(b => rng => (f(a, b), rng)))
}
