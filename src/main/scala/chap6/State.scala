package chap6

import scala.annotation.tailrec

object State {
  type State[S, +A] = S => (A, S)

  def unit[S, A](a: A): State[S, A] = s => (a, s)

  def mapNaive[S, A, B](st: State[S, A])(f: A => B): State[S, B] =
    s => {
      val (a, s2) = st(s)
      (f(a), s2)
    }

  def map2Naive[S, A, B, C](ra: State[S, A], rb: State[S, B])(f: (A, B) => C): State[S, C] =
    s => {
      val t1 = ra(s)
      val t2 = rb(t1._2)
      (f(t1._1, t2._1), t2._2)
    }

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    s => {
      def function(fs: List[State[S, A]]): (List[A], S) =
        fs match {
          case h :: Nil =>
            val result = h(s)
            (List(result._1), result._2)
          case h :: t =>
            val result = function(t)
            val head = h(result._2)
            (head._1 :: result._1, head._2)
        }

      function(fs)
    }

  def sequenceLoop[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    s => {
      @tailrec
      def loop(fs: List[State[S, A]], s: S, l: List[A]): (List[A], S) =
        fs match {
          case h :: Nil =>
            val result = h(s)
            (result._1 :: l, result._2)
          case h :: t =>
            val result = h(s)
            loop(t, result._2, result._1 :: l)
        }

      loop(fs, s, List())
    }

  def flatMap[S, A, B](f:State[S, A])(g:A=> State[S, B]): State[S, B] =
    s => {
      val result = f(s)
      g(result._1)(result._2)
    }

  def map[S, A, B](s: State[S, A])(f: A => B): State[S, B] =
    flatMap(s)(a => s => (f(a), s))

  def map2[S, A, B, C](ra: State[S, A], rb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(ra)(a => flatMap(rb)(b => s => (f(a, b), s)))
}
