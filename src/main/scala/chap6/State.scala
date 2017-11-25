package chap6

import scala.annotation.tailrec

case class State[S, +A](run: S => (A, S)) {

  def flatMap[B](g: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, newState) = run(s)
      g(a).run(newState)
    })

  def map[B](g: A => B): State[S, B] =
    flatMap(a => State(s => (g(a), s)))

}

object State {

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def map2[S, A, B, C](ra: State[S, A], rb: State[S, B])(f: (A, B) => C): State[S, C] =
    ra.flatMap(a => rb.flatMap(b => State(s => (f(a, b), s))))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    State(s => {
        @tailrec
        def loop(fs: List[State[S, A]], s: S, l: List[A]): (List[A], S) =
          fs match {
            case h :: Nil =>
              val result = h.run(s)
              (result._1 :: l, result._2)
            case h :: t =>
              val result = h.run(s)
              loop(t, result._2, result._1 :: l)
          }

        loop(fs, s, List())
      })

}
