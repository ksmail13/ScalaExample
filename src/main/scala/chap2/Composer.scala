package chap2

object Composer {
  def apply[A, B, C](f: B => C, g: A => B): A => C = {
    def func(param: A): C = {
      f(g(param))
    }

    func
  }
}
