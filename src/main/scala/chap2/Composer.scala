package chap2

object Composer {

  /**
    * compose 2 functions
    * Composer(g, f) => g(f(x))
    *
    * @param g evaluate after f
    * @param f evaluate first
    * @tparam A input type
    * @tparam B middle type
    * @tparam C return type
    * @return composed function
    */
  def apply[A, B, C](g: B => C, f: A => B): A => C = {
    def func(param: A): C = {
      g(f(param))
    }
    func
  }
}
