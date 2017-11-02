package chap2

class Curry {

  /**
    * The function make partial applied function
    * @param f function
    * @tparam A operand type
    * @tparam B operand type
    * @tparam C return type
    * @return curried function
    */
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    def func(part: A): B => C = {
      def inFunc(part2: B): C = {
        f(part, part2)
      }
      inFunc
    }
    func
  }

  /**
    * Partial applied function to double parameter function
    * @param f partial applied function
    * @tparam A partial applied parameter type
    * @tparam B other operand type
    * @tparam C return type
    * @return uncurried  function
    */
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    // 미리 프로토타입을 작성하고 생각하면 풀기 쉽다.
    def func(a: A, b: B): C = {
      f(a)(b)
    }
    func
  }

}
