package chap2

object Curry {

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    def func(part: A): B => C = {
      def inFunc(part2: B): C = {
        f(part, part2)
      }
      inFunc
    }
    func
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    // 미리 프로토타입을 작성하고 생각하면 풀기 쉽다.
    def func(a: A, b: B): C = {
      f(a)(b)
    }
    func
  }

  def main(args: Array[String]): Unit = {
    val addInt: (Int, Int) => Int = (a:Int, b:Int) => a + b
    val addString: (String, String) => String = (a:String, b:String) => a + b

    val addIntGenerator = curry(addInt)
    val addStringGenerator = curry(addString)

    val add10 = addIntGenerator(10)
    val addA = addStringGenerator("A")

    println(add10(1)); println(addA("B"))

    val uncurriedAddInt = uncurry(addIntGenerator)
    val uncurriedAddString = uncurry(addStringGenerator)

    println(uncurriedAddInt(10, 20))
    println(uncurriedAddString("B", "A"))

    println(uncurriedAddInt.eq(addInt))
    println(uncurriedAddString.eq(addString))
  }
}
