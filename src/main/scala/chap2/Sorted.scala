package chap2

/**
  * Functional programming in Scala
  */
object Sorted {

  def isSorted[A](arr: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(arr: Array[A], n: Int): Boolean = {
      if (n == arr.length-1) true
      else if(!ordered(arr(n), arr(n+1))) false
      else loop(arr, n+1)
    }

    loop(arr, 0)
  }

  def main(args: Array[String]): Unit = {
    println("is sorted check")
    val ordered: Array[String] = "a,b,c,d,e,f,g,h,i,j,k,l".split(",")
    val shuffled: Array[String] = "f,e,g,s,f,e,sd,f,e,s,f,e,s,d".split(",")

    val stringOrdered: (String, String) => Boolean = (a: String, b: String) => a.compareTo(b) <= 0

    println(ordered + " to is Sorted: " + isSorted(ordered, stringOrdered))
    println(shuffled + " to is Sorted: " + isSorted(shuffled, stringOrdered))
  }
}
