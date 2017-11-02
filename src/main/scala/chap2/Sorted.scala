package chap2

object Sorted {

  /**
    * Check `Array` is sorted
    * @param arr Array that checked
    * @param ordered Function that check elements are sorted
    * @tparam A Array type
    * @return `true` when array is sorted. Otherwise `false`
    */
  def apply[A](arr: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(arr: Array[A], n: Int): Boolean = {
      if (n == arr.length-1) true
      else if(!ordered(arr(n), arr(n+1))) false
      else loop(arr, n+1)
    }

    loop(arr, 0)
  }
}
