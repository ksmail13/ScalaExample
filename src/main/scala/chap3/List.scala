package chap3

sealed trait List[+A]

object Nil extends List[Nothing]
case class Cons[+A](h: A, t: List[A]) extends List[A]

object List {
  def apply[A](list: A*): List[A] = {
    if (list.isEmpty) Nil
    else Cons(list.head, apply(list.tail: _*))
  }

  def append[A](as: List[A], bs: List[A]): List[A] = as match {
    case Nil => bs
    case Cons(h, t) => Cons(h, append(t, bs))
  }

  def sumLoop(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def productLoop(l: List[Int]): Int = foldLeft(l, 1)(_ * _)

  def sum(l: List[Int]): Int =
    foldRight(l, 0)(_ + _)

  def product(l: List[Int]): Int =
    foldRight(l, 1)(_ * _)

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) =>
      if (n > 1) drop(t, n - 1)
      else t
  }

  /**
    * drop element while 'f' return true
    * curryied function
    *
    * @param l list
    * @param f evaluate function
    * @tparam A some type
    * @return dropped list
    */
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) =>
      if (f(h)) dropWhile(t)(f)
      else l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }


  /**
    * get length by foldright
    *
    * @param l item list
    * @tparam A item type
    * @return length
    */
  def length[A](l: List[A]): Int = foldRight(l, 0)((_, x) => x + 1)

  /**
    * get length by foldleft
    *
    * @param l item list
    * @tparam A item type
    * @return length
    */
  def lengthLoop[A](l: List[A]): Int = foldLeft(l, 0)((x, _) => x + 1)

  def reverse[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) =>
      @annotation.tailrec
      def loop(as:List[A], ns:List[A]): List[A] = as match {
        case Cons(ah, Nil) => Cons(ah, ns)
        case Cons(ah, at) => loop(at, Cons(ah, ns))
      }

      loop(t, Cons(h, Nil))
  }

  def reverseFold[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) =>
      foldLeft(t, Cons(h, Nil))((bs, a) => Cons(a, bs))
  }


  /**
    * fold list from end of list
    * f evaluate after traverse
    *
    * @param l data list
    * @param z default data
    * @param f evaluate function
    * @tparam A list type
    * @tparam B return type
    * @return
    */
  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  /**
    * fold list from front of list
    * f evaluate before traverse
    *
    * @param l data list
    * @param z default data
    * @param f evaluate function
    * @tparam A list type
    * @tparam B return type
    * @return
    */
  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) =>
      foldLeft(t, f(z, h))(f)
  }

  def foldRightByLeft[A, B](l: List[A], z:B)(f:(A, B) => B): B =
    foldLeft(reverseFold(l), z)((b, a) => f(a,b))

  def foldLeftByRight[A, B](l: List[A], z:B)(f:(B, A) => B): B =
    foldRight(reverseFold(l), z)((a, b) => f(b,a))

  def appendByFold[A](as: List[A], bs: List[A]): List[A] =
    foldRightByLeft(as, bs)((a, bs) => Cons(a, bs))

  /**
    * flatten Nth dimension array to N-1th dimension array
    * @param l 2nd dimension array
    * @tparam A array element type
    * @return flatten array
    */
  def flatten[A](l:List[List[A]]): List[A] =
    foldLeft(l, Nil:List[A])((a, b) => appendByFold(a, b))

  def map[A, B](l:List[A])(f:A => B): List[B]
    = foldRightByLeft(l, Nil:List[B])((a, b) => Cons(f(a), b))

  def filter[A](l: List[A])(f: A => Boolean): List[A]
    = foldRightByLeft(l, Nil:List[A])((a, b) => if (f(a)) Cons(a, filter(b)(f)) else filter(b)(f))

  def flatMap[A, B](l:List[A])(f:A => List[B]): List[B] =
    foldRightByLeft(l, Nil:List[B])((a, b) => appendByFold(f(a), b))

}
