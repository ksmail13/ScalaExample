package chap3

sealed trait List[+A]
object Nil extends List[Nothing]
case class Cons[+A](h: A, t: List[A]) extends List[A]

object List {

  def apply[A](list: A*): List[A] = {
    if(list.isEmpty) Nil
    else Cons(list.head, apply(list.tail: _*))
  }

  def sum(l: List[Int]): Int = l match {
    case Nil => 0
    case Cons(h, t) => h + sum(t)
  }

  def product(l: List[Int]): Int = l match {
    case Nil => 1
    case Cons(h,t) => h * product(t)
  }

}
