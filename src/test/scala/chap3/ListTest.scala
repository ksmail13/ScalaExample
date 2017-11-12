package chap3

import org.scalatest.{FlatSpec, Matchers}

class ListTest extends FlatSpec with Matchers {
  behavior of "List"

  it should "Create List" in {
    List(1, 2, 3, 4, 5)
  }

  it should "get summery" in {
    val l = List(1, 2, 3, 4, 5)
    List.sum(l) equals 15
  }

  it should "return 0 when list Empty" in {
    val l = Nil
    List.sum(l) equals 0
  }

  it should "get product" in {
    val l = List(1, 2, 3, 4, 0)
    List.product(l) equals 0
  }

  it should "return 1 when list empty" in {
    val l = Nil
    List.product(l) equals 1
  }

  it should "drop head element" in {
    val l = List(1, 2, 3, 4, 5)
    val tail = List.tail(l)

    tail equals List(2, 3, 4, 5)
  }

  it should "set new head" in {
    val l = List(1, 2, 3, 4, 5)


    List.setHead(l, 10) equals List(10, 2, 3, 4, 5)
  }

  it should "drop some elements" in {
    var l = List(1, 2, 3, 4, 5)

    l = List.drop(l, 3)

    l equals List(4, 5)
  }

  it should "drop element while condition correct" in {
    var l = List(1, 2, 3, 4, 5)
    l = List.dropWhile(l)(_ > 3)

    l equals List(4, 5)
  }

  it should "drop last element" in {
    var l = List(1, 2, 3, 4, 5)

    l = List.init(l)

    l equals List(1, 2, 3, 4)
  }

  it should "copy list" in {
    val l = List(1, 2, 3, 4, 5)
    val copiedL = List.foldRight(l, Nil: List[Int])(Cons(_, _))

    (l == copiedL) shouldBe true
  }

  it should "get list length" in {
    val l = List(1, 1, 1, 1, 1)

    List.length(l) equals 5
  }

  it should "throw StackOverflowError when long list length" in {
    var l = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    l = List.append(l, l) // 20
    l = List.append(l, l) // 40
    l = List.append(l, l) // 80
    l = List.append(l, l) // 160
    l = List.append(l, l) // 320
    l = List.append(l, l) // 640
    l = List.append(l, l) // 1280
    l = List.append(l, l) // 5120
    l = List.append(l, l) // 10240
    l = List.append(l, l) // 10240

    the[StackOverflowError] thrownBy List.length(l)
  }

  it should "get long list length" in {
    var l = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    l = List.append(l, l) // 20
    l = List.append(l, l) // 40
    l = List.append(l, l) // 80
    l = List.append(l, l) // 160
    l = List.append(l, l) // 320
    l = List.append(l, l) // 640
    l = List.append(l, l) // 1280
    l = List.append(l, l) // 5120
    l = List.append(l, l) // 10240
    l = List.append(l, l) // 10240

    List.lengthLoop(l) equals 10240
  }

  it should "sum and sumLoop always return same result" in {
    val l = List(1, 2, 3, 4, 5)

    List.sum(l) equals List.sumLoop(l)
  }

  it should "product and productLoop always return same result" in {
    val l = List(1, 2, 3, 4, 5)

    List.product(l) equals List.productLoop(l)
  }

  it should "reverse list" in {
    val l = List(1, 2, 3, 4, 5)

    List.reverse(l) equals List(5, 4, 3, 2, 1)
  }

  it should "reverse and reverseFold has same result" in {
    val l = List(1, 2, 3, 4, 5)

    List.reverse(l) equals List.reverseFold(l)
  }

  it should "append list by appendByFold" in {
    val l1 = List(1, 2, 3, 4, 5)
    val l2 = List(5, 4, 3, 2, 1)

    List.lengthLoop(List.appendByFold(l1, l2)) equals 10

  }

  it should "flatten 2nd dimension list" in {
    val l = List(List(1, 2, 3, 4, 5), List(5, 4, 3, 2, 1), List(6, 7, 8, 9, 0))

    List.lengthLoop(List.flatten(l)) equals 15
  }

  it should "mapping each element" in {
    val l = List(1, 2, 3, 4, 5)

    val result = List.map(l)(_ + 1)
    println(result)
    result equals List(2, 3, 4, 5, 6)
  }

  it should "filter odd" in {
    val l = List(1, 2, 3, 4, 5)
    println(List.filter(l)(_ % 2 == 0))
  }

  it should "flatMap Test" in {
    val l = List(1, 2, 3, 4, 5)
    List.flatMap(l)((a) => List(a, a)) equals List(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
  }

}
