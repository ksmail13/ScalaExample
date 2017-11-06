package chap3

import org.scalatest.{FlatSpec, Matchers}

class ListTest extends FlatSpec with Matchers {
  behavior of "List"

  it should "Create List" in {
    List(1,2,3,4,5)
  }

  it should "get summery" in {
    val l = List(1,2,3,4,5)
    assertResult(15)(List.sum(l))
  }

  it should "return 0 when list Empty" in {
    val l = Nil
    assertResult(0)(List.sum(l))
  }

  it should "get product" in {
    val l = List(1,2,3,4,0)
    assertResult(0)(List.product(l))
  }

  it should "return 1 when list empty" in {
    val l = Nil
    assertResult(1)(List.product(l))
  }

  it should "drop head element" in {
    val l = List(1,2,3,4,5)
    val tail = List.tail(l)
    println(l)
    println(tail)
  }

  it should "set new head" in {
    var l = List(1,2,3,4,5)
    l = List.setHead(l, 10)

    println(l)
  }

  it should "drop some elements" in {
    var l = List(1,2,3,4,5)

    l = List.drop(l, 3)

    println(l)
  }

  it should "drop element while condition correct" in {
    var l = List(1,2,3,4,5)
    l = List.dropWhile(l)(_ > 3)

    println(l)
  }

  it should "drop last element" in {
    var l = List(1,2,3,4,5)

    l = List.init(l)

    println(l)
  }

  it should "copy list" in {
    val l = List(1, 2, 3, 4, 5)
    val copiedL = List.foldRight(l, Nil: List[Int])(Cons(_, _))

    (l == copiedL) shouldBe true
  }

  it should "get list length" in {
    val l = List(1,1,1,1,1)

    List.length(l) equals 5
  }

  it should "throw StackOverflowError when long list length" in {
    var l = List(1,2,3,4,5,6,7,8,9,10)
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

    the [StackOverflowError] thrownBy List.length(l)
  }

  it should "get long list length" in {
    var l = List(1,2,3,4,5,6,7,8,9,10)
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
    println(List.sum(l), List.sumLoop(l))
    List.sum(l) equals List.sumLoop(l)
  }

  it should "product and productLoop always return same result" in {
    val l = List(1, 2, 3, 4, 5)
    println(List.product(l), List.productLoop(l))
    List.product(l) equals List.productLoop(l)
  }

  it should "reverse list" in {
    val l = List(1,2,3,4,5)
    println(List.reverse(l))
  }

  it should "reverse and reverseFold has same result" in {
    val l = List(1,2,3,4,5)
    List.reverse(l) equals List.reverseFold(l)
  }

}
