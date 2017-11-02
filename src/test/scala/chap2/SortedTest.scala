package chap2

import org.scalatest.FlatSpec

class SortedTest extends FlatSpec {

  behavior of "SortedTest"

  it should "apply will return true when parameter sorted" in {
    val ordered: Array[String] = "a,b,c,d,e,f,g,h,i,j,k,l".split(",")
    assertResult(true)(Sorted(ordered, (a: String, b:String) => {
      a.compareTo(b) <= 0 }))
  }

  it should "apply will return false when parameter unsorted" in {
    val shuffled: Array[String] = "f,e,g,s,f,e,sd,f,e,s,f,e,s,d".split(",")
    assertResult(false)(Sorted(shuffled, (a: String, b:String) => {
      a.compareTo(b) <= 0 }))
  }

}
