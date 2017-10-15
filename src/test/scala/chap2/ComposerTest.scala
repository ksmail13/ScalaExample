package chap2

import org.scalatest.FlatSpec

class ComposerTest extends FlatSpec {

  behavior of "Composer"

  it should "Composer(Integer.toHexString, Integer.parseInt) will hex integer string to integer value" in {
    val decStrToHexStr = Composer(Integer.toHexString, Integer.parseInt)
    assertResult("a")(decStrToHexStr("10"))
  }

}
