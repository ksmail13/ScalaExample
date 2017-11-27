package chap6

import org.scalatest.FlatSpec

class MachineInputTest extends FlatSpec {

  behavior of "Input"

  it should "sell 4 candies" in {
    val inputs = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
    val machine = Machine(locked = true, 5, 10)
    val result = Machine.simulateMachine(inputs).run(machine)

    result equals ((14, 1), Machine(locked = false, 1, 14))
  }

  behavior of "Coin"

  it should "not work on unlocked machine" in {
    val inputs = List(Coin)
    val machine = Machine(locked = false, 5, 10)
    val result = Machine.simulateMachine(inputs).run(machine)

    result equals ((10, 5), Machine(locked = false, 5, 10))
  }

  behavior of "Turn"

  it should "not work on locked machine" in {
    val inputs = List(Turn)
    val machine = Machine(locked = true, 5, 10)
    val result = Machine.simulateMachine(inputs).run(machine)

    result equals ((10, 5), Machine(locked = true, 5, 10))
  }

  behavior of "Machine"

  it should "working only candy exist" in {
    val inputs = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
    val machine = Machine(locked = true, 0, 10)
    val result = Machine.simulateMachine(inputs).run(machine)

    result equals ((10, 0), machine)
  }

}
