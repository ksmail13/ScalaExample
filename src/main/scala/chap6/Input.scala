package chap6

sealed trait Input

case object Coin extends Input
case object Turn extends Input
case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {

  /**
    * simulate machine by inputs
    * @param inputs input list
    * @return final result (num of candies and coins. also Machine state)
    */
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State(s => {
    val (l, ns) = State.sequence(inputs.map(inputState)).run(s)
    (l.head, ns)
  })


  private def checkAny[A](conditions: Boolean*)(success: => A, fail: => A): A = {
    if(conditions.foldLeft(false)(_ || _)) success
    else fail
  }

  /**
    * translate input to state
    * @param i input
    * @return State object for input
    */
  private def inputState(i: Input): State[Machine, (Int, Int)] = i match {
    case Coin => State(s => checkAny(s.candies <= 0, !s.locked)
            (State.unit((s.coins, s.candies)).run(s)
            , ((s.coins + 1, s.candies), Machine(locked = true, s.candies, s.coins + 1))))
    case Turn => State(s => checkAny(s.candies <= 0, s.locked)
            (State.unit((s.coins, s.candies)).run(s)
            , ((s.coins, s.candies - 1), Machine(locked = false, s.candies - 1, s.coins))))
  }

}
