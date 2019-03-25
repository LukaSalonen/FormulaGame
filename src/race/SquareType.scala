package race

abstract class SquareType {
  var carHere: Option[Car] = None
  val canPassThrough: Boolean = true
}


class Obstacle extends SquareType {
  override val canPassThrough = false
  override def toString = "Obstacle"
}

class Driveway extends SquareType {
  override def toString = "Driveway"
}

class GoalLine extends SquareType { 
  override def toString = "GoalLine"
}

class StartingPlace extends SquareType {
  override def toString = "StartingPlace"
}
