package race

abstract class SquareType {
  var carHere: Option[Car] = None
  def canPassThrough: Boolean = carHere.isEmpty
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

class Checkpoint extends SquareType {
  override def toString = "Checkpoint"
}
