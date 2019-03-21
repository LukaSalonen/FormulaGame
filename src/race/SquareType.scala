package race

abstract class SquareType 


object Obstacle extends SquareType {
  val canPassThrough = false
}

object Driveway extends SquareType {
  val canPassThrough = true
}

object GoalLine extends SquareType {
  
}