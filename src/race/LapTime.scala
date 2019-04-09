package race

class LapTime(val driver: Driver, val nameOfTrack: String, private var turnsTaken: Int = 0) {
  
  def incrementTurns = turnsTaken += 1
  def getTurns = turnsTaken
}