package race

class LapTime(driver: Driver, nameOfTrack: String) {
  private var turnsTaken = 0
  
  def incrementTurns = turnsTaken += 1
  def getTurns = turnsTaken
}