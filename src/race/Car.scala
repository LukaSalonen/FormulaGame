package race


class Car(val driver: Driver) {
  
  private var turnsTaken = 0
  var crashed = false
  
  def increaseTurnsTaken = turnsTaken +=1
  def getTurnsTaken = this.turnsTaken
}