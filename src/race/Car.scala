package race


class Car(val driver: Driver) {
  
  private var turnsTaken = 0
  
  def increaseTurnsTaken = turnsTaken +=1
  def getTurnsTaken = this.turnsTaken
}