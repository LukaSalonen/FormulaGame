package race

class Race {

  // Testing code for testing purposes
  val testCar = new Car(new Driver("Luka"))
  val testCar2 = new Car(new Driver("Olli"))
  val trackData = IO.readTrack("paris.txt")
  IO.writeLaptimes("paris.txt", ("Kimi",10))
  private val track = new Track("Test Track", buildMap(trackData._2), Array(testCar,testCar2))

  //Moves the car whose turn it is to the given position
  //Also gives the turn to the next Player
  def nextMove(pos: Coordinates) = {
    if (!nextCar.isCrashed) {
      track.moveCar(nextTurnIndex, pos)
      if(track.lastMoveWon(nextTurnIndex)) {
        winner = Some(nextCar)
      }
    }
    if (nextTurnIndex + 1 == track.cars.length) nextTurnIndex = 0
    else nextTurnIndex += 1
  }

  def getMap: Array[Array[SquareType]] = this.track.raceTrack

  // Keeps track of whose turn it is
  private var nextTurnIndex = 0
  
  def nextCar: Car = track.cars(nextTurnIndex)
  
  var winner: Option[Car] = None
  
  def gameOver: Boolean = winner.isDefined || track.cars.forall(_.isCrashed)

  // Returns the possible moves for the player whose turn it is
  def nextMovementOptions = track.moveOptions(nextTurnIndex)

  // Converts a two-dimensional array of Char to a two-dimensional array of SquareType
  def buildMap(source: Array[Array[Char]]): Array[Array[SquareType]] = {
    val result = Array.ofDim[SquareType](source.length, source.last.length)
    for (i <- result.indices) {
      for (j <- result.last.indices) {
        source(i)(j) match {
          case '#' => result(i)(j) = new Obstacle
          case '¤' => result(i)(j) = new Driveway
          case 's' => result(i)(j) = new StartingPlace
          case 'g' => result(i)(j) = new GoalLine
          case _   => result(i)(j) = new Obstacle
        }
      }
    }
    result
  }

  def buildDrivers(names: Array[String]): Array[Driver] = names.map(new Driver(_))

}