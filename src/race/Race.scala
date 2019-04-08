package race

class Race {

  // Testing code for testing purposes
  val testCar = new Car(new Driver("Luka"))
  val testCar2 = new Car(new Driver("Olli"))
  private val track = new Track("Test Track", buildMap(IO.testTrack1), Array(testCar, testCar2))
  //buildDrivers(IO.readDrivers).foreach(println)

  //Moves the car whose turn it is to the given position
  //Also gives the turn to the next Player
  def nextMove(pos: Coordinates) = {
    track.moveCar(nextTurnIndex, pos)
    if (nextTurnIndex + 1 == track.cars.length) nextTurnIndex = 0
    else nextTurnIndex += 1
  }

  def getMap: Array[Array[SquareType]] = this.track.raceTrack

  // Keeps track of whose turn it is
  private var nextTurnIndex = 0

  // Returns the possible moves for the player whose turn it is
  def nextMovementOptions = track.moveOptions(nextTurnIndex)

  // Converts a two-dimensional array of Char to a two-dimensional array of SquareType
  def buildMap(source: Array[Array[Char]]): Array[Array[SquareType]] = {
    val result = Array.ofDim[SquareType](source.length, source.head.length)
    for (i <- result.indices) {
      for (j <- result.head.indices) {
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