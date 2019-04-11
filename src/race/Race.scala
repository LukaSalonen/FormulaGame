package race

class Race {

  // Testing code for testing purposes
  val drivers = buildDrivers(IO.readDrivers)
  val testCar = new Car(drivers(0))
  val testCar2 = new Car(drivers(1))
  val testCar3 = new Car(drivers(2))
  val testCar4 = new Car(drivers(4))
  val trackData = IO.readTrack("paris.txt")
  private val track = new Track(trackData._1, buildMap(trackData._2), Array(testCar,testCar2, testCar3, testCar4))

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
  
  def getCars: Array[Car] = this.track.cars
  
  def noOptions: Boolean = nextMovementOptions.isEmpty

  // Keeps track of whose turn it is
  private var nextTurnIndex = 0
  
  def nextCar: Car = track.cars(nextTurnIndex)
  
  var winner: Option[Car] = None
  
  def gameOver: Boolean = winner.isDefined || track.cars.forall(_.isCrashed) // TODO game also over if only one car remains

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

  // TODO make last car not crashed winner 
  
  def writeNewDriverToFile(name: String) = IO.writeNewDriver(name)
  
  def buildDrivers(names: Array[String]): Array[Driver] = names.map(new Driver(_))
  
  def atEndOfGame = {
    if(winner.isDefined) {
      val first = winner.get
      val fileName = track.nameOfTrack.toLowerCase() + ".txt"
      IO.writeLaptimes(fileName, (first.driver.name, first.getTurnsTaken))
    }
  }

}