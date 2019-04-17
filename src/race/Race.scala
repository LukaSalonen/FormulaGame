package race

class Race {

  private var track = new Track("default track", Array[Array[SquareType]](Array(new Obstacle)), Array(new Car(new Driver("default driver"))))

  def setTrack(trackName: String, cars: Array[String]): Unit = {
    //TODO make best laptimes visible in the sidebar
    val map = buildMap(IO.readTrack(trackName + ".txt")._2)
    track = new Track(trackName, map, buildCars(cars))
  }

  //Moves the car whose turn it is to the given position
  //Also gives the turn to the next Player
  def nextMove(pos: Coordinates) = {
    if (!nextCar.isCrashed) {
      track.moveCar(nextTurnIndex, pos)
      track.updateCheckpointPassed(nextTurnIndex)
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

  def gameOver: Boolean = {

    val potWinner = track.cars.indices.find(a => track.lastMoveWon(a) && track.checkpointPassed(a))

    track.cars.filter(!_.isCrashed).size == 1 || potWinner.isDefined

  }
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
          case 'c' => result(i)(j) = new Checkpoint
          case _   => result(i)(j) = new Obstacle
        }
      }
    }
    result
  }

  def saveNewDriver(name: String) = IO.writeNewDriver(name)

  def buildCars(names: Array[String]): Array[Car] = buildDrivers(names).map(new Car(_))

  def buildDrivers(names: Array[String]): Array[Driver] = names.map(new Driver(_))

  def getDrivers: Array[String] = IO.readDrivers

  def getTracks: Array[String] = IO.availableTracks

  def atEndOfGame() = {
    val cars = track.cars
    val potWinner = track.cars.indices.find(a => track.lastMoveWon(a) && track.checkpointPassed(a))

    if (cars.filter(!_.isCrashed).size == 1) {
      winner = Some(cars.filter(!_.isCrashed).head)
    } else if (potWinner.isDefined) {
      winner = Some(track.cars(potWinner.get))
      val first = winner.get
      val fileName = track.nameOfTrack.toLowerCase() + ".txt"
      IO.writeLaptimes(fileName, (first.driver.name, first.getTurnsTaken))
    }
  }

}