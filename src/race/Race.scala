package race

class Race {

  private val yup: Array[Array[SquareType]] = Array(
    Array(new StartingPlace, new StartingPlace, new StartingPlace, new StartingPlace, new StartingPlace),
    Array(new Driveway, new Driveway, new Driveway, new Driveway, new Driveway),
    Array(new GoalLine, new GoalLine, new GoalLine, new Driveway, new Driveway),
    Array(new Driveway, new Driveway, new Driveway, new Driveway, new Driveway),
    Array(new Driveway, new Driveway, new Driveway, new Driveway, new Driveway))
    
  private val trackNo1 = buildMap(IO.testTrack1)

  val testCar = new Car(new Driver("Luka"))
  val testCar2 = new Car(new Driver("Olli"))
  private val track = new Track(trackNo1, Array(testCar,testCar2))

  def nextMove(pos: Coordinates) = {
    track.moveCar(nextTurnIndex, pos)
    if(nextTurnIndex + 1 == track.cars.length) nextTurnIndex = 0
    else nextTurnIndex += 1
  }
  
  def getMap: Array[Array[SquareType]] = this.trackNo1
  
  private var nextTurnIndex = 0

  def nextMovementOptions = track.moveOptions(nextTurnIndex)
  
  def buildMap(source: Array[Array[Char]]): Array[Array[SquareType]] = {
    val result = Array.ofDim[SquareType](source.length, source.head.length)
    for(i <- result.indices) {
      for(j <- result.head.indices) {
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

}