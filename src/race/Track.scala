package race

import scala.math._

class Track(val nameOfTrack: String, val raceTrack: Array[Array[SquareType]], val cars: Array[Car]) {

  // Track the dimensions of the track
  val width = raceTrack.head.length
  val height = raceTrack.length

  // where the cars where before their last move
  // Used for calculating the next possible moves
  val previousLocation: Array[Coordinates] = Array.ofDim[Coordinates](cars.length)

  //Sets cars up for the game
  this.initializeTrack

  // returns the positions where the given player can move
  def moveOptions(playerIndex: Int): Array[Coordinates] = {
    var result = Array.ofDim[Coordinates](9)
    val currentLocation = locationOfCar(playerIndex)
    val lastLocation = previousLocation(playerIndex)
    var nextCenter = currentLocation.moveTheDifference(lastLocation)

    if (currentLocation.equals(lastLocation)) {
      val closestGoal = closestGoalLine(currentLocation).getOrElse(new Coordinates(currentLocation.x + 1, currentLocation.y))
      if (currentLocation.x != closestGoal.x) {
        if (currentLocation.x > closestGoal.x) nextCenter = new Coordinates(currentLocation.x + 1, currentLocation.y)
        else nextCenter = new Coordinates(currentLocation.x - 1, currentLocation.y)
      } else if (currentLocation.y != closestGoal.y) {
        if (currentLocation.y > closestGoal.y) nextCenter = new Coordinates(currentLocation.x, currentLocation.y + 1)
        else nextCenter = new Coordinates(currentLocation.x, currentLocation.y - 1)
      }
    }

    result(0) = nextCenter
    result(1) = nextCenter.addToXY(-1, -1)
    result(2) = nextCenter.addToXY(0, -1)
    result(3) = nextCenter.addToXY(1, -1)
    result(4) = nextCenter.addToXY(-1, 0)
    result(5) = nextCenter.addToXY(1, 0)
    result(6) = nextCenter.addToXY(-1, 1)
    result(7) = nextCenter.addToXY(0, 1)
    result(8) = nextCenter.addToXY(1, 1)

    result = result.filter(b => b.x >= 0 && b.y >= 0).filter(a => squareAtPos(a).canPassThrough) //index out of bounds TODO

    result
  }

  // Gives coordinates of a given players car
  def locationOfCar(playerIndex: Int): Coordinates = {
    var result = new Coordinates(-1, -1)
    for (i <- 0 until this.height) {
      for (j <- 0 until this.width) {
        val current = raceTrack(i)(j)
        if (current.carHere == Some(cars(playerIndex))) result = coordinatesOfSquare(current)
      }
    }
    result
  }

  // Returns a car whose last move crossed the finish line or none if no car did.
  def lastMoveWon(carIndex: Int): Boolean = {

    val goalLine = findGoal.map(coordinatesOfSquare(_))
    var goalPoints = (new Coordinates(goalLine.minBy(_.x).x, goalLine.minBy(_.y).y), new Coordinates(goalLine.maxBy(_.x).x, goalLine.maxBy(_.y).y))
    val carPoints = (previousLocation(carIndex), locationOfCar(carIndex))

    def onSegment(p: Coordinates, q: Coordinates, r: Coordinates): Boolean = {
      (q.x <= max(p.x, r.x) && q.x >= min(p.x, r.x) && q.y <= max(p.y, r.y) && q.y >= min(p.y, r.y))
    }
    
    def orientation(p: Coordinates, q: Coordinates, r: Coordinates): Int = {
      val value = (q.y - p.y) * (r.x - q.x) - (q.x - p.x) * (r.y - q.y)
      if(value == 0) 0
      else if(value > 0) 1
      else 2
    }
    
    def doIntersect(p1: Coordinates, q1: Coordinates, p2: Coordinates, q2: Coordinates): Boolean = {
      val o1 = orientation(p1, q1, p2) 
      val o2 = orientation(p1, q1, q2)
      val o3 = orientation(p2, q2, p1)
      val o4 = orientation(p2, q2, q1)
      
      if (o1 != o2 && o3 != o4) true
      else if(o1 == 0 && onSegment(p1, p2, q1)) true
      else if(o2 == 0 && onSegment(p1, q2, q1)) true
      else if(o3 == 0 && onSegment(p2, p1, q2)) true
      else if(o4 == 0 && onSegment(p2, q1, q2)) true
      else false
    }

    doIntersect(carPoints._1, carPoints._2, goalPoints._1, goalPoints._2)
  }

  def findGoal: Array[SquareType] = {

    val result = scala.collection.mutable.Buffer[SquareType]()

    for (i <- 0 until this.height) {
      for (j <- 0 until this.width) {
        val current = raceTrack(i)(j)
        if (current.toString == "GoalLine") result += current
      }
    }

    result.toArray
  }

  def squareAtPos(pos: Coordinates) = raceTrack(pos.y)(pos.x)

  // moves the given player to the target position
  def moveCar(playerIndex: Int, target: Coordinates) = {
    val currentPos = locationOfCar(playerIndex)
    if (!currentPos.equals(target)) {
      val currentSquare = squareAtPos(currentPos)
      val targetSquare = squareAtPos(target)
      currentSquare.carHere.foreach(_.increaseTurnsTaken)
      targetSquare.carHere = currentSquare.carHere
      currentSquare.carHere = None
      previousLocation(playerIndex) = currentPos
    }
  }

  // Finds the closest StartingPlace square within 4 squares
  // used for calculating moveOptions for a car on its first turn
  private def closestGoalLine(pos: Coordinates): Option[Coordinates] = {
    var result: Option[Coordinates] = None
    for (i <- 1 to 5) {
      if (result.isEmpty) {
        var testArray = Array(new Coordinates(pos.x - i, pos.y), new Coordinates(pos.x + i, pos.y),
          new Coordinates(pos.x, pos.y - i), new Coordinates(pos.x, pos.y + i))

        result.foreach(currentPos => (if (raceTrack(currentPos.x)(currentPos.y).toString == "GoalLine") result = Some(currentPos)))
      }
    }
    result
  }

  // Sets cars to random StartingPlaces and sets their previousLocations to their starting ones.
  private def initializeTrack = {

    var startingSquares = scala.collection.mutable.Buffer[SquareType]()

    for (i <- raceTrack.indices) {
      for (j <- raceTrack(i).indices) {
        val current = raceTrack(i)(j)
        if (current.toString == "StartingPlace") startingSquares += current
      }
    }
    require(startingSquares.length >= cars.length)
    if (startingSquares.length < cars.length) println("placingCarsOnStart kusee") //TODO exception

    else {
      startingSquares = scala.util.Random.shuffle(startingSquares)
      for (k <- cars.indices) {
        startingSquares(k).carHere = Some(cars(k))
        val test = coordinatesOfSquare(startingSquares(k))
        previousLocation(k) = coordinatesOfSquare(startingSquares(k))
      }

    }
  }

  // Gets you coordinates of a certain square
  private def coordinatesOfSquare(square: SquareType): Coordinates = {
    var result = new Coordinates(-1, -1)
    for (i <- raceTrack.indices) {
      for (j <- raceTrack(i).indices) {
        if (square == raceTrack(i)(j)) result = new Coordinates(j, i)
      }
    }
    result
  }

  //For testing purposes
  override def toString = {
    var result = ""

    for (i <- raceTrack.indices) {
      result += "|"
      for (j <- raceTrack(i).indices) {
        val current = raceTrack(i)(j)
        result += (if (current.toString == "Driveway" && current.carHere == None) "." else if (current.toString == "Obstacle") "b" else if (current.carHere != None) "X" else "s")
      }
      result += "|\n"
    }

    result
  }

}