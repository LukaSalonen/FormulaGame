package race

class Track(private val raceTrack: Array[Array[SquareType]], private val cars: Array[Car]) {

  val previousLocation: Array[Coordinates] = Array.ofDim[Coordinates](cars.length)

  placeCarsOnStart

  private def placeCarsOnStart = {
    var startingSquares = scala.collection.mutable.Buffer[SquareType]()

    for (i <- raceTrack.indices) {
      for (j <- raceTrack(i).indices) {
        val current = raceTrack(i)(j)
        if (current.toString == "StartingPlace") startingSquares += current
      }
    }

    if (startingSquares.length < cars.length) println("placingCarsOnStart kusee") //TODO throw exception

    else {
      startingSquares = scala.util.Random.shuffle(startingSquares)

      for (k <- cars.indices) {
        startingSquares(k).carHere = Some(cars(k))
        previousLocation(k) = coordinatesOfSquare(startingSquares(k))
      }

    }
  }

  private def coordinatesOfSquare(square: SquareType): Coordinates = {
    var result = new Coordinates(-1, -1)
    for (i <- raceTrack.indices) {
      for (j <- raceTrack(i).indices) {
        if (square == raceTrack(i)(j)) result = new Coordinates(j, i)
      }
    }
    result
  }

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