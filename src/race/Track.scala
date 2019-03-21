package race

class Track(private val raceTrack: Array[Array[SquareType]], private val cars: Array[Car]) {
  
  //Tuple of coordinates, first is the current location, second is the last one.
  private val carLocations: Array[(Coordinates, Coordinates)] = ???  //TODO
  
  def moveCar(car: Car, newLocation: Coordinates) = ??? //TODO  
  
  def checkCollision(point1: Coordinates, point2: Coordinates) = ??? //TODO
  
  def checkVictory = ??? //TODO
}