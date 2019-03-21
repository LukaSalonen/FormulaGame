package race

class Track(private val raceTrack: Array[Array[SquareType]], private val cars: Array[Car]) {
  
  //Tuple of coordinates, first is the current location, second is the last one.
  private val carLocations: Array[(Coordinates, Coordinates)] = ???  //TODO
  
  //Does not currently check if square can be driven to. TODO
  def moveCar(car: Car, newLocation: Coordinates) = {
    val carNumber = this.cars.indexOf(car)
    this.carLocations(carNumber) = (newLocation, carLocations(carNumber)._1)
  }
  
  def checkCollision(point1: Coordinates, point2: Coordinates) = ??? //TODO
  
  def checkVictory = ??? //TODO
}