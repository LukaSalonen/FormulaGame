package race

class Coordinates(val x: Int, val y: Int) {
  
  def xDistance(another: Coordinates) = x - another.x
  
  def yDistance(another: Coordinates) = y- another.y
  
  def equals(another: Coordinates) = (this.x == another.x) && (this.y == another.y)
  
  def yGreaterThan(another: Coordinates) = this.y > another.y
  
  def xGreaterThan(another: Coordinates) = this.x > another.x
  
  def addToXY(a: Int, b: Int) = new Coordinates(this.x + a, this.y + b)
  
  def moveTheDifference(lastPos: Coordinates): Coordinates = {
    val moveX = this.x - lastPos.x
    val moveY = this.y - lastPos.y
    new Coordinates(this.x + moveX, this.y + moveY)
  }
  
  override def toString = s"($x,$y)"
  
}