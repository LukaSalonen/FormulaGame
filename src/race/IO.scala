package race

object IO {

  def readDrivers = ??? //TODO

  def readTracks = ??? //TODO

  def writePlayers = ??? //TODO

  def writeTracks = ??? //TODO

  val testTrack1: Array[Array[Char]] = {
    Array(
      Array('#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'),
      Array('#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'),
      Array('#', '#', '#', '#', '#', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', 's', '¤', 'g', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '#', '#', '#', '#', '#'),
      Array('#', '#', '#', '#', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', 's', '¤', 'g', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '#', '#', '#', '#'),
      Array('#', '#', '#', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', 's', '¤', 'g', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '#', '#', '#'),
      Array('#', '#', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', 's', '¤', 'g', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '#', '#'),
      Array('#', '#', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', 's', '¤', 'g', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '#', '#'),
      Array('#', '#', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', 's', '¤', 'g', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '#', '#'),
      Array('#', '#', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', 's', '¤', 'g', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '#', '#'),
      Array('#', '#', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', 's', '¤', 'g', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '#', '#'),
      Array('#', '#', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', 's', '¤', 'g', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '#', '#'),
      Array('#', '#', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '#', '#'),
      Array('#', '#', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '#', '#'),
      Array('#', '#', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '#', '#'),
      Array('#', '#', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '#', '#'),
      Array('#', '#', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '#', '#'),
      Array('#', '#', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '#', '#'),
      Array('#', '#', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '#', '#'),
      Array('#', '#', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '#', '#'),
      Array('#', '#', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '#', '#'),
      Array('#', '#', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '#', '#'),
      Array('#', '#', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '#', '#'),
      Array('#', '#', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '#', '#'),
      Array('#', '#', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '#', '#'),
      Array('#', '#', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '#', '#'),
      Array('#', '#', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '#', '#'),
      Array('#', '#', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '#', '#'),
      Array('#', '#', '#', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '#', '#', '#'),
      Array('#', '#', '#', '#', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '#', '#', '#', '#'),
      Array('#', '#', '#', '#', '#', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '¤', '#', '#', '#', '#', '#'),
      Array('#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'),
      Array('#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'))
  }

}