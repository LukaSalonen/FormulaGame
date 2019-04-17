package race

import java.io.BufferedReader
import java.io.BufferedWriter
import java.io.FileNotFoundException
import java.io.FileReader
import java.io.FileWriter
import java.io.IOException
import java.io.PrintWriter
import scala.collection.mutable.Buffer
import java.io.File

object IO {

  def readDrivers: Array[String] = {

    val result = Buffer[String]()

    try {
      val fileIn = new FileReader("data/drivers.txt")
      val linesIn = new BufferedReader(fileIn)

      try {

        var currentLine = linesIn.readLine()

        while (currentLine != null) {
          result += currentLine
          currentLine = linesIn.readLine()
        }
      } finally {
        fileIn.close()
        linesIn.close()
      }
    } catch {
      case e: FileNotFoundException => println("File was not found.")
      case e: IOException           => println("Something went wrong while reading the drivers file.")
    }
    result.toArray
  }

  def writeNewDriver(name: String): Unit = {
    try {
      val fileOut = new FileWriter("data/drivers.txt", true)
      val bufferedWriter = new BufferedWriter(fileOut)
      val printWriter = new PrintWriter(bufferedWriter)
      try {

        printWriter.println(name)
        printWriter.flush()

      } finally {
        fileOut.close()
        bufferedWriter.close()
        printWriter.close()
      }
    } catch {
      case e: IOException => println("Something went wrong while writing the new player.")
    }
  }

  def readTrack(fileName: String): (String, Array[Array[Char]], Array[(String, Int)]) = {

    var trackName = "default"
    var track = Buffer[Buffer[Char]]()
    var topLaps = Buffer[(String, Int)]()

    try {
      val fr = new FileReader("data/tracks/" + fileName)
      val br = new BufferedReader(fr)

      try {
        var currentLine = br.readLine()

        while (currentLine != null) {
          currentLine match {
            case s if s.contains("nameOfTrack") => trackName = s.split(":").last
            case s if s.contains("lapTime") => {
              val stuff = s.split(":").last.split(",")
              topLaps += ((stuff(0), stuff(1).toInt))
            }
            case s if s == "" =>
            case s if s.forall(a => a == '#' || a == '¤' || a == 'g' || a == 's' || a == 'c') => {
              track += s.toBuffer
            }
            case _ =>
          }
          currentLine = br.readLine()
        }

      } finally {
        fr.close()
        br.close()
      }
    } catch {
      case e: FileNotFoundException => println("File was not found.")
      case e: IOException           => println("Something went wrong while reading the track file.")
    }

    if (trackName == "default") throw new IOException
    else if (topLaps.length != 3) throw new IOException
    else if (!track.forall(x => x.length == track.head.length)) throw new IOException
    else {

      val topLapsN = topLaps.toArray
      val trackN = Array.ofDim[Char](track.length, track.head.length)

      for (i <- track.indices) {
        trackN(i) = track(i).toArray
      }
      (trackName, trackN, topLapsN)
    }

  }

  def writeLaptimes(fileName: String, newLapData: (String, Int)): Unit = {
    val trackData = readTrack(fileName)

    var laps = trackData._3 :+ newLapData
    laps = laps.sortBy(x => x._2).dropRight(1)

    try {
      val fileOut = new FileWriter("data/tracks/" + fileName, false)
      val bufferedWriter = new BufferedWriter(fileOut)
      val printWriter = new PrintWriter(bufferedWriter)

      try {
        printWriter.println("nameOfTrack:" + trackData._1)
        for (i <- 1 to 3) {
          printWriter.println(s"lapTime$i:" + laps(i - 1)._1 + "," + laps(i - 1)._2)
        }
        printWriter.println("")
        trackData._2.foreach(x => printWriter.println(x))
        printWriter.flush()

      } finally {
        fileOut.close()
        bufferedWriter.close()
        printWriter.close()
      }
    } catch {
      case e: IOException => println("Something went wrong while writing new laptimes.")
    }
  }

  def availableTracks: Array[String] = {
    val result = Buffer[String]()
    val cwd = new File("./data/tracks")
    val listing = cwd.listFiles()

    for (i <- listing) {
      if (i.isFile && i.getName.contains(".txt")) result += i.getName
    }
    result.toArray
  }

}