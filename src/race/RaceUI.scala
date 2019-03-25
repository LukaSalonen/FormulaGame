package race

import scalafx.Includes._
import scalafx.application._
import scalafx.stage._
import scalafx.scene.Scene
import scalafx.scene.layout._

object RaceUI extends App {
  //JFXApp
  //TODO
  /*
  stage = new JFXApp.PrimaryStage {
    title = "Test window pls ignore"
    width = 800
    height = 600

    scene = new Scene(new javafx.scene.Scene(root))
  }

  lazy val root = new GridPane {}
*/

  val yup: Array[Array[SquareType]] = Array(
                                      Array(new StartingPlace,new StartingPlace,new StartingPlace,new StartingPlace,new StartingPlace),
                                      Array(new Driveway,new Driveway,new Driveway,new Driveway,new Driveway),
                                      Array(new Driveway,new Driveway,new Driveway,new Driveway,new Driveway),
                                      Array(new Driveway,new Driveway,new Driveway,new Driveway,new Driveway),
                                      Array(new Driveway,new Driveway,new Driveway,new Driveway,new Driveway)
                                      )
  
  
  val testCar = new Car(new Driver("Luka"))
  val testCar2 = new Car(new Driver("Luka2"))
  val testTrack = new Track(yup, Array(testCar,testCar2))
  println(testCar.driver)
  println(testTrack)

}