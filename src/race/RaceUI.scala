package race

import scalafx.Includes._
import scalafx.application._
import scalafx.stage._
import scalafx.scene.Scene
import scalafx.scene.layout._

object RaceUI extends JFXApp {
  //TODO
  stage = new JFXApp.PrimaryStage {
    title = "Test window pls ignore"
    width = 800
    height = 600

    scene = new Scene(new javafx.scene.Scene(root))
  }

  lazy val root = new GridPane {}
}