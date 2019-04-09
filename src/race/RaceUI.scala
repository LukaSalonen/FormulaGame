package race

import scalafx.Includes._
import scalafx.Includes.eventClosureWrapperWithParam
import scalafx.Includes.jfxActionEvent2sfx
import scalafx.application.JFXApp
import scalafx.event.ActionEvent
import scalafx.scene.Scene
import scalafx.scene.control.Button
import scalafx.scene.control.Menu
import scalafx.scene.control.MenuBar
import scalafx.scene.control.MenuItem
import scalafx.scene.control.SeparatorMenuItem
import scalafx.scene.layout.AnchorPane
import scalafx.scene.layout.BorderPane
import scalafx.scene.layout.GridPane
import scalafx.scene.layout.StackPane
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle
import scalafx.geometry.Pos
import scalafx.scene.shape.Circle
import scalafx.scene.layout.Pane
import scalafx.scene.input.KeyEvent
import scalafx.scene.input.MouseEvent
import scalafx.scene.input.KeyCode

object RaceUI extends JFXApp {

  var race = new Race

  def testStuff = {
    race = new Race
  }

  def buildGridLayout(gp: GridPane, rp: Pane): Unit = {

    val options = race.nextMovementOptions

    for (i <- race.getMap.indices) {
      for (j <- race.getMap(i).indices) {
        val current = race.getMap(i)(j)
        val newSquare = Rectangle(50, 50)
        var nextColor = Color.Pink
        newSquare.width <== (rp.width / race.getMap.head.size)
        newSquare.height <== (rp.height / race.getMap.size)

        current.toString match {
          case "Obstacle"      => nextColor = Color.Black
          case "Driveway"      => nextColor = Color.Burlywood
          case "StartingPlace" => nextColor = Color.Grey
          case "GoalLine"      => nextColor = Color.DarkMagenta
          case _               =>
        }
        newSquare.fill = nextColor
        gp.add(newSquare, j, i)

        if (current.carHere != None) {
          val circle = Circle(13)
          circle.radius <== (rp.width / (3 * race.getMap.head.size))
          circle.fill = Color.Red
          circle.alignmentInParent_=(Pos.CENTER)
          gp.add(circle, j, i)
        }

        if (options.isEmpty) {
          
          val last = race.nextCar // TODO make the car move to the obstacel it crashed into
          race.nextMove(new Coordinates(j,i))
          last.isCrashed = true
        } else if (!race.nextCar.isCrashed) {
          for (k <- options) {
            if (k.x == j && k.y == i) {
              val circle = Circle(2)
              circle.radius <== (rp.width / (4 * race.getMap.head.size))
              circle.fill = Color.BLUEVIOLET
              circle.alignmentInParent_=(Pos.CENTER)
              gp.add(circle, j, i)

              circle.onMouseClicked = (me: MouseEvent) => {
                race.nextMove(new Coordinates(j, i))
                gp.children_=(new Pane)
                buildGridLayout(gp, rp)
              }
            }
          }
        }
      }
    }
  }

  stage = new JFXApp.PrimaryStage {
    title = "Formula Game"
    scene = new Scene(1200, 800) {
      val menuBar = new MenuBar
      val fileMenu = new Menu("File")
      val testItem = new MenuItem("test stuff")
      val exitItem = new MenuItem("Exit")
      fileMenu.items = List(testItem, new SeparatorMenuItem, exitItem)
      val editMenu = new Menu("Edit")
      val drawTest = new MenuItem("Draw course")
      val cutItem = new MenuItem("Cut")
      val copyItem = new MenuItem("Copy")
      val pasteItem = new MenuItem("Paste")
      editMenu.items = List(drawTest, cutItem, copyItem, pasteItem)
      menuBar.menus = List(fileMenu, editMenu)

      val testButton = new Button("Test button pls click!")

      val testBox = Rectangle(100, 100)
      testBox.fill = Color.Grey

      val testBox2 = Rectangle(100, 100)
      testBox2.fill = Color.Red

      var gridPane = new GridPane
      gridPane.gridLinesVisible = true
      //gridPane.add(testButton, 0, 0)
      testButton.alignmentInParent_=(Pos.CENTER)
      //gridPane.add(testBox, 0, 0)
      //gridPane.add(testBox2, 0, 1)

      gridPane.alignment_=(Pos.CENTER)
      val rootPane = new BorderPane
      rootPane.top = menuBar
      rootPane.center = gridPane
      root = rootPane

      drawTest.onAction = (ae: ActionEvent) => {
        gridPane.children_=(new Pane)
        buildGridLayout(gridPane, rootPane)
      }

      testButton.onAction = (ae: ActionEvent) => {
        testButton.text = "Gotcha bich!"
      }

      exitItem.onAction = (ae: ActionEvent) => {
        sys.exit(0)
      }

      testItem.onAction = (ae: ActionEvent) => {
        testStuff
      }

      cutItem.onAction = (ae: ActionEvent) => {
        gridPane.children_=(testButton)
      }

      onKeyPressed = (ke: KeyEvent) => {
        ke.code match {
          case KeyCode.SPACE => {
            gridPane.children_=(new Pane)
            buildGridLayout(gridPane, rootPane)
          }
          case KeyCode.SHIFT => {
            testStuff
          }
          case _ =>
        }
      }

    }

  }

}