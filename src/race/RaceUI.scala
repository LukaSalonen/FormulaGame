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
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.Alert
import scalafx.scene.layout.TilePane
import javafx.scene.layout.Border
import scalafx.scene.layout.BorderStrokeStyle
import javafx.scene.layout.CornerRadii
import scalafx.scene.layout.BorderStroke
import scalafx.scene.layout.BorderWidths
import scalafx.geometry.Orientation
import scalafx.scene.text.Text
import Color._
import scala.collection.mutable.Buffer
import scalafx.scene.control.TextInputDialog

object RaceUI extends JFXApp {

  var race = new Race
  var lPane: Option[TilePane] = None
  var rPane: Option[GridPane] = None
  val carColors = Array(RED, BLUE, GREEN, ORANGE, YELLOW, CYAN, CRIMSON, DARKBLUE)

  stage = new JFXApp.PrimaryStage {
    title = "Racing Game"
    scene = new Scene(1920, 1080) {
      val menuBar = new MenuBar
      val gameMenu = new Menu("Game")
      val newGameItem = new MenuItem("New game")
      val addPlayerItem = new MenuItem("Create Player")
      val exitItem = new MenuItem("Exit")
      gameMenu.items = List(newGameItem, addPlayerItem, new SeparatorMenuItem, exitItem)
      menuBar.menus = List(gameMenu)
      val debugMenu = new Menu("Debug")
      val drawTest = new MenuItem("Draw track")
      debugMenu.items = List(drawTest)
      menuBar.menus = List(gameMenu, debugMenu)

      //val testButton = new Button("Test button pls click!")
      //gridPane.add(testButton, 0, 0)
      //testButton.alignmentInParent_=(Pos.CENTER)

      val rootPane = new BorderPane

      val leftTile = new TilePane
      lPane = Some(leftTile)
      //leftTile.border = new Border(new BorderStroke(Color.BLACK, BorderStrokeStyle.Solid,
      //CornerRadii.EMPTY, BorderWidths.Default))
      leftTile.prefWidth <== (rootPane.width / 6)
      leftTile.prefHeight <== (rootPane.height - menuBar.height)
      leftTile.orientation = Orientation.HORIZONTAL

      val gridPane = new GridPane
      rPane = Some(gridPane)
      gridPane.gridLinesVisible = false
      gridPane.hgap = 1
      gridPane.vgap = 1
      gridPane.alignment_=(Pos.TOP_LEFT)

      val topBorderPane = new BorderPane
      topBorderPane.left = leftTile
      topBorderPane.center = gridPane

      rootPane.top = menuBar
      rootPane.center = topBorderPane
      root = rootPane

      //Action handlers start here

      drawTest.onAction = (ae: ActionEvent) => {
        buildEverything()
      }

      /*
      testButton.onAction = (ae: ActionEvent) => {
        testButton.text = "Gotcha bich!"
      }
      */

      addPlayerItem.onAction = (ae: ActionEvent) => {
        val dialog = new TextInputDialog {
          initOwner(stage)
          title = "Add new driver"
          contentText = "Please give a name to the new driver:"
        }
        val result = dialog.showAndWait()
        
        result match {
          case Some(name) => race.writeNewDriverToFile(name)
          case None       =>
        }
      }
      
      exitItem.onAction = (ae: ActionEvent) => {
        sys.exit(0)
      }

      onKeyPressed = (ke: KeyEvent) => {
        ke.code match {
          case KeyCode.SPACE => {
            buildEverything()
          }
          case KeyCode.SHIFT => {
            testStuff
          }
          case KeyCode.ALT => {
            new Alert(AlertType.Information, "Dialog test!").showAndWait()
          }
          case _ =>
        }
      }

    }

  }

  def buildGridLayout(gp: GridPane): Unit = {

    val options = race.nextMovementOptions

    for (i <- race.getMap.indices) {
      for (j <- race.getMap(i).indices) {
        val current = race.getMap(i)(j)
        val newSquare = Rectangle(50, 50)
        var nextColor = Color.Pink
        newSquare.width <== (gp.width / race.getMap.head.size - 2)
        newSquare.height <== (gp.height / race.getMap.size - 1)

        current match {
          case s: Obstacle      => nextColor = Color.Black
          case s: Driveway      => nextColor = Color.Burlywood
          case s: StartingPlace => nextColor = Color.Grey
          case s: GoalLine      => nextColor = Color.DarkMagenta
          case _                =>
        }
        newSquare.fill = nextColor
        gp.add(newSquare, j, i)

        if (current.carHere.isDefined) {
          val circle = Circle(10)
          circle.radius <== (gp.width / (3 * race.getMap.head.size))
          circle.fill = carColors(race.getCars.indexOf(current.carHere.get))
          circle.alignmentInParent_=(Pos.CENTER)
          gp.add(circle, j, i)
        }
/*
        if (race.noOptions) {

          val last = race.nextCar // TODO make the car disappear if crashed
          last.isCrashed = true
          race.nextMove(new Coordinates(j, i))

        }
        */
      }
    }
  }

  def buildMoveOptions(gp: GridPane): Unit = {
    val options = race.nextMovementOptions
    for (k <- options) {
      val circle = Circle(2)
      circle.radius <== (gp.width / (3.5 * race.getMap.head.size))
      circle.fill = Color.BLUEVIOLET
      circle.alignmentInParent_=(Pos.CENTER)
      gp.add(circle, k.x, k.y)

      circle.onMouseClicked = (me: MouseEvent) => {
        race.nextMove(new Coordinates(k.x, k.y))
        buildEverything()
      }
    }
  }

  def buildCarInfoTiles(tp: TilePane): Unit = {
    val cars = race.getCars
    val results: Buffer[StackPane] = Buffer()

    for (a <- cars) {
      val stackPane = new StackPane
      val rect = Rectangle(tp.width.value, tp.height.value / cars.size)
      rect.width <== tp.width
      rect.height <== tp.height / cars.length

      if (!a.isCrashed) {
        if (a == race.nextCar) rect.fill = DARKGREY
        else rect.fill = LIGHTGREY

      } else rect.fill = YELLOW
      val text = new Text(a.driver.name)
      text.alignmentInParent = Pos.TOP_LEFT
      val circle = Circle(10)
      circle.alignmentInParent = Pos.TOP_RIGHT
      circle.fill = carColors(cars.indexOf(a))
      stackPane.children = List(rect, text, circle)
      results += stackPane
    }
    tp.children = results.toList
  }

  def buildEverything(): Unit = {
    if (lPane.isDefined && rPane.isDefined) {
      val lp = lPane.get
      val rp = rPane.get
      rp.children = (new Pane)
      buildGridLayout(rp)
      buildCarInfoTiles(lp)
      if(!race.gameOver) buildMoveOptions(rp)
    } else throw new NullPointerException
  }

  def testStuff = {
    race = new Race
  }
}