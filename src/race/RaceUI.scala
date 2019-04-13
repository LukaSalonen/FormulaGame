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
import scalafx.scene.control.ChoiceDialog
import scalafx.scene.control.ButtonType

object RaceUI extends JFXApp {

  var race = new Race
  var lPane: Option[TilePane] = None
  var rPane: Option[GridPane] = None
  val carColors = Array(Crimson, Yellow, LightGreen, Cyan)

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

      val rootPane = new BorderPane

      val leftTile = new TilePane
      lPane = Some(leftTile)
      leftTile.prefWidth <== (rootPane.width / 6)
      leftTile.prefHeight <== (rootPane.height - menuBar.height)
      leftTile.orientation = Orientation.Horizontal

      val gridPane = new GridPane
      rPane = Some(gridPane)
      gridPane.gridLinesVisible = false
      gridPane.hgap = 1
      gridPane.vgap = 1
      gridPane.alignment_=(Pos.TopLeft)

      val topBorderPane = new BorderPane

      val newGameButton = new Button("New Game")
      topBorderPane.center = newGameButton
      newGameButton.alignmentInParent_=(Pos.Center)

      rootPane.top = menuBar
      rootPane.center = topBorderPane
      root = rootPane

      //Action handlers start here

      drawTest.onAction = (ae: ActionEvent) => {
        buildEverything()
      }

      newGameButton.onAction = (ae: ActionEvent) => {
        topBorderPane.left = leftTile
        topBorderPane.center = gridPane
        newGame()
      }

      newGameItem.onAction = (ae: ActionEvent) => {
        newGame()
      }

      addPlayerItem.onAction = (ae: ActionEvent) => {
        newDriver()
      }

      exitItem.onAction = (ae: ActionEvent) => {
        sys.exit(0)
      }

      onKeyPressed = (ke: KeyEvent) => {
        ke.code match {
          case KeyCode.Space => {
            buildEverything()
          }
          case KeyCode.Shift => {

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

        if (current.carHere.isDefined && !current.carHere.get.isCrashed) {
          val circle = Circle(10)
          circle.radius <== (gp.width / (3 * race.getMap.head.size))
          circle.fill = carColors(race.getCars.indexOf(current.carHere.get))
          circle.alignmentInParent_=(Pos.Center)
          gp.add(circle, j, i)
        }
        if (race.noOptions) {

          val last = race.nextCar // TODO make the car disappear if crashed
          last.isCrashed = true
          race.nextMove(new Coordinates(j, i))
          buildEverything()
        }
      }
    }
  }

  def buildMoveOptions(gp: GridPane): Unit = {
    val options = race.nextMovementOptions
    for (k <- options) {
      val circle = Circle(2)
      circle.radius <== (gp.width / (3.5 * race.getMap.head.size))
      circle.fill = Color.DarkBlue
      circle.alignmentInParent_=(Pos.Center)
      gp.add(circle, k.x, k.y)

      circle.onMouseClicked = (me: MouseEvent) => {
        takeTurn(new Coordinates(k.x, k.y))
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
        else rect.fill = LightGrey
      } else rect.fill = LightSalmon
      val text = new Text(a.driver.name)
      text.alignmentInParent = Pos.TopLeft
      val circle = Circle(10)
      circle.alignmentInParent = Pos.TopRight
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
      if (!race.gameOver) buildMoveOptions(rp)
    } else throw new NullPointerException
  }

  def newGame(): Unit = {

    try {
      val trackOptions = race.getTracks.map(_.replaceAll(".txt", "")).map(_.toLowerCase()).sorted
      val playerOptions = race.getDrivers.sorted

      val chosenTrack = askTrack(trackOptions)
      val chosenNoOfPlayers = askNoOfPlayers()
      val chosenPlayers = chosenNoOfPlayers match {
        case Some(a) => askWhichPlayers(a, playerOptions)
        case None    => None
      }

      chosenTrack match {
        case Some(a) => {
          chosenPlayers match {
            case Some(b) => {
              race.setTrack(a, b)
              buildEverything()

            }
            case None => println("Ei uutta peliä 2")
          }
        }
        case None => println("Ei uutta peliä 1")
      }

    } catch {
      case e: Exception => println("newGame() kusee huolella")
    }

    def askTrack(tracks: Array[String]): Option[String] = {
      val dialog = new ChoiceDialog(defaultChoice = tracks.head, choices = tracks) {
        initOwner(stage)
        title = "Choose Track"
        headerText = None
        contentText = "Choose a track to play on:"
      }
      dialog.showAndWait()
    }

    def askNoOfPlayers(): Option[Int] = {
      val options = (2 to 4)
      val dialog = new ChoiceDialog(defaultChoice = 2, choices = options) {
        initOwner(stage)
        title = "Choose Number of Players"
        headerText = None
        contentText = "Choose number of players:"
      }
      dialog.showAndWait()
    }

    def askWhichPlayers(number: Int, players: Array[String]): Option[Array[String]] = {
      if (number > players.size) throw new TooFewPlayersException
      val result: Buffer[String] = Buffer()
      var playersLeft = players.toBuffer

      for (i <- 1 to number) {
        val dialog = new ChoiceDialog(defaultChoice = playersLeft.head, choices = playersLeft) {
          initOwner(stage)
          title = "Choose a Player"
          headerText = "You can add new players in the game menu."
          contentText = s"Choose player number $i:"
        }
        val current = dialog.showAndWait()
        current match {
          case Some(name) => {
            playersLeft -= name
            result += name
          }
          case None =>
        }
      }
      result.size match {
        case s if s == number => Some(result.toArray)
        case s                => None
      }
    }

  }

  def takeTurn(toPos: Coordinates): Unit = {

    if (!race.nextCar.isCrashed && !race.noOptions) race.nextMove(toPos)

    while (race.nextCar.isCrashed || race.noOptions) {
      if (race.noOptions) race.nextCar.isCrashed = true
      race.nextMove(toPos)
    }

    buildEverything()

    if (race.gameOver) {
      race.atEndOfGame()
      val button1 = new ButtonType("New Game")
      val button2 = new ButtonType("Exit")
      val alert = new Alert(AlertType.Information) {
        initOwner(stage)
        title = "Winner Winner Chicken Dinner"
        headerText = None
        buttonTypes = Seq(ButtonType.OK, button1, button2)
        contentText = race.winner match {
          case Some(a) => "The winner is " + a.driver.name + "!"
          case None    => "No winner? Something went wrong."
        }

      }
      alert.showAndWait() match {
        case Some(button1) if button1.text == "New Game" => newGame()
        case Some(button2) if button2.text == "Exit"     => sys.exit(0)
        case _                                           => println("Alles gut!")
      }
    }

  }

  def newDriver(): Boolean = {
    val dialog = new TextInputDialog {
      initOwner(stage)
      title = "Add new driver"
      headerText = None
      contentText = "Please give a name to the new driver:"
    }
    val result = dialog.showAndWait()

    result match {
      case Some(name) => {
        race.saveNewDriver(name)
        true
      }
      case None => false
    }
  }
}