package race

import java.io.FileNotFoundException
import java.io.IOException
import scala.collection.mutable.Buffer
import scalafx.Includes.eventClosureWrapperWithParam
import scalafx.Includes.jfxActionEvent2sfx
import scalafx.Includes.jfxMouseEvent2sfx
import scalafx.application.JFXApp
import scalafx.event.ActionEvent
import scalafx.geometry.Orientation
import scalafx.geometry.Pos
import scalafx.scene.Scene
import scalafx.scene.control.Alert
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.Button
import scalafx.scene.control.ButtonType
import scalafx.scene.control.ChoiceDialog
import scalafx.scene.control.Label
import scalafx.scene.control.Menu
import scalafx.scene.control.MenuBar
import scalafx.scene.control.MenuItem
import scalafx.scene.control.SeparatorMenuItem
import scalafx.scene.control.TextArea
import scalafx.scene.control.TextInputDialog
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout.BorderPane
import scalafx.scene.layout.GridPane
import scalafx.scene.layout.Pane
import scalafx.scene.layout.Priority
import scalafx.scene.layout.StackPane
import scalafx.scene.layout.TilePane
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color.Crimson
import scalafx.scene.paint.Color.Cyan
import scalafx.scene.paint.Color.DARKGREY
import scalafx.scene.paint.Color.LightGreen
import scalafx.scene.paint.Color.LightGrey
import scalafx.scene.paint.Color.LightSalmon
import scalafx.scene.paint.Color.Yellow
import scalafx.scene.shape.Circle
import scalafx.scene.shape.Rectangle
import scalafx.scene.text.Text

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

      newGameButton.onAction = (ae: ActionEvent) => {
        topBorderPane.left = leftTile
        topBorderPane.center = gridPane
        newGame()
      }

      newGameItem.onAction = (ae: ActionEvent) => {
        topBorderPane.left = leftTile
        topBorderPane.center = gridPane
        newGame()
      }

      addPlayerItem.onAction = (ae: ActionEvent) => {
        newDriver()
      }

      exitItem.onAction = (ae: ActionEvent) => {
        sys.exit(0)
      }

    }

  }

  // error dialog for exception handling
  def errorAlert(content: String, e: Exception) = {

    val label = new Label("The exception stacktrace was:")
    val textArea = new TextArea {
      text = e.toString() + "\n" + e.getStackTraceString
      editable = false
      wrapText = true
      maxWidth = Double.MaxValue
      maxHeight = Double.MaxValue
      vgrow = Priority.Always
      hgrow = Priority.Always
    }
    val expContent = new GridPane {
      maxWidth = Double.MaxValue
      add(label, 0, 0)
      add(textArea, 0, 1)
    }

    new Alert(AlertType.Error) {
      initOwner(stage)
      title = "Exception Occured"
      headerText = None
      contentText = content
      dialogPane().setExpandableContent(expContent)
    }.showAndWait()
  }

  // draws the current 
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
          case s: Checkpoint    => nextColor = Color.Burlywood
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
      }
    }
  }

  // draws the movement options for the next turn
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

  // draws info tiles that tell who is who and whose turn it is
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

  // calls all the build functions to draw everything
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

  // asks player what type of game they want to play and loads it
  // handels exceptions
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
            case None =>
          }
        }
        case None =>
      }
    } catch {
      case e: TooFewPlayersException         => errorAlert("There are not enough saved players for that.", e)
      case e: TooFewStartingSquaresException => errorAlert("That map has too few starting squares for that many players.", e)
      case e: FileNotFoundException          => errorAlert("A required file was not found.", e)
      case e: IOException                    => errorAlert("Something went wrong while reading/writing a file.", e)
      case e: Exception                      => errorAlert("Something went wrong in the method newGame().", e)
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

  // moves the game forward by one turn
  def takeTurn(toPos: Coordinates): Unit = {

    if (!race.nextCar.isCrashed && !race.noOptions) race.nextMove(toPos)

    while (race.nextCar.isCrashed || race.noOptions) {
      if (race.noOptions) race.nextCar.isCrashed = true
      race.nextMove(toPos)
    }

    buildEverything()

    if (race.gameOver) {
      try {
        race.atEndOfGame()
      } catch {
        case e: IOException => errorAlert("Something went wrong while reading/writing a file.", e)
        case e: Exception   => errorAlert("Something went wrong in the method atEndOfGame().", e)
      }
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
        case Some(button2) if button2.text == "Exit" => sys.exit(0)
        case _ =>
      }
    }

  }
  
  // asks the user for a new driver to be saved to file
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