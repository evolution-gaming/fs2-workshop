import cats.effect.{IO, IOApp}
import fs2._
import fs2.concurrent.SignallingRef

import scala.concurrent.duration.DurationInt
import scala.sys.process.stringSeqToProcess


object RedLightGreenLight extends IOApp.Simple {

  val TrackSize = 100

  sealed trait GameMode
  case object Running extends GameMode
  case object Loss extends GameMode
  case object Victory extends GameMode

  case class GameState(playerPosition: Int, mode: GameMode)

  // Sets the terminal to raw mode so that we can instantly read character input
  def terminalRawMode: Stream[IO, Int] = Stream.bracket(IO(Seq("sh", "-c", "stty raw </dev/tty").!))(_ =>
    IO {
      Seq("sh", "-c", "stty cooked </dev/tty").! // reset terminal mode back to normal
      println()
    })

  // Draws the current state of the game to screen
  def draw(gameState: GameState, greenLight: Boolean): IO[Unit] = IO {
    val (player, status) = gameState.mode match {
      case Running => ("O", "")
      case Loss    => ("X", "You Lost! Press q to quit.")
      case Victory => ("☻", "You Won! Press q to quit.")
    }
    val track = "." * (gameState.playerPosition - 1) + player + "." * (TrackSize - gameState.playerPosition - 1)

    val clearLine = "\u001B[150D"
    val colorReset = "\u001B[0m"
    val bold = "\u001B[1"
    val light =
      if (gameState.mode == Victory) "\u001B[43mVICTORY!!!"
      else if (greenLight) "\u001B[42mGREEN LIGHT"
      else "\u001B[41mRED LIGHT"
    print(clearLine + track + bold + light + colorReset + " " + status)

  }

  // Sets the player input signal to the key the user is currently pressing
  def inputStream(playerInput: SignallingRef[IO, Option[Char]]): Stream[IO, Unit] = Stream.empty ++ {
    val key = Console.in.read().toChar // Blocking!
    if (key == 'q')
      Stream.empty // End input stream, which will terminate the other streams as well
    else
      Stream.eval(playerInput.set(Some(key))) ++ inputStream(playerInput)
  }

  // Changes the light between green and red periodically
  def lightChanger(greenLight: SignallingRef[IO, Boolean]): Stream[IO, Unit] =
    Stream.repeatEval(greenLight.update(!_)).metered(2.seconds)

  /**
    * Task 3: Red Light, Green Light
    *
    * The game: In Red Light, Green Light, there is a light that changes color back and forth between
    * green and red. The goal of the player is to get from start to finish, by pressing any key to move
    * their character forward. However, if the player is caught moving during red light, they will lose!
    *
    * Your task is to implement a stream that updates the current game state based on player input,
    * the state of the light, and previous game state.
    *
    * You should return a stream that is metered to update every 10 milliseconds, and concurrently runs
    * the [[lightChanger()]] stream.
    *
    * Here are the basic rules your implementation should follow:
    * - If the game is running, you should increment the player's position if the player input isn't empty.
    * - Set the player input back to None after you have done so.
    * - If the game is running and the player is moving during red light, change the game mode to Loss.
    * - If the game is running and the player position is equal to [[TrackSize]], change the game mode to Victory.
    */
  def gameLogic(gameState: SignallingRef[IO, GameState],
    greenLight: SignallingRef[IO, Boolean],
    playerInput: SignallingRef[IO, Option[Char]]
  ): Stream[IO, Unit] = ???


  override def run: IO[Unit] = for {
    gameState <- SignallingRef[IO, GameState](GameState(1, Running))
    greenLight <- SignallingRef[IO, Boolean](false)
    playerInput <- SignallingRef[IO, Option[Char]](None)
    drawStream = Stream.repeatEval(for {
      state <- gameState.get
      light <- greenLight.get
      drawing <- draw(state, light)
    } yield drawing).metered(17.millis)
    input = inputStream(playerInput)
    gameStream = input.concurrently(drawStream).concurrently(gameLogic(gameState, greenLight, playerInput))
    stream = terminalRawMode.flatMap(rawModeResult =>
      if (rawModeResult == 0)
        gameStream
      else
        Stream.eval(IO(println("Failed to switch terminal to raw input mode!")))
    )
    program <- stream.compile.drain
  } yield program
}
