import fs2._
import cats.effect.{IO, IOApp}
import fs2.io.file.{Files, Path}

object Chat extends IOApp.Simple {

  case class Message(timestamp: Int, person: String, text: String) {
    override def toString: String = s"$person: $text"
  }

  object Message {
    def fromLine(person: String)(line: String): Option[Message] = {
      val (timestamp, text) = line.span(_ != ' ')
      Option.when(timestamp.nonEmpty && text.nonEmpty)(timestamp.toIntOption.map(Message(_, person, text)))
        .flatten
    }
  }

  // Hack because I believe this exercise triggers a FS2 bug when doing pulls after a file stream has been exhausted
  def preCompute[A](input: Stream[IO, A]): Stream[IO, A] = Stream.eval(input.compile.toList)
    .flatMap(Stream.emits)

  val captainFileStream: Stream[IO, Message] = Files[IO].readAll(Path("assets/captain.txt"))
    .through(text.utf8.decode)
    .through(text.lines)
    .map(Message.fromLine("Captain"))
    .collect { case Some(msg) => msg }
    .through(preCompute)

  val doctorFileStream: Stream[IO, Message] = Files[IO].readAll(Path("assets/doctor.txt"))
    .through(text.utf8.decode)
    .through(text.lines)
    .map(Message.fromLine("Doctor"))
    .collect { case Some(msg) => msg }
    .through(preCompute)

  /**
    * Task 4: Chat history
    *
    * In this task you will be learning how to statefully transform streams.
    *
    * Imagine you have a chatting server where messages are stored in a log file belonging to each user.
    * In this example, we have two users: captain and doctor. The provided streams open the respective
    * files in the project's assets directory, and read them line by line, and close them when the streams
    * are terminated. (Don't worry, this has all been done for you.)
    *
    * Each line of these files contains a timestamp for the message, and the message content.
    * Your task is to merge these two streams into a single output stream, where the messages are sorted
    * in ascending order. The input streams are already sorted, so think of this as the merge step in merge-sort.
    *
    * Because these files might be huge in the real world (too large to fit in memory!) we are merging them
    * using streams.
    *
    */
  override def run: IO[Unit] = {

    val firstPull: Pull[IO, Message, Unit] = ???

    firstPull.stream.evalMap(msg => IO(println(msg))).compile.drain
  }


}