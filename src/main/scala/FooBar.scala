import fs2._
import cats.effect._

import scala.concurrent.duration.DurationInt

object FooBar extends IOApp.Simple {

  private val count: Stream[IO, Unit] = {
    def next(start: Int): Stream[Pure, Int] = Stream(start) ++ next(start + 1)

    next(1)
  }.evalMap(number => IO(print(s"\n$number: ")))

  private val foos: Stream[IO, Unit] = Stream.eval(IO(print("foo"))).repeat

  private val bars: Stream[IO, Unit] = Stream.eval(IO(print("bar"))).repeat

  /**
   * Task 2: Asynchronous Foobar
   *
   * Produce a stream that will print a line once every second. Each line should contain the line number
   * (i.e. in increasing order), and if the line number is divisible by three, print "foo",
   * and if the line number is divisible by five, print "bar".
   * Stop after 15 lines have been printed.
   *
   * The following functions on streams may be helpful to you:
   * .merge(otherStream) – combines the outputs of two streams non-deterministically by running them both asynchronously.
   * .delayBy(duration) – returns a stream, that when pulled from, sleeps for a duration, and then pulls from the original stream.
   * .metered(duration) – returns a stream that sleeps for duration before every pull of the original stream. Effectively a "throttle".
   * .interruptAfter(duration) – returns a stream that terminates after the given duration.
   */
  private val stream = {
    val numbers = count.metered(1.second)
    val foo = foos.metered(3.seconds)
    val bar = bars.metered(5.seconds)
    val foobar = foo merge bar.delayBy(100.millis)

    (numbers merge foobar.delayBy(100.millis)).interruptAfter(15.seconds + 500.millis)
  }

  def run: IO[Unit] = stream.compile.drain
}
