package misc

import fs2._

// Just a simple example on how to use pulls.
// This program transforms an integer stream into only the values that are new maximums or minimums
object PullExample extends App {

  val numbers = Stream(1, 2, 5, 4, -4, -3, 8, 5, -7, -6, -9)

  def process[F[_]](currentMin: Int, currentMax: Int, stream: Stream[F, Int]): Pull[F, Int, Unit] =
    stream.pull.uncons1.flatMap {
      case Some((head, tail)) =>
        if (head > currentMax || head < currentMin)
          Pull.output1(head) >> process(currentMin.min(head), currentMax.max(head), tail)
        else process(currentMin, currentMax, tail)
      case None               => Pull.done
    }

  val pull = process(0, 0, numbers)

  pull.stream.compile.toVector.foreach(println)
}
