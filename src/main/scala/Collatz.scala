import cats.effect.{IO, IOApp}
import fs2._

object Collatz extends App {

  val announcement = Stream("hip", "hip") ++ Stream("hooray!")
  announcement.toList.foreach(println)

  println

  /**
    * Task 1: The Collatz conjecture
    *
    * Return a sequence of integers, starting from the provided one, according to the following law:
    * If the previous number X is even, then the next number returned will be X / 2
    * If the previous number X is odd, then the next number returned will be 3X + 1
    *
    * The Collatz conjecture states that every such sequence of integers (starting from a positive one), will
    * end up in a loop 4 -> 2 -> 1 -> 4. However since no one has proved this, we use streams for safety!
    */
  def collatz(start: Int): Stream[Pure, Int] = {
    val next = if (start % 2 == 0) start / 2 else start * 3 + 1
    Stream(start) ++ collatz(next)
  }


  collatz(27).takeThrough(_ != 1).toList.foreach(println)

  println

  /**
    * Bonus task! Intersperse the collatz number stream with string comments stating if the next number
    * will grow (be greater in value) or shrink (be smaller in value).
    *
    * Example: Stream(2, 1, 4) should become Stream("2", "Shrinking!", "1", "Growing!", "4", "Shrinking!")
    * Tip: Use flatMap!
    */
  def bonus(stream: Stream[Pure, Int]): Stream[Pure, String] = {
    stream.flatMap(number => Stream(
      number.toString,
      if (number % 2 == 0) "Shrinking!" else "Growing!"
    ))
  }

  bonus(collatz(5).takeThrough(_ != 1)).toList.foreach(println)
}
