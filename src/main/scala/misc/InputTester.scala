package misc

import scala.sys.process._

// Program to test if your terminal supports raw input mode.
object InputTester extends App {
  var loop = true
  Seq("sh", "-c", "stty raw </dev/tty").!
  while (loop) {
    val c = Console.in.read().toChar
    if (c == 'q') loop = false
    print(s"\u001B[100DYou are pressing $c")
    Thread.sleep(10L)
  }
  Seq("sh", "-c", "stty cooked </dev/tty").!
}