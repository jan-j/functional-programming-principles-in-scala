package recfun

import common._

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println(pascal(15, 40))

    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c < 0 || r < 0 || c > r) throw new IllegalArgumentException

    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def charValue(char: Char): Int = {
      if (char == '(') 1
      else if (char == ')') -1
      else 0
    }

    def count(counter: Int, chars: List[Char]): Int = {
      if (chars.isEmpty || counter == -1) counter
      else count(counter + charValue(chars.head), chars.tail)
    }

    count(0, chars) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def loop(acc: Int, money: Int, coins: List[Int]): Int = coins match {
      case Nil => acc
      case coin :: tail =>
        if (money < 0) acc
        else if (money == 0) acc + 1
        else loop(acc + countChange(money, tail), money - coin, coins)
    }

    loop(0, money, coins)
  }
}
