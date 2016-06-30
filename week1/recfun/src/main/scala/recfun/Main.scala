package recfun

import java.util

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /*  1
     1 1
    1 2 1
   1 3 3 1
  1 4 6 4 1*/

  /*  1
      1 1
      1 2 1
      1 3 3 1
      1 4 6 4 1*/

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if(c == 0 && r == 0) 1
    else if (c < 0 || r < 0) 0
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  @tailrec
  def balance(chars: List[Char]): Boolean = {
    if (chars.isEmpty) true
    else if (chars.head == '(') {
      val lastInd: Int = chars.lastIndexOf(')')
      if (lastInd == -1) false
      else balance(chars.tail.dropRight(lastInd - 1))
    } else if (chars.head == ')') false
    else balance(chars.tail)
  }

  val moneys = new util.LinkedList[Int]
  /**
    * Exercise 3
    */

  def countChange(money: Int, coins: List[Int]): Int = {
    val auxCoins = coins.sortWith((a, b) => a < b)

    if (auxCoins.isEmpty || auxCoins.min > money) 0
    else if (money == 0) 0
    else {
      var count = 0
      for (coin <- auxCoins) {
        val times = money / coin

        for (i <- 1 until (times + 1)) {
          val tail = money - i * coin
          if (tail == 0) {
            count += 1
          } else {
            val count2 = countChange(tail, auxCoins.drop(auxCoins.indexOf(coin) + 1))
            if (count2 != 0) {
              count += count2
            }
          }
        }
      }

      count
    }
  }
}
