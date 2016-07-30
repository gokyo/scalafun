package recfun
import common._
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

  /**
   * Exercise 1
   */
  // This function is not tail-recursive. Would be nice to make it 
  // tail-recursive, in order to have a better performance!
  def pascal(c: Int, r: Int): Int = {
    if (c < 0 || r < 0)
      0
    else if (c == 0 || c == r)
      1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  // tail-recursive version: bugged! it is not working
  /*
  def pascal(c: Int, r: Int): Int = {

    @tailrec
    def computePascal(c: Int, r: Int, acc: Int): Int = {
      if (r == 0 || c == r)
        acc
      else
        computePascal(c - 1, r - 1, (c / r) * acc)
    }

    if (c < r || r < 0)
      0
    else
      computePascal(c, r, 1)
  }
  */

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    @tailrec
    def checkBalance(chars: List[Char], currentBalance: Int): Boolean = {

      def computeNewBalance: Int = {
        val head = chars.head;
        if (head == '(')
          currentBalance + 1;
        else if (head == ')')
          currentBalance - 1;
        else
          currentBalance
      }

      if (currentBalance < 0)
        false
      else if (chars.isEmpty)
        currentBalance == 0
      else
        checkBalance(chars.tail, computeNewBalance)
    }

    checkBalance(chars, 0)
  }

  /**
   * Exercise 3
   */
  // This function is not tail-recursive. Would be nice to make it 
  // tail-recursive, in order to have a better performance!
  def countChange(money: Int, coins: List[Int]): Int = {

    if (money == 0)
      1
    else if (money < 0 || coins.isEmpty)
      0
    else
      countChange(money - coins.head, coins) +
        countChange(money, coins.tail)
  }
}
