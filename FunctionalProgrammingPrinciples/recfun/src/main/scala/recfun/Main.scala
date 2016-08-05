package recfun
import common._

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
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || r == c) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def loop(chars: List[Char], total: Int): Boolean =
      if (total < 0 && !chars.isEmpty) false
      else if (chars.isEmpty && (total == 0) ) true
      else if (chars.isEmpty) false
      else if (chars.head == '(') loop(chars.tail, total + 1)
      else if (chars.head == ')') loop(chars.tail, total - 1)
      else loop(chars.tail, total)

    loop(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    
    def loop(coins: List[Int], accum: Int): Int = 
      if (coins.isEmpty) 0
      else if (accum == money) 1
      else if (accum > money) 0
      else loop(coins, accum+coins.head) + loop(coins.tail, accum)
      
      
    loop(coins, 0)
  }
}
