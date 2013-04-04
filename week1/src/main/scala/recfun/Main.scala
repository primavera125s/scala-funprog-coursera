package recfun
import collection.immutable

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
  def pascal(c: Int, r: Int): Int = {
    def innerPascal(c: Int, r: Int): Int = {
      val ret = if (c < 0 || c > r) {
        0
      } else if (c == 0 || c == r) {
        1
      } else {
        innerPascal(c - 1, r - 1) + innerPascal(c, r - 1)
      }
      ret
    }
    innerPascal(c, r)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def innerBalance(chars: List[Char], const: Int): Boolean = {
      if (chars.isEmpty) {
        (const == 0)
      } else if (const > 0) {
        false
      } else {
        val value =  if (chars.head.toString == "(") {
          1
        } else if (chars.head.toString == ")") {
          -1
        } else {
          0
        }
        innerBalance(chars.tail, const - value)
      }
    }
    innerBalance(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def innerCountChange(money: Int, coins: List[Int]):Int = {
      if (money == 0) {
        1
      } else if (money < 0) {
        0
      } else if (coins.isEmpty) {
        0
      } else {
        val someWays = innerCountChange(money, coins.slice(0, coins.length - 1))
        val moreWays = if (money >= coins.last) {
          innerCountChange(money - coins.last, coins)
        } else {
          0
        }
        someWays + moreWays
      }
    }
    innerCountChange(money, coins.sorted)
  }
}
