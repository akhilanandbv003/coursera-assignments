package recfun

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
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def balance1(data: List[Char], count: Int): Boolean = {
      if (data.isEmpty && count == 0) true

      else if (data.isEmpty || count < 0) false
      else {
        data match {
          case '(' :: tail => balance1(tail, count + 1)
          case ')' :: tail => balance1(tail, count - 1)
          case _ => balance1(data.tail, count)
        }
      }
    }
    balance1(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {

    def loop(combinations:Int, denominations:List[Int]): Int ={
      if(denominations.isEmpty ) 0

      else if (combinations == money) 1

      else if (combinations < money ) {
        loop(combinations + denominations.head, denominations) + loop(combinations, denominations.tail)
      }

      else 0

    }
    loop(0, coins)
    if (coins.isEmpty) 0 //no coins

    else {
      loop(0, coins)
    }
  }

}
