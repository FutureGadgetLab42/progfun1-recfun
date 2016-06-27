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
  def pascal(c: Int, r: Int): Int =
    if(c < 0 || r < 0) throw new IllegalArgumentException("Illegal index. r = " + r.toString + " c = " + c.toString)
    else if(c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceAux(charList: List[Char], count: Int): Boolean =
      if (count < 0) false
      else charList match {
        case '(' :: t => balanceAux(t, count + 1)
        case ')' :: t => balanceAux(t, count - 1)
        case _ :: t => balanceAux(t, count)
        case Nil => count == 0
      }
    balanceAux(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countChangeAux(money: Int, coins: List[Int], index: Int): Int = {
      if (money < 0 || index == coins.length) 0
      else if (money == 0) 1
      else countChangeAux(money, coins, index + 1) + countChangeAux(money - coins.apply(index), coins, index)
    }
    if (money == 0) 0
    else countChangeAux(money, coins, 0)
  }

}
