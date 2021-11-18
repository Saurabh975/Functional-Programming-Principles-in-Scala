package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (r == 0 || c == 0 || r == c) return 1
    else if (c > r) return 0
    pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def remainingBalance(chars: List[Char], count: Int): Boolean = {
      if (chars.isEmpty) return count == 0
      if (count < 0) return false
      if (chars.head == '(') return remainingBalance (chars.tail, count + 1)
      else if (chars.head == ')') return remainingBalance (chars.tail, count - 1)
      remainingBalance(chars.tail, count)
    }
    remainingBalance(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countMoney(money: Int, coins: List[Int]): Int = {
      if (money < 0 || coins.isEmpty) return 0
      if (money == 0) return 1
      (countMoney (money - coins.head, coins) + countMoney (money, coins.tail))
    }
    if (money <= 0) return 0
    countMoney(money, coins.sorted.reverse)
  }
