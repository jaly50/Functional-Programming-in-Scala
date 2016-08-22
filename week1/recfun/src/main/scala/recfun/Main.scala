package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    println("Balance")
    val st = "())("
    println(balance(st.toList))

    println("count")
    println(countChange(100, List(1, 100, 10, 25, 50)))
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (c==0 || r==0 || c==r) 1
    else  pascal(c-1, r-1) + pascal(c, r-1)
  }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
    def balanceInside(chars: List[Char], leftNum: Int) : Boolean = {
      if (leftNum <0) false
      else if (chars.isEmpty) leftNum==0
      else if (chars.head == '(') balanceInside(chars.tail, leftNum+1)
      else if (chars.head == ')') balanceInside(chars.tail, leftNum -1)
      else balanceInside(chars.tail, leftNum)

    }
    balanceInside(chars, 0)
  }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
        if (coins.isEmpty) 0
         else if (money==coins.head) 1+countChange(money, coins.tail)
        else if (money < coins.head) countChange(money, coins.tail)
        else countChange(money-coins.head, coins) + countChange(money, coins.tail)
  }
  }
