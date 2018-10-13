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
      if ((c == 0) || c == r) 1
      else pascal(c, r - 1) + pascal(c - 1, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def loop(count: Int, chars: List[Char]): Boolean = {
        if (count < 0) false
        else if (chars.isEmpty && count == 0) true
        else if (chars.isEmpty && count > 0) false
        else if (chars.head == '(') loop(count + 1, chars.tail)
        else if (chars.head == ')') loop(count - 1, chars.tail)
        else loop(count, chars.tail)
      }
      loop(0, chars)
    }
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def countChange2(combs: Int, moneyleft: Int, coins: List[Int]): Int ={
        def loop1(combs: Int, moneyleft: Int, coins: List[Int]): Int = {
          if (coins.isEmpty) combs
          else if (moneyleft - coins.head == 0) loop1(combs + 1, moneyleft, coins.tail)
          else if (moneyleft - coins.head < 0) loop1(combs, moneyleft, coins.tail)
          else loop1(countChange2(combs, moneyleft - coins.head, coins), moneyleft, coins.tail)
        }
        loop1(combs, moneyleft, coins)
      }
      if (money == 0) 1
      else if (coins == List()) 0
      else countChange2(0, money, coins)

    }
  }
