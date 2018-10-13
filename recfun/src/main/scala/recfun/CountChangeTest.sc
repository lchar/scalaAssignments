object CountChangeTest {
  def countChange(money: Int, coins: List[Int]): Int = {
    def countChange2(combs: Int, tot: Int, moneyleft: Int, coins: List[Int]): Int ={
      def loop1(combs: Int, tot: Int, moneyleft: Int, coins: List[Int]): Int = {
        if (coins.isEmpty) combs
        else if (moneyleft - coins.head == 0) loop1(combs + 1, tot, moneyleft, coins.tail)
        else if (moneyleft - coins.head < 0) loop1(combs, tot, moneyleft, coins.tail)
        else loop1(countChange2(combs, tot, moneyleft - coins.head, coins), tot, moneyleft, coins.tail)
      }
      loop1(combs, tot, moneyleft, coins)
    }
    if (money == 0) 1
    else countChange2(0, 0, money, coins)

  }
  countChange(4, List(1,2))
  countChange(300,List(5,10,20,50,100,200,500))
}