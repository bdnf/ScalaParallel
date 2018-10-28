def countChange(money: Int, coins: List[Int]): Int =
  if (money < 0)
    0
  else if (coins.isEmpty)
    if (money == 0) 1 else 0
  else {
      println("money is: "+ money + " coins.tail "+ coins.tail + " = " + countChange(money, coins.tail) + " + "
        + " (money - coins.head) " +(money - coins.head) + " coins " + coins + " = " + countChange(money - coins.head, coins))
      countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }


countChange(4, List(2,4))

