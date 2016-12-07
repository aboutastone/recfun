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
      if (c==r||c==0) 1
      else pascal(c-1,r-1) + pascal(c,r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def processNextChar(chars: List[Char], level:Int): Boolean = {
        if (chars.isEmpty) level == 0;
        else if (level < 0)  false
        else if (chars.head == '(')  processNextChar(chars.tail,level+1)
        else if (chars.head == ')')  processNextChar(chars.tail,level-1)
        else false
      }

      processNextChar(chars,0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {

      def getPossibleCombos(money: Int, coins: List[Int]):Int = {
        var numSolutions = 0
        var coinSet = coins
        for (coin <- coins) {
          if (money > coin) {
            numSolutions += getPossibleCombos(money - coin,coinSet)
          } else if (money == coin) {
            numSolutions += 1
          }
          coinSet = coinSet.tail
        }
        numSolutions
      }

      getPossibleCombos(money,coins)
    }
  }
