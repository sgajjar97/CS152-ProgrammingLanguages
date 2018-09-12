
import scala.annotation.tailrec

object Recursion {

  def inc(n: Int) = n + 1           //> inc: inc[](val n: Int) => Int
  def dec(n: Int) = n - 1           //> dec: dec[](val n: Int) => Int
  def isZero(n: Int) = n == 0   //> isZero: isZero[](val n: Int) => Boolean

  //Problem 1
  def add(n: Int, m: Int): Int = {    //> add: add[](val n: Int,val m: Int) => Int
    @tailrec
    def helper(count: Int, result: Int): Int =
      if (isZero(count)) result else helper(dec(count), inc(result))
    helper(n, m)
  }
  add(3,4)    //> res0: Int = 7

  //Problem 2
  def mul(n: Int, m: Int): Int = {    //> mul: mul[](val n: Int,val m: Int) => Int
    var result = 0
    if (m < 1){
      return 0
    } else {
      result = add(mul(n, dec(m)), n)
    }
    return result
  }

  mul(5,6)      //> res1: Int = 30
  mul(-3,4)     //> res2: Int = -12

  //Problem 3
  def exp2(n: Int): Int = {   //> exp2: exp2[](val n: Int) => Int
    var result = 0
    if (n < 1) {
      return 1
    } else{
      result = mul(exp2(dec(n)), 2)
    }
    return result
  }

  exp2(5)   //> res3: Int = 32
  exp2(2)   //> res4: Int = 4

  //Problem 4
  def hyperExp(n: Int): Int = {   //> hyperExp: hyperExp[](val n: Int) => Int
    var result = 1
    if (n < 1){
      return 2
    } else {
      result = exp2(hyperExp(dec(n)))
    }
    return result
  }

  hyperExp(2)   //> res5: Int = 16
  hyperExp(3)   //> res6: Int = 65536

  //Problem 5
  //> yes it does improve the stack overflow problem
  //> computation time must have improved little bit
  //> Not really noticeable since we are dealing with small computation



  //Problem 9
  // recursive solution
  def fibonacciRec(n: Int): Int = {   //> fibonacciRec: fibonacciRec[](val n: Int) => Int
    if (n <= 2){
      return 1
    }
    else {
      fibonacciRec(n-1) + fibonacciRec(n-2)
    }
  }
  fibonacciRec(3)   //> res7: Int = 2

  // tail recursive
  def fibonacciTail(n: Int): Int={    //> fibonacciTail: fibonacciTail[](val n: Int) => Int
    def helper(a:Int, b: Int, c : Int, res: Int): Int ={
      if (c > n){
        res
      }
      else{
        helper(a, res+b, a+b, inc(c))
      }
    }
    helper(0, 0,1,0)
  }
  fibonacciTail(3)    //> res8: Int = 2

  //Problem 10
  def choose(n: Int, m: Int): Int = {   //> choose: choose[](val n: Int,val m: Int) => Int
    if(n < m) {0}
    else if(m == 0 || n == m) {1}
    else{
      choose(n-1, m-1) + choose(n-1, m)
    }
  }
  choose(8, 2)    //> res9: Int = 28
  choose(10, 5)   //> res10: Int = 252
}

























