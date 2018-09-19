
object funtionalProgramming {

  //Problem 1
  def compose[A, B, C](f: B => C, g: A => B): A => C = {    //>compose: compose[A,B,C](val f: B => C,val g: A => B) => A => C
    def r(x: A) = f(g(x))
    r _
  }
  def len (s: String) = s.length      //>len: len[](val s: String) => Int
  def sqrt(x: Int) = math.sqrt(x)  //>sqrt: sqrt[](val x: Int) => Double
  var f = compose(sqrt _, len _)          //>f: String => Double = $Lambda$1260/2062335729@17054aed

  f("8938")     //>res0: Double = 2.0

  //Problem 2
  def inc(x: Double) = x + 1      //>inc: inc[](val x: Double) => Double
  def double(x: Double) = 2 * x   //>double: double[](val x: Double) => Double
  def selfIter[T](f: T => T, n: Int): T => T = {    //>selfIter: selfIter[T](val f: T => T,val n: Int) => T => T
    def rec(x: T, count: Int): T = {
      if (count == 0)
        x
      else
        rec(f(x), count-1)
    }
    rec( _, n)
  }

  var a = selfIter(inc, 7)      //>a: Double => Double = $Lambda$1262/1543095217@5a7b01d5
  var b = selfIter(double, 20)  //>b: Double => Double = $Lambda$1262/1543095217@56b6950c
  a(7)      //>res1: Double = 14.0
  b(20)     //>res2: Double = 2.097152E7

  //Problem 3
  def isEven(x: Int)={    //>isEven: isEven[](val x: Int) => Boolean
    if (x%2 == 0)
      true
    else
      false
  }

  def countPass[T]( nums: Array[T], testNums: T => Boolean): Int={  //>countPass: countPass[T](val nums: Array[T],val testNums: T => Boolean) => Int
    var res = 0
    for (i <-nums)
    {
      if (testNums(i))
        res = res+1
    }
    res
  }
  val nums = Array(13, 12, 8)   //>nums: Array[Int] = Array(13, 12, 8)
  countPass(nums, isEven)     //>res3: Int = 2

  //Problem 4
  def recur(baseVal: Int, combiner: (Int, Int) => Int) = {    //>recur: recur[](val baseVal: Int,val combiner: (Int, Int) => Int) => Int => Int
    def r(n: Int): Int = {
      if (n == 0) {
        baseVal
      } else {
        combiner(n,r(n-1))
      }
    }
    r _
  }

  val factorial = recur(1, _ * _)   //>factorial: Int => Int = $Lambda$1379/1333022813@6f3dea7

  factorial(2)    //>res4: Int = 2
  factorial(3)    //>res5: Int = 6

  //Problem 5
  def deOptionize[T, S](n: T => Option[S]): T => S = {    //>deOptionize: deOptionize[T,S](val n: T => Option[S]) => T => S
    def r(t: T) = {
      n(t) match {
        case None => throw new Exception("failed")
        case Some(s) => s
      }
    }
    r _
  }

  def parseDigits(digits: String): Option[Int] =    //>parseDigits: parseDigits[](val digits: String) => Option[Int]
    if (digits.matches("[0-9]*")) Some(digits.toInt) else None

  val pDigits = deOptionize(parseDigits _)    //>pDigits: String => Int = $Lambda$1381/1280678637@60bdabef
  pDigits("12345")    //>res6: Int = 12345

  try{
    pDigits("1234x5")   //>java.lang.Exception: failed
  } catch {
    case e: Exception => println(e)   //>res7: AnyVal = ()
  }
}