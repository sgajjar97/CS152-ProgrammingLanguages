object Math {

  //Problem 1
  def solve(a: Double, b: Double, c: Double) = {  //> solve: solve[](val a: Double,val b: Double,val c: Double) => Any
    val disc = b * b - 4 * a * c
    if (disc < 0) {
      None
    }
    else if (disc == 0) {
      -b/(2*a)
    }
    else {
      val root = math.sqrt(disc)
      val s1 = (-b + root)/(2*a)
      val s2 = (-b - root)/(2*a)
      (s1, s2)
    }
  }

  solve(2, -2, -4)      //> res0: Any = (2.0,-1.0)
  solve(1, 0, 1)        //> res1: Any = None
  solve(1, 0, -1)       //> res2: Any = (1.0,-1.0)

  //Problem 2
  def distance(x: (Double, Double), y: (Double, Double)) = {  //> distance: distance[](val x: (Double, Double),val y: (Double, Double)) => Double
    var result = 0.0
    result = math.sqrt(math.pow(y._1 - x._1, 2) + math.pow(y._2 - x._2, 2))
    result
  }

  distance((1, 1), (0,0))     //> res3: Double = 1.4142135623730951
  distance((3, 0), (0,0))     //> res4: Double = 3.0

  //Problem 3
  def dotProduct(x: (Double, Double, Double), y: (Double, Double, Double)) = {   //> dotProduct: dotProduct[](val x: (Double, Double, Double),val y: (Double, Double, Double)) => Double
    var result = 0.0
    result = (x._1 * y._1) + (x._2 * y._2) + (x._3 * y._2)
    result
  }

  dotProduct((2.0, 3, 4), (2, 2.0, 2))    //> res5: Double = 18.0

  //Problem 4
  def force(m1: Double, m2: Double, d: Double) = {  //> force: force[](val m1: Double,val m2: Double,val d: Double) => Double
    var result = 0.0
    result = ((6.6726 * math.pow(10, -11)) * m1 * m2) / math.pow(d , 2)
    result
  }

  force(10, 2, 4)      //> res6: Double = 8.34075E-11

  //Problem 5
  def mean(x: Array[Double]) = {    //> mean: mean[](val x: Array[Double]) => Double
    var result = 0.0
    val one = x.length
    for (i <- 0 until one) {
      result += x(i)
    }
    result = result / one
    result
  }

  mean(Array(2.0, 3, 4, 5))     //> res7: Double = 3.5


  def stdDev(x: Array[Double]) = {    //> stdDev: stdDev[](val x: Array[Double]) => Double
    var result = 0.0
    val one = x.length
    val a: Array[Double] = new Array[Double](one)
    val m = mean(x)
    var diff = 0.0

    for (i <- 0 until(one)) {
      a(i) = math.pow(m-x(i), 2)
    }
    diff = mean(a)
    result = math.sqrt(diff)
    result
  }

  stdDev(Array(2, 3.0, 4, 5))     //> res8: Double = 1.118033988749895

  //Problem 6
  def isPrime(n: Integer): Boolean = {    //> isPrime: isPrime[](val n: Integer) => Boolean
    if (n <= 1)
      false
    else if (n == 2)
      true
    else
      !(2 to (n-1)).exists(x => n % x == 0)
  }

  isPrime(0)      //> res9: Boolean = false
  isPrime(1)      //> res10: Boolean = false
  isPrime(2)      //> res11: Boolean = true
  isPrime(8)      //> res12: Boolean = false
  isPrime(12)     //> res13: Boolean = false
  isPrime(13)     //> res14: Boolean = true

  //Problem 7
  def phi(n: Integer): Integer = {    // >phi: phi[](val n: Integer) => Integer
    var result = 1
    if (n < 2) {
      return 1
    }
    for (i <- 2 until n)
      if (gcd(i,n)) {
        result = result + 1
      }
    result
  }

  def gcd(a: Integer, b: Integer): Boolean = {    //> gcd: gcd[](val a: Integer,val b: Integer) => Boolean
    val result = true
    for(i <- 2 to a){
      if((a%i==0) && (b%i==0)){
        return false
      }
    }
    result
  }

  phi(9)      //> res15: Integer = 6
  phi(10)     //> res16: Integer = 4

  //Problem 8

  def rollDice = {    //> rollDice: rollDice[] => (Int, Int)
    val random = new scala.util.Random()
    var x = random.nextInt(6)
    var y = random.nextInt(6)
    x = x + 1
    y = y + 1
    val z = (x,y)
    z
  }

  rollDice    //> res17: (Int, Int) = (1,6)
  rollDice    //> res18: (Int, Int) = (4,2)
  rollDice    //> res19: (Int, Int) = (5,3)
  rollDice    //> res20: (Int, Int) = (3,1)
  rollDice    //> res21: (Int, Int) = (4,4)
  rollDice    //> res22: (Int, Int) = (4,4)

}