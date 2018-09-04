
object sequenceControl {

  //Problem 1
  def tax(income: Double): Double = {   //> tax: tax[](val income: Double) => Double
    if (income < 0) {
      println("Income cannot be negative\nPlease enter a valid income!")
      return 0.0
    }
    else if(income < 20000) {
      return 0.0
    }
    else if(income < 30000) {
      return (5.0/100)*income
    }
    else if(income < 40000) {
      return (11/100)*income
    }
    else if(income < 60000) {
      return (23.0/100)*income
    }
    else if(income < 100000) {
      return (32.0/100)*income
    }
    else {
      return (50.0/100)*income
    }
  }

  tax(12300)    //> res0: Double = 0.0
  tax(29000)    //> res1: Double = 1450.0
  tax(125000)   //> res2: Double = 62500.0
  tax(1000000)  //> res3: Double = 500000.0
  tax(-1000000) //> Income cannot be negative, Please enter a valid income!


  //Problem 2
  def drawRectangle(n: Int, m: Int) = {
    var x = ""
    for (i <- 0 to (n-1)) {
      for (k <- 1 until ((n+1)*(m+1))/4) {
        x += "*"
      }
      x +="\n"
    }
    x
  }

  println(drawRectangle(3,4))
//>  ****
//>  ****
//>  ****

  //Problem 3
  def printSums(a: Int, b: Int) = {
    for(i <- 0 to a - 1;
        j <- 0 to b - 1)
    {
      println(i + " + " + j + " = " + (i + j))
    }
  }

  printSums(3, 4)
//  0 + 0 = 0
//  0 + 1 = 1
//  0 + 2 = 2
//  0 + 3 = 3
//  1 + 0 = 1
//  1 + 1 = 2
//  1 + 2 = 3
//  1 + 3 = 4
//  2 + 0 = 2
//  2 + 1 = 3
//  2 + 2 = 4
//  2 + 3 = 5


  //Problem 4
  def mystery(): Unit = {   //> mystery: mystery[]() => Unit
    var i = 0
    while (i < 100 && i != 10) {
      if (i % 3 != 0) {
        println("i = " + i)
      }
      i += 1
    }
    println("Done")
  }

  mystery()
//  i = 1
//  i = 2
//  i = 4
//  i = 5
//  i = 7
//  i = 8
//  Done


  //Problem 5
  def root(x: Double): Option[Double] = if (x < 0) None else Some(math.sqrt(x))

  def below10(x: Double): Option[Double] = if (x < 10) Some(x) else None

  def pureRoot(x: Option[Double]): Option[Double] = {
    if (x.isEmpty)
      None
    else {
      root(x.get) match {
        case None => None
        case Some(y) => Some(y)
      }
    }
  }

  def pureBelow10(x: Option[Double]): Option[Double] = {
    if (x.isEmpty)
      None
    else {
      below10(x.get) match {
        case None => None
        case Some(y) => Some(y)
      }
    }
  }

  def below10root(x: Option[Double]): Option[Double] = {
    var result: Option[Double] = None
    pureBelow10(x) match {
      case None => None
      case Some(y) => result = Some(y)
    }
    if(result.isDefined)
      pureRoot(x) match {
        case None => result = None
        case Some(y) => result = Some(y)
      }
    result
  }

  pureRoot(None)              //> Option[Double] = None
  pureRoot(Some(64))          //> Option[Double] = Some(8.0)
  pureRoot(Some(-64))         //> Option[Double] = None

  pureBelow10(Some(100))      //> Option[Double] = None
  pureBelow10(Some(-100))     //> Option[Double] = Some(-100.0)
  pureBelow10(None)           //> Option[Double] = None

  below10root(Some(100))      //> Option[Double] = None
  below10root(Some(-100))     //> Option[Double] = None
  below10root(Some(9))        //> Option[Double] = Some(3.0)
  below10root(None)           //> Option[Double] = None


  //Problem 6
  var x = 10        //> x: Int = 10
  lazy val y = math.log(-1)       //> y: Double = NaN

  if (false) 3    //> res18: AnyVal = ()

  if (false) if (true) 3 else 4     // dangling else

  //if (x = 10) true else false  //error: type mismatch

  if (true) 1 else 1/0    //res20: Int = 1        // conditional eval

  if (if (true) false else true) true else false  //false

  for(i <- 1 to 5) yield i * i  //scala.collection.immutable.IndexedSeq[Int] = Vector(1, 4, 9, 16, 25)

  y + 10   //Double = NaN

  println("100") // tricky!   //100

  true || 3 == 1/0            //true      // short circuit eval

  false && math.log(-1) > 0  //false       // short circuit eval


  //Problem 7
  var x = 10

  {1; 2; 3} + {4; 5; 6}   //warning --> Int = 9

  {1; 2; {3; 4; {5; 6} } }   //warning --> Int = 6

  if (false) {2; 3} else {4; {5; 6}} + {10; 20} // warning --> Int = 26

  {10; 20} + if (false) {2; 3} else {4; {5; 6}}  //+ error

  {var x = 20; var y = x + 1; x + y}   //41

  { var y = x + 1; var x = 5; var z = x + 1; x + y + z}  //error

  {var x = 3; var y = {var x = 9; x + 1}; x + y} // shadowning   //13

  {def tri(n: Int): Int = if (n == 0) 0 else n + tri(n - 1); tri(5) }  //15
  //recursive blocks

}