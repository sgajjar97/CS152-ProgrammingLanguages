/****************************************
  Assignment 4, List Processing II
  Name: Shail Gajjar
  ***************************************/


/****************************************
  * Problem 1: score processing
  ***************************************/
var cs152 = List(List(93, 89, 90), List(75, 76, 68), List(88, 82, 78))
// avg
def avg(scores: List[Double]): Double = {  //>avg: avg[](val scores: List[Double]) => Double
  scores.reduce(_+_)/scores.length
}

// avgAvg
def avgAvg(scores: List[List[Double]]): List[Double]={  //>avgAvg: avgAvg[](val scores: List[List[Double]]) => List[Double]
  var avgs = List[Double]()
  for(i <- scores){
    avgs = avg(i)::avgs
  }
  avgs.reverse
}

// passing
def passing(scores: List[List[Double]]): List[Int]={  //>passing: passing[](val scores: List[List[Double]]) => List[Int]
  val avgs = avgAvg(scores)
  var pass = List[Int]()
  for(i <- 0 until avgs.length){
    if(avgs(i) >= 70)
      pass = i::pass
  }
  pass.reverse
}

// sumSums
def sumSums(scores: List[List[Double]]):Double ={   //>sumSums: sumSums[](val scores: List[List[Double]]) => Double
  scores.flatten.reduce(_+_);
}

// testing
avg(List(1,2,3,4,5,6,7,8,9,10))   //>res0: Double = 5.5
avgAvg(List(List(93, 89, 90), List(75, 76, 68), List(88, 82, 78)))  //>res1: List[Double] = List(90.66666666666667, 73.0, 82.66666666666667)
passing(List(List(93, 89, 90), List(20, 89, 40), List(75, 76, 68), List(88, 82, 78), List(70, 49, 30)))   //>res2: List[Int] = List(0, 2, 3)
sumSums(List(List(93, 89, 90), List(75, 76, 68), List(88, 82, 78)))   //>res3: Double = 739.0


/****************************************
  * Problem 2: spellCheck
  ***************************************/
// solution
def spellCheck(doc: List[String], dictionary: List[String]): List[String] =   //>spellCheck: spellCheck[](val doc: List[String],val dictionary: List[String]) => List[String]

  if (doc == Nil) Nil
  else if (!dictionary.contains(doc.head)) doc.head::spellCheck(doc.tail, dictionary)
  else spellCheck(doc.tail, dictionary)

// testing
val dic = List("hello", "I", "am", "having", "fun")   //>dic: List[String] = List(hello, I, am, having, fun)
val words1 = List("I","fun","am")    //>words1: List[String] = List(I, fun, am)
spellCheck(dic, words1)   //>res4: List[String] = List(hello, having)

/****************************************
  * Problem 3: spellCheck using map, filter, etc.
  ***************************************/
// solution
def spellCheckMFR(words: List[String], dictionary: List[String]): List[String]={  //>spellCheckMFR: spellCheckMFR[](val words: List[String],val dictionary: List[String]) => List[String]
  words.filterNot((i:String)=>dictionary.contains(i))
}

// testing
val dictionary = List("how", "are", "you", "doing", "today")  //>dictionary: List[String] = List(how, are, you, doing, today)
val words2 = List("are","how","doing","you")  //>words2: List[String] = List(are, how, doing, you)
spellCheck(words2,dictionary)   //>res5: List[String] = List()


/****************************************
  * Problem 4: polynomials
  ***************************************/
// evalMono
def evalMono(mono: (Double, Double), x: Double): Double = {   //>evalMono: evalMono[](val mono: (Double, Double),val x: Double) => Double
  var result = 0.0
  result = mono._1*math.pow(x, mono._2)
  result
}

// evalPoly
def evalPoly(poly: List[(Double, Double)], x: Double): Double = {   //>evalPoly: evalPoly[](val poly: List[(Double, Double)],val x: Double) => Double
  var result = 0.0
  for(mono <- poly){
    result += evalMono(mono,x)
  }
  result
}

// testing
var polynomial = List((3.0, 2.0), (-5.0, 0.0))    //>polynomial: List[(Double, Double)] = List((3.0,2.0), (-5.0,0.0))
evalPoly(polynomial, 3.0)   //>res6: Double = 22.0