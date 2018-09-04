import scala.util.{Random, Sorting}

object stringProcessing {

  //Problem 1
  def isPal(aString: String) = {    //> isPal: isPal[](val aString: String) => Boolean
    var result = false;
    val inputLowerCase = aString.trim.toLowerCase
    if (inputLowerCase == inputLowerCase.reverse) {
      result = true
    }
    result
  }

  isPal("rotator")      //> res0: Boolean = true
  isPal("cat")          //> res1: Boolean = false
  isPal("Toyota")       //> res2: Boolean = false
  isPal("$3.1441.3$")   //> res3: Boolean = true

  //Problem 2
  def single(aString: Char) =   //> single: single[](val aString: Char) => Boolean
    'A' <= aString && aString <= 'Z' || 'a' <= aString && aString <= 'z'

  def isPal2(input: String) = {   //> isPal2: isPal2[](val input: String) => Boolean
    isPal(input.filter(single))
  }

  isPal2("A man, a plan, a canal, Panama!")   //> res4: Boolean = true

  //Problem 3
  def mkPal(input: String) = {    //> mkPal: mkPal[](val input: String) => String
    var result = input
    result = result + input.reverse
    result
  }

  mkPal("mars")   //> res5: String = marssram
  mkPal("3X@#")   //> res6: String = 3X@##@X3

  //Problem 4
  def mkWord(inputLength: Int = 11) = {   //> mkWord: mkWord[](val inputLength: Int = 11) => String
    var result = ""
    val random = new Random()
    for (i <- 0 until inputLength){
      result += (random.nextInt(25)+97).toChar
    }
    result
  }

  val x1 = mkWord()     //> x1: String = wjcpseoephn
  val x2 = mkWord()     //> x2: String = ynwjryrnfsx
  val x3 = mkWord()     //> x3: String = wcyjyhfxkpr
  val x4 = mkWord()     //> x4: String = camjsqiknqn

  //Problem 5
  def mkSentence(a: Int = 10) = {
    var result = ""
    val random = new Random()
    for(i <- 0 until a){
      val c = (random.nextInt(25) + 97).toChar
      if (i == 0) {
        c.toUpper
      }
      result += c + mkWord(random.nextInt(10)+1)
      if (i != a-1){
        result += " "
      }
      else {
        result += "."
      }
    }
    result
  }

  val sentence1 = mkSentence()
  //> sentence1: String = tkemrlokc ej rhui enqylbdck efun dtppddpl jmwjllp fiftjce imx mvlgqcc.

  val sentence2 = mkSentence()
  //> sentence2: String = cumdlrof xfsokgcy yw qqlielxxg tfeyw xswihqgoyk fpxymwr nsuax dg qntcxvodxmn.

  val sentence3 = mkSentence()
  //> sentence3: String = vqmvstfhqpr qc vqoudydyo vsoyakdf smhguth wopoykux kainliim gtahmtvs stbm suahsdk.

  val sentence4 = mkSentence()
  //> sentence4: String = aimuq jna pwxebmk ps uhlfbqkhcp ucoxev vqjykecsomg gvytvbag dojarlacvbo ahwd.

  //Problem 6
  def shuffle(aString: String)= {   //> shuffle: shuffle[](val aString: String) => String
    var result = ""
    val x = aString.length
    if (x % 2 == 0) {
      result = aString.drop(x/2).take(x/2)
    }
    else {
      result = aString.drop(x/2).take(x/2+1)
    }
    result += aString.take(x/2)
    result
  }

  shuffle("abcdefghij")     //> res7: String = fghijabcde
  shuffle("abcdefghijk")    //> res8: String = fghijkabcde

  //Problem 7
  def countSubstrings(aString: String, bString: String) = {   //> countSubstrings: countSubstrings[](val aString: String,val bString: String) => Int
    var result = 0
    var lastIndex = 0
    while(lastIndex != -1){
      lastIndex = bString.indexOf(aString,lastIndex)
      if (lastIndex != -1){
        result += 1
        lastIndex += aString.length
      }
    }
    result
  }

  countSubstrings("is", "Mississippi")    //> res9: Int = 2

  //Problem 8
  def eval(string: String): Number = {    //> eval: eval[](val string: String) => Number
    val result = string.split("\\s+")
    if (result.length < 2) throw new Exception("Operator must be + ")
    var x = 0.0; var y = 0.0
    try {
      x = result(0).toDouble
      y = result(1).toDouble
      x + y
    } catch {
      case e: Exception => throw new Exception("Arguments must be Number type " + e)
    }
  }

  try {
    eval("3.14+42")
  } catch {
    case e: Exception => println(e)
  }
  try {
    eval("  -26  +  -49.99  ")
  } catch {
    case e: Exception => println(e)
  }
  try {
    eval("21 * 43")
  } catch {
    case e: Exception => println(e)
  }
  try {
    eval("abc + 3")
  } catch {
    case e: Exception => println(e)
  }

  //Problem 9
  def evalProducts(string: String): Number = {  //>evalProducts: evalProducts[](val string: String) => Number
    val result = string.split("\\s*")
    if (result.length < 2) throw new Exception("Operator must be * ")
    var x = 0.0; var y = 0.0
    try {
      x = result(0).toDouble
      y = result(1).toDouble
      x * y
    } catch {
      case e: Exception => throw new Exception("Arguments must be Number type " + e)
    }
  }

  try {
    evalProducts("7*14")
  } catch {
    case e: Exception => println(e)
  }
  try {
    evalProducts("  -26  *  -49.99  ")
  } catch {
    case e: Exception => println(e)
  }
  try {
    evalProducts("21 * 43")
  } catch {
    case e: Exception => println(e)
  }
  try {
    evalProducts("abc * 3")
  } catch {
    case e: Exception => println(e)
  }


  //Problem 10
  def first(aString: Array[String]) = {   //>first: first[](val aString: Array[String]) => String
    val x = aString
    Sorting.quickSort(x)
    x(0)
  }

  first(Array("cat", "rat", "bat"))   //> res10: String = bat

}
