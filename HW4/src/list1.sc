/****************************************
  Assignment 4, List Processing I
  Name: Shail Gajjar
  ***************************************/


/****************************************
  * Problem 1: sum of odd cubes
  ***************************************/
def isOdd(x: Int)={   //>isOdd: isOdd[](val x: Int) => Boolean
  if (x%2 == 0) false
  else true
}
def cube(n: Int) = n * n * n    //>cube: cube[](val n: Int) => Int

// Iterative solution
def sumOfOddCubes(n: List[Int]): Int = {    //>sumOfOddCubes: sumOfOddCubes[](val n: List[Int]) => Int
  var result = 0
  for (i <- n) {
    if(isOdd(i))
      result += cube(i)
  }
  result
}

// Recursive solution
def sumOfOddCubesR(n: List[Int]): Int = {   //>sumOfOddCubesR: sumOfOddCubesR[](val n: List[Int]) => Int
  if (n == Nil) 0
  else if (isOdd(n.head)) cube(n.head) + sumOfOddCubesR(n.tail)
  else sumOfOddCubesR(n.tail)
}

// tail recursive solution
def sumOfOddCubesTR(n: List[Int]) = {   //>sumOfOddCubesTR: sumOfOddCubesTR[](val n: List[Int]) => Int
  def helper(result: Int, unseen: List[Int]): Int =
    if(unseen == Nil) result
    else if (isOdd(unseen.head)) helper(result + cube(unseen.head), unseen.tail)
    else helper(result, unseen.tail)
  helper(0, n)
}

// MPF solution
def sumOfOddCubesMPF(n: List[Int]): Int = {   //>sumOfOddCubesMPF: sumOfOddCubesMPF[](val n: List[Int]) => Int
  n.filter(isOdd).map(cube).reduce(_+_)
}

// testing
var numbers = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) //>numbers: List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
sumOfOddCubes(numbers)      //>res0: Int = 1225
sumOfOddCubesR(numbers)     //>res1: Int = 1225
sumOfOddCubesTR(numbers)    //>res2: Int = 1225
sumOfOddCubesMPF(numbers)   //>res3: Int = 1225


/****************************************
  * Problem 2: sum of sums
  ***************************************/
// Iterative solution
def sumOfSums(n: List[List[Int]]) = {   //>sumOfSums: sumOfSums[](val n: List[List[Int]]) => Int
  var result = 0
  val i = n
  for (j <- i){
    for (k <- j) {
      result += k
    }
  }
  result
}

// Recursive solution
def sumOfNumsR(n: List[Int]): Int =   //>sumOfNumsR: sumOfNumsR[](val n: List[Int]) => Int
  if (n == Nil) 0
  else n.head + sumOfNumsR(n.tail)
def sumOfSumsR(lists: List[List[Int]]): Int =   //>sumOfSumsR: sumOfSumsR[](val lists: List[List[Int]]) => Int
  if (lists == Nil) 0
  else sumOfNumsR(lists.head) + sumOfSumsR(lists.tail)

// tail recursive solution
def sumOfNumsTR(n: List[Int]): Int = {      //>sumOfNumsTR: sumOfNumsTR[](val n: List[Int]) => Int
  def helper(result: Int, unseen: List[Int]): Int =
    if(unseen == Nil) result
    else helper(result + unseen.head, unseen.tail)
  helper(0, n)
}
def sumOfSumsTR(lists: List[List[Int]]): Int = {    //>sumOfSumsTR: sumOfSumsTR[](val lists: List[List[Int]]) => Int
  def helper(result: Int, unseen: List[List[Int]]): Int =
    if(unseen == Nil) result
    else helper(result + sumOfNumsTR(unseen.head), unseen.tail)
  helper(0, lists)
 }

// MPF solution
def sumOfSumsMPF(n: List[List[Int]]): Int = {   //>sumOfSumsMPF: sumOfSumsMPF[](val n: List[List[Int]]) => Int
  (n.flatten).reduce(_+_)
}

// testing
sumOfSums(List(List(1,2,3), List(4,5,6)))     //>res4: Int = 21
sumOfSumsR(List(List(1,2,3), List(4,5,6)))    //>res5: Int = 21
sumOfSumsTR(List(List(1,2,3), List(4,5,6)))   //>res6: Int = 21
sumOfSumsMPF(List(List(1,2,3), List(4,5,6)))  //>res7: Int = 21

/****************************************
  * Problem 3: depth
  ***************************************/
// solution
def depth(v: Any): Int =    //>depth: depth[](val v: Any) => Int
  v match {
  case first::rest => math.max(depth(first) + 1, depth(rest))
  case _ => 0
    }

// testing
val testListD = List(1, List(1, 2), List(List(3, 4)), Nil, List(List(List(5, 6))))  //>testListD: List[Any] = List(1, List(1, 2), List(List(3, 4)), List(), List(List(List(5, 6))))
depth(testListD)  //>res8: Int = 4

/****************************************
  * Problem 6: numPass
  ***************************************/
// Iterative solution
def numPass[T](test: T => Boolean, v: List[T]) = {    //>numPass: numPass[T](val test: T => Boolean,val v: List[T]) => Int
  var result = 0
  for(t <- v if test(t)) result += 1
    result
}

// Recursive solution
def numPassR[T](test: T => Boolean, v: List[T]): Int =    //>numPassR: numPassR[T](val test: T => Boolean,val v: List[T]) => Int
  if (v == Nil) 0
  else if (test(v.head)) 1 + numPassR(test, v.tail)
  else numPassR(test, v.tail)

// tail recursive solution
def numPassTR[T](test: T => Boolean, v: List[T]) = {   //>numPassTR: numPassTR[T](val test: T => Boolean,val v: List[T]) => Int
  def helper(result: Int, unseen: List[T]): Int =
  if (unseen == Nil) result
  else helper(if (test(unseen.head)) result + 1 else result, unseen.tail)
    helper(0, v)
    }

// MPF solution
def numPassMPF[T](test: T => Boolean, v: List[T]) = v.filter(test).size   //>numPassMPF: numPassMPF[T](val test: T => Boolean,val v: List[T]) => Int

// testing
val numbersPass = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)    //>numbersPass: List[Int] = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
numPass(isOdd _, numbersPass)       //>res9: Int = 5
numPassR(isOdd _, numbersPass)      //>res10: Int = 5
numPassTR(isOdd _, numbersPass)     //>res11: Int = 5
numPassMPF(isOdd _, numbersPass)    //>res12: Int = 5

/****************************************
  * Problem 7: allPass
  ***************************************/
// Iterative solution
def allPass[T](test: T => Boolean, v: List[T]) = {    //>allPass: allPass[T](val test: T => Boolean,val v: List[T]) => Boolean
  var result = true
  for(i <- v if result) result = test(i)
    result
    }

// Recursive solution
def allPassR[T](test: T => Boolean, v: List[T]): Boolean =    //>allPassR: allPassR[T](val test: T => Boolean,val v: List[T]) => Boolean
  if (v == Nil) true
  else test(v.head) && allPassR(test, v.tail)

// tail recursive solution
def allPassTR[T](test: T => Boolean, v: List[T]) = {    //>allPassTR: allPassTR[T](val test: T => Boolean,val v: List[T]) => Boolean
  def helper(result: Boolean, unseen: List[T]): Boolean =
  if (unseen == Nil) result
  else helper(result && test(unseen.head), unseen.tail)
    helper(true, v)
    }

// MPF solution
def allPassMPF[T](test: T => Boolean, v: List[T]) =   //>allPassMPF: allPassMPF[T](val test: T => Boolean,val v: List[T]) => Boolean
  if (v == Nil) true else v.map(test).reduce(_ && _)

// testing
val one = List(-1,0,1,2,3,4,5,6,7,8,9,10)   //>one: List[Int] = List(-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

allPass(isOdd _, one)           //>res13: Boolean = false
allPass((x: Int)=> x < 10, one) //>res14: Boolean = false
allPass(isOdd _, Nil)           //>res15: Boolean = true

allPassR(isOdd _, one)            //>res16: Boolean = false
allPassR((x: Int)=> x < 10, one)  //>res17: Boolean = false
allPassR(isOdd _, Nil)            //>res18: Boolean = true

allPassTR(isOdd _, one)           //>res19: Boolean = false
allPassTR((x: Int)=> x < 10, one) //>res20: Boolean = false
allPassTR(isOdd _, Nil)           //>res21: Boolean = true

allPassMPF(isOdd _, one)            //>res22: Boolean = false
allPassMPF((x: Int)=> x < 10, one)  //>res23: Boolean = false
allPassMPF(isOdd _, Nil)            //>res24: Boolean = true


/****************************************
  * Problem 8: somePass
  ***************************************/
// Iterative solution
def somePass[T](test: T => Boolean, v: List[T]) = {   //>somePass: somePass[T](val test: T => Boolean,val v: List[T]) => Boolean
  var result = false
  for(i <- v if !result) result = test(i)
    result
    }

// Recursive solution
def somePassR[T](test: T => Boolean, v: List[T]): Boolean =   //>somePassR: somePassR[T](val test: T => Boolean,val v: List[T]) => Boolean
  if (v == Nil) false
  else test(v.head) || somePassR(test, v.tail)

// tail recursive solution
def somePassTR[T](test: T => Boolean, v: List[T]) = {   //>somePassTR: somePassTR[T](val test: T => Boolean,val v: List[T]) => Boolean
  def helper(result: Boolean, unseen: List[T]): Boolean =
  if (unseen == Nil) result
  else helper(result || test(unseen.head), unseen.tail)
    helper(false, v)
    }

// MPF solution
def somePassMPF[T](test: T=>Boolean, vals: List[T]) =   //>somePassMPF: somePass4[T](val test: T => Boolean,val vals: List[T]) => Boolean
  if (vals == Nil) false else vals.map(test).reduce(_ || _)

// testing
val somePassT = List(9,-9,8,-8,0,3,7,22,10,99,2)  //>somePassT: List[Int] = List(9, -9, 8, -8, 0, 3, 7, 22, 10, 99, 2)

somePass(isOdd _, somePassT)          //>res25: Boolean = true
somePass((x: Int)=> x < 0, somePassT) //>res26: Boolean = true
somePass(isOdd _, Nil)                //>res27: Boolean = false

somePassR(isOdd _, somePassT)           //>res28: Boolean = true
somePassR((x: Int)=> x < 0, somePassT)  //>res29: Boolean = true
somePassR(isOdd _, Nil)                 //>res30: Boolean = false

somePassTR(isOdd _, somePassT)          //>res31: Boolean = true
somePassTR((x: Int)=> x < 0, somePassT) //>res32: Boolean = true
somePassTR(isOdd _, Nil)                //>res33: Boolean = false

somePassMPF(isOdd _, somePassT)           //>res34: Boolean = true
somePassMPF((x: Int)=> x < 0, somePassT)  //>res35: Boolean = true
somePassMPF(isOdd _, Nil)                 //>res36: Boolean = false


/****************************************
  * Problem 10: isSorted
  ***************************************/
// solution
def isSorted(n: List[Int]): Boolean =   //>isSorted: isSorted[](val n: List[Int]) => Boolean
  if (n.size < 2) true else n(0) <= n(1) && isSorted(n.tail)

// testing
val isSortedT = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)    //>isSortedT: List[Int] = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)

isSorted(isSortedT)   //>res37: Boolean = true
isSorted(List(1, 2, 3, 5, 4, 6, 7))   //>res38: Boolean = false

/****************************************
  * Problem 13: streams
  ***************************************/
// ones
def Ones: Stream[Int] = 1 #::Ones   //>Ones: Ones[] => Stream[Int]
val x = Ones    //>x: Stream[Int] = Stream(1, ?)

x(10)   //>res39: Int = 1
x       //>res40: Stream[Int] = Stream(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ?)

// nats
def Nats(from: Int): Stream[Int] = from #::Nats(from + 1)   //>Nats: Nats[](val from: Int) => Stream[Int]
val y = Nats(0)   //>y: Stream[Int] = Stream(0, ?)

y(10)   //>res41: Int = 10
y       //>res42: Stream[Int] = Stream(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, ?)

// evens
val Evens = y.filter((n: Int)=> n % 2 == 0)   //>Evens: scala.collection.immutable.Stream[Int] = Stream(0, ?)

Evens(10)   //>res43: Int = 20
Evens       //>res44: scala.collection.immutable.Stream[Int] = Stream(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, ?)

// squares
val Squares = y.map((n: Int)=> n * n)   //>Squares: scala.collection.immutable.Stream[Int] = Stream(0, ?)
Squares(10)   //>res45: Int = 100
Squares       //>res46: scala.collection.immutable.Stream[Int] = Stream(0, 1, 4, 9, 16, 25, 36, 49, 64, 81, 100, ?)
