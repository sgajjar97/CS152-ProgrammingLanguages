
object DDS {

  //Problem 1
  def controlLoop[S](   //>controlLoop: controlLoop[S](val state: S,val cycle: Int,val halt: (S, Int) => Boolean,val update: (S, Int) => S) => S
                      state: S,
                      cycle: Int,
                      halt: (S, Int) => Boolean,
                      update: (S, Int) => S): S = {
    val x = halt(state, cycle)
    if(x){
      state
    }
    else{
      controlLoop(update(state, cycle), 1 + cycle, halt, update)
    }
  }

  //Problem 2
  def newUpdate(currentState: Int, cycle: Int) = {    //>newUpdate: newUpdate[](val currentState: Int,val cycle: Int) => Int
    val x = currentState * 2
    x
  }
  def newHalt(currentState: Int, cycle: Int)= {   //>newHalt: newHalt[](val currentState: Int,val cycle: Int) => Boolean
    if (currentState >= math.pow(10, 5))
      true
    else
      false
  }
  controlLoop(1,0, newHalt, newUpdate)    //>res0: Int = 131072

  //Problem 3
  def solve(f: Double=>Double) = {    //>solve: solve[](val f: Double => Double) => Double
    val delta = 1e-5
    def fPrime(a: Double) = (f(a + delta) - f(a))/delta
    def approx_roots(guess: Double, cycle: Int) = guess - f(guess)/fPrime(guess)
    def close_approx(guess: Double, cycle: Int) = math.abs(f(guess)) < delta
    controlLoop(1.00, 0, close_approx _, approx_roots _)
  }

  //Problem 4
  def squareRoot(x: Double)={   //>squareRoot: squareRoot[](val x: Double) => Double
    def func(a: Double) = math.pow(a,2)-x
    solve(func _)
  }

  squareRoot(9)   //>res1: Double = 3.0000000015508212
  squareRoot(49)  //>res2: Double = 7.000000142285558


  //Problem 5
  def cubeRoot(x: Double)={   //>cubeRoot: cubeRoot[](val x: Double) => Double
    def func(a: Double) = math.pow(a,3)-x
    solve(func _)
  }

  cubeRoot(8)   //>res3: Double = 2.000000000036784
  cubeRoot(64)  //>res4: Double = 4.0000000001199725

  //Problem 6
  def nthRoot(x: Double, n: Int)={    //>nthRoot: nthRoot[](val x: Double,val n: Int) => Double
    def func(a: Double) = math.pow(a,n)-x
    solve(func _)
  }

  nthRoot(9,2)    //>res5: Double = 3.0000000015508212
  nthRoot(49,3)   //>res6: Double = 3.6593057100236823

}