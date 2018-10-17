object midterm {
  println("***Midterm scala worksheet***")

  //problem 1
  def sumFirsts(lists: List[List[Int]]): Int = {
    def helper(res: Int, sums: List[List[Int]]): Int = {
      if(sums == Nil) res
      else if(sums.head == Nil)
        helper(res, sums.tail)
      else
        helper(res + sums.head.head, sums.tail)
    }
    helper(0, lists)
  }

  //problem 1 testing
  sumFirsts(List(List(1), Nil, List(2, 3, 4)))
  sumFirsts(Nil)
  //res1: Int = 3
  //res2: Int = 0


  //problem 2
  trait Command {
    def execute()
  }

  object accumulator {
    var accum: Int = 0
    def execute(program: Command*) = {
      accum = 0
      for(cmmd <- program) cmmd.execute() // updates accum
      accum
    }
  }

  class Set(value: Int = 0) extends Command {
    override def execute() = accumulator.accum = value
  }
  object Set {
    def apply(value: Int) = new Set(value)
  }

  class Mul(value: Int) extends Command {
    override def execute() = accumulator.accum = accumulator.accum * value
  }
  object Mul {
    def apply(value: Int) = new Mul(value)
  }

  class Iter(command: Command, iters: Int) extends Command  {
    override def execute() = {
      for( i <- 1 to iters)
        command.execute()
    }
  }

  object Iter {
    def apply(command: Command, iters: Int) = new Iter(command, iters)
  }

  //problem 2 test
  accumulator.execute(Set(2), Mul(3), Iter(Mul(2), 5))
  // res3: Int = 192


  //problem 3
  def applyFuns[T, S](t: T, transformers: List[T => S]): List[S] = {
    var res: List[S] = List[S]()
    transformers.map((transformer) => {
      res = (transformer(t)) :: res
    })
    res.reverse
  }

  val myFuns = List((x: Int) => 2 * x, (x: Int) => x * x, (x: Int) => x + 1) // = a list of transformer functions
  applyFuns(10, myFuns) // = List(20, 100, 11)

}