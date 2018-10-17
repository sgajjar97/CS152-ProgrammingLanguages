package dnd

object Dungeon {

  def main(args: Array[String]) {
    val random = new scala.util.Random(System.nanoTime())
    val K = Knight("John Snow", 100)
    val D = Dragon("Charizard", 100)

    var timer = 0
    while (K.hp > 0 && D.hp > 0) {
      if (timer == 0) {
        val Kdamage = random.nextInt(K.hp)
        K.attack(D, Kdamage)
        println(K.name + " attacked " + D.name + " for " + Kdamage + " damage. [" + D.name + " is at " + D.hp + "]")
        timer = 1
      } else {
        val Ddamage = random.nextInt(D.hp)
        D.attack(K, Ddamage)
        println(D.name + " attacked " + K.name + " for " + Ddamage + " damage. [" + K.name + " is at " + K.hp + "]")
        timer = 0
      }
    }
    if(K.hp <= 0) println(K.name + " has died! :(") else println(D.name + " has died! :(")
  }
}

//> OUTPUT:
//John Snow attacked Charizard for 86 damage. [Charizard is at 14]
//Charizard attacked John Snow for 9 damage. [John Snow is at 91]
//John Snow attacked Charizard for 57 damage. [Charizard is at -43]
//Charizard has died! :(