package indus

import scala.collection.mutable.ArrayBuffer

object testIndus extends App {
  var Inventory = new ArrayBuffer[Item]
  var matrix = Item(Description("The Matrix DVD", 15.50, "DVD World"))
  var terminator = Item(Description("The Terminator DVD", 13.25, "DVD World"))
  var ironman = Item(Description("The IronMan DVD", 18.00, "DVD Planet"))

  Inventory+= matrix
  Inventory+= matrix
  Inventory+= matrix
  Inventory+= matrix
  Inventory+= matrix
  Inventory += terminator
  Inventory += terminator
  Inventory += terminator
  Inventory+= ironman
  Inventory+= ironman

  println(Inventory)
}

//> OUTPUT:
//ArrayBuffer([ID: 501 | Description: The Matrix DVD | 15.5 | DVD World]
//, [ID: 501 | Description: The Matrix DVD | 15.5 | DVD World]
//, [ID: 501 | Description: The Matrix DVD | 15.5 | DVD World]
//, [ID: 501 | Description: The Matrix DVD | 15.5 | DVD World]
//, [ID: 501 | Description: The Matrix DVD | 15.5 | DVD World]
//, [ID: 502 | Description: The Terminator DVD | 13.25 | DVD World]
//, [ID: 502 | Description: The Terminator DVD | 13.25 | DVD World]
//, [ID: 502 | Description: The Terminator DVD | 13.25 | DVD World]
//, [ID: 503 | Description: The IronMan DVD | 18.0 | DVD Planet]
//, [ID: 503 | Description: The IronMan DVD | 18.0 | DVD Planet]
//)