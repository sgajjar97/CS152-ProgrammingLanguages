package indus

class Item(i: Int, d: Description) {
  private val id = i
  private val description = d

  def getID() = this.id
  def getDesc() = this.description.toString()

  override def toString() = "[ID: " + this.getID + " | Description: " + this.getDesc() +"]" +"\n"

}

object Item {
  private var nextID = 500
  def apply(d: Description) = new Item(getNextID(), d)
  def getNextID() : Int = {
    nextID +=1
    return nextID
  }
}
