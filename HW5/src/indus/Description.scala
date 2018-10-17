package indus

class Description(d: String, p: Double, s: String) {
  private val description = d
  private val price = p
  private val supplier = s

  override def toString() = this.description + " | " + this.price + " | " + this.supplier
}

object Description {
  def apply(d: String, p: Double, s: String) = new Description(d, p, s)
}
