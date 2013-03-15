package multitool

object Shuffle {
  def apply[A](data : Array[A]) {
	val rando = new java.util.Random()
	for (n <- Iterator.range(data.length - 1,0,-1)) {
	  val k = rando.nextInt(n + 1)
	  val t = data(k);data(k) = data(n);data(n) = t
	}
  }
}
