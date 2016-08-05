def sumSegment(a: Array[Int], p: Double, s: Int, t: Int): Int = {
  val subArray = for (i: Int <- s until t) yield Math.pow(Math.abs(a(i)), p).toInt
  Math.pow(subArray sum, 1/p).toInt
}


sumSegment(Array(1, 2, 3), 2, 0, 3)




