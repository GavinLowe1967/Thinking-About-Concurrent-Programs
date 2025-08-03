object Outer{
  object Inner{
    println("Inner created")
    val x = 42
  }

  def main(args: Array[String]) = { println("hello"); println(Inner.x) }
}

