import spatial.dsl._

@spatial object HelloSpatial extends SpatialApp {
  def main(args: Array[String]): Unit = {
    val a = 1.to[Int]
    val b = 2.to[Int]
    val argInA = ArgIn[Int]
    val argInB = ArgIn[Int]

    setArg(argInA, a)
    setArg(argInB, b)
    val argOutC = ArgOut[Int]

    Accel {
      argOutC := argInA.value + argInB.value
    }

    val c = getArg(argOutC)
    println("result = " + c)
  }
}
