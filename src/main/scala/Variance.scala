class Variance {

  // variance is about being able to substitute one value for another
  sealed trait Shape
  case class Circle(radius: Double) extends Shape

  val circles: List[Circle]= List()
  val shapes: List[Shape] = circles
  Option()
}
