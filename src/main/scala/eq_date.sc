import cats.Eq
import cats.implicits._
import cats.instances.string._
import cats.instances.option._

final case class Cat(name: String, age: Int, color: String)

implicit val catEq = Eq.instance[Cat] { (cat1, cat2) =>
  cat1.name === cat2.name
}

val cat1 = Cat("Garfield", 38, "orange and black")
val cat2 = Cat("Garfield", 33, "orange and black")
val oCat1 = Option(cat1)
val oCat2 = Option(cat2)

cat1 === cat2

