import cats.Monoid
import cats.syntax.semigroup._
import cats.instances.int._
import cats.instances.double._
import cats.instances.option._

def add[A](items: List[A])(implicit monoid: Monoid[A]): A =
  items.foldLeft(monoid.empty)(_ |+| _)

add(List(1,2))
add(List(Some(1), Some(3), None))

case class Order(totalCost: Double, quantity: Double)

implicit val orderSemiGroup = new Monoid[Order] {
  override def combine(a: Order, b: Order) =
    Order(a.totalCost |+| b.totalCost,
      a.quantity |+| b.quantity)

  override def empty = Order(0,0)
}

val amazonOrderOne = Order(20.5, 3.0)
val amazonOrderTwo = Order(120.5, 7.0)

add(List(amazonOrderOne, amazonOrderTwo))