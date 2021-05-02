import cats.implicits.catsSyntaxEq
import cats.kernel.Monoid
import cats.kernel.Semigroup
object SetMonoid {
  implicit def setUnionMonoid[A] = new Monoid[Set[A]] {
    override def empty = Set()

    override def combine(a: Set[A], b: Set[A])  = a union b
  }

  val intSetUnion = Monoid[Set[Int]]
  val strSetMonoid = Monoid[Set[String]]

  intSetUnion.combine(Set(1), intSetUnion.empty) == Set(1)

  implicit def setIntersectionMonoid[A] = new Semigroup[Set[A]] {
    override def combine(a: Set[A], b: Set[A]) = a intersect b
  }

  val doubleSetIntersect = Semigroup[Set[Double]]

}