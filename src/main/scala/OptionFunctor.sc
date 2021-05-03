import cats.Functor
import cats.syntax.functor._

implicit val optionFunctor = new Functor[Option] {
  override def map[A, B](fa: Option[A])(f: A => B) = fa.map(f)
}

sealed trait Tree[+A]
final case class Branch[A](left: Tree[A], right: Tree[A])
  extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]


implicit val treeFunctor = new Functor[Tree] {
  override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = {
    fa match {
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
      case Leaf(a) => Leaf(f(a))
    }
  }
}

val branch: Tree[Int] = Branch(Leaf(10), Leaf(20))
branch.map(_ * 2)



