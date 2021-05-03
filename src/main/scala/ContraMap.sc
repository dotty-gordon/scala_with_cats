import cats.instances.function._ // for Functor
import cats.syntax.functor._ // for map

trait Printable[A] {
  self =>
  def format(value: A): String
  def contramap[B](func: B => A): Printable[B] =
    new Printable[B] {
      def format(value: B): String = self.format(func(value))
    }
}

def format[A](value: A)(implicit printable: Printable[A]): String ={
  printable.format(value)
}

