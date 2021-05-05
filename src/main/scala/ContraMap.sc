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


implicit val stringPrintable: Printable[String] =
  new Printable[String] {
    def format(value: String): String =
      s"'${value}'"
  }
implicit val booleanPrintable: Printable[Boolean] =
  new Printable[Boolean] {
    def format(value: Boolean): String =
      if(value) "yes" else "no"
  }
format("hello")
// res2: String = "'hello'"
format(true)
// res3: String = "yes"

final case class Box[A](value: A)

implicit def boxPrintable[A](implicit printable: Printable[A]): Printable[Box[A]] =
  printable.contramap(x => x.value)


val boxStr = Box[String]("TestString")
val boxBool = Box[Boolean](false)
val boxInt = Box[Int](123) //won't compile as no Printable[Int] defined


def print[A](box: Box[A])(implicit  printBox: Printable[Box[A]] ) = printBox.format(box)


print(boxStr)
print(boxBool)

print(Box("hello world"))
// res4: String = "'hello world'"
print(Box(true))
// res5: String = "yes"