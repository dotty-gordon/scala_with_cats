import cats.Show
import cats.instances.int._ // for Show
import cats.instances.string._ // for Show
import cats.syntax.show._ // for show


trait Printable[T] {
  def format(value: T): String
}

final case class Cat(name: String, age: Int, color: String)

object PrintableInstances {
  implicit val stringPrintable: Printable[String] = new Printable[String] {
    override def format(value: String): String = value.toUpperCase
  }
  //
  implicit val intPrintable: Printable[Int] = new Printable[Int] {
    override def format(value: Int): String = value.toString
  }
}

object Cat {
  import PrintableInstances._
  implicit val catPrintable: Printable[Cat] = new Printable[Cat] {
    override def format(value: Cat) = {
      val name = Printable.format(value.name)
      val age = Printable.format(value.age)
      val color = Printable.format(value.color)
      s"$name is a $age year-old $color cat."
    }
  }

}

object PrintableSyntax {
  implicit class PrintableOps[A](value: A) {
    def format(implicit p: Printable[A]): String =
      p.format(value)
    def print(implicit p: Printable[A]): Unit =
      println(format(p))
  }
}

object Printable {
  def format[T](value: T)(implicit printable: Printable[T]) = printable.format(value)
  def print[T](value: T)(implicit printable: Printable[T]) = println(printable.format(value))
}

import java.util.Date


object Main {

  def main(args: Array[String]): Unit = {
    val showInt: Show[Int] = Show.apply[Int]
    val showString: Show[String] = Show.apply[String]

    val intAsString: String =
      showInt.show(123)
    // intAsString: String = "123"
    val stringAsString: String =
      showString.show("abc")
    // stringAsString: String = "abc"

    val shownInt = 123.show
    // shownInt: String = "123"
    val shownString = "abc".show
    // shownString: String = "abc"

    println(intAsString)
    println(stringAsString)
    println(shownInt)
    println(shownString)

//    implicit val showDate = new Show[Date] {
//      override def show(t: Date): String = s"${t.getTime}ms since the epoch."
//    }

    implicit val showDateUsingCompanionMethods: Show[Date] = Show.show(date => s"${date.getTime}ms since the epoch.")

   // implicit val showDate: Show[Date] = Show.fromToString


    print(new Date().show)

    implicit val showCat: Show[Cat] = Show.show(cat =>  s"${cat.name} is a ${cat.age} year-old ${cat.color} cat.")
    val garfield = Cat(name = "Garfield", age = 6, color = "Brown")
    print(garfield.show)

  }

  def msg = "I was compiled by dotty :)"

}


