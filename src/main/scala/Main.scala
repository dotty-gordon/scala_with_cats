import cats.{Eq, Show}
import cats.instances.int._
import cats.instances.string._
import cats.syntax.show._ // for show
import cats.syntax.eq._
import cats.instances.long._
import cats.instances.option._
import cats.syntax.option._

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


    println(new Date().show)

    implicit val showCat: Show[Cat] = Show.show(cat => s"${cat.name} is a ${cat.age} year-old ${cat.color} cat.")
    val garfield = Cat(name = "Garfield", age = 6, color = "Brown")
    println(garfield.show)


    lazy val evaluateDateEq = {
      implicit val dateEq: Eq[Date] =
        Eq.instance[Date] { (date1, date2) =>
          date1.getTime === date2.getTime
        }

      val now = new Date() // now
      val later = new Date() // a bit later than now
      now === now // true
      now === later // false
      println("Date Equality")
    }


    lazy val evaluateCatEq = {

      implicit val catEq: Eq[Cat] =
        Eq.instance[Cat] { (cat1, cat2) =>
          (cat1.name === cat2.name && cat1.age === cat2.age && cat1.color === cat2.color)
        }

      val garfield = Cat(name = "Garfield", age = 6, color = "Brown")
      val fluffy = Cat(name = "Fluffy", age = 10, color = "White")

      val optionGarfield = garfield.some
      val optionNoCat = none[Cat]

      println("/**  Cats Eq */")
      println(garfield === garfield)
      println(garfield =!= fluffy)
      println(garfield =!= fluffy)
      println(optionGarfield =!= optionNoCat)
      println("/**  Cats Eq */")


    }


    evaluateCatEq


  }

  def msg = "I was compiled by dotty :)"

}


