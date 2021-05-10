import cats.instances.function._ // for Functor
import cats.syntax.functor._ // for map

 trait Codec[A] {
   self =>
  def encode(value: A): String
  def decode(value: String): A
  def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
    override def encode(value: B) = self.encode(enc(value))

    override def decode(value: String) = dec(self.decode(value))
  }
}


def encode[A](value: A)(implicit codec: Codec[A]) = codec.encode(value)
def decode[A](value: String)(implicit codec: Codec[A]) = codec.decode(value)

implicit val strCodec: Codec[String] = new Codec[String] {
  override def encode(value: String): String = s"'$value'"
  override def decode(value: String): String = s"'$value'"
}

implicit val intCodec: Codec[Int] = strCodec.imap(_.length, _.toString)


encode[String]("Hello World")
encode[Int](345)

implicit val doubleCodec: Codec[Double] = strCodec.imap(_.length.toDouble, _.toString)
encode[Double](20.5)

final case class Box[A](value: A)

implicit def boxCodec[A](implicit codec: Codec[A]) =
  codec.imap[Box[A]](x => Box(x), _.value)

encode(123.4)
// res11: String = "123.4"
decode[Double]("123.4")
// res12: Double = 123.4
encode(Box(123.4))
// res13: String = "123.4"
decode[Box[Double]]("123.4")
// res14: Box[Double] = Box(123.4)