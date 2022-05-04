package example
import cats.Show
import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import io.circe.literal._
import cats.syntax.foldable._, cats.syntax.functor._, cats.syntax.show._
import io.circe.Decoder
import io.circe.generic.auto._
import io.circe.jawn.decode
import io.circe.literal._

object Hello extends App {
  final case class Hero(a: Int, b: String, c: Double)
  implicit val heroDecoder: Decoder[Hero] = json =>
    for {
      a <- json.downField("a").as[Int]
      b <- json.downField("b").as[String]
      c <- json.downField("c").as[Double]
    } yield Hero(a, b, c)

  val rawSuccessfulJsonString = """
    {"a":10, "b":"test", "c":123}
    """

  println("Successful parse and decode:")
  val decodedFoo = decode[Hero](rawSuccessfulJsonString)
  println(decodedFoo)

  println("")
  println("Decode failure (default output):")
  val decodeEmptyObject = decode[Hero]("{}")
  println(decodeEmptyObject)

  println("")
  println("Decode failure (Show[] output):")
  def printFailuresWithShow(attempt: Either[Error, Hero]): Unit =
    attempt match {
      case Left(error) =>
        error match {
          case parsingFailure: ParsingFailure =>
            println(Show[ParsingFailure].show(parsingFailure))
          case failure: DecodingFailure =>
            println(Show[DecodingFailure].show(failure))
        }
      case Right(value) => println(value)
    }
  printFailuresWithShow(decodeEmptyObject)

  println("")
  println("Decode failure (default output):")
  val decodeDoubleParseError =
    decode[Hero]("""{"a":10, "b":"test", "c":"notanumber"}""")
  println(decodeDoubleParseError)
  println("")
  println("Decode failure (Show[] output):")
  printFailuresWithShow(decodeDoubleParseError)

  println("")
  println("Accumulating nested failures Decode failures (default output):")
  case class Foo(bar: Bar)
  case class Bar(baz: Baz)
  case class Baz(qux: Qux, i: List[Int])
  case class Qux(a: String, b: Boolean)
  val doc = """{
    "bar": {
      "baz": {
        "i": [1, 2, true],
        "qux": { "a": 10, "b": "TRUE" }
      }
    }
  }"""

  val result = decodeAccumulating[Foo](doc)
  println(result.swap.toList)
  // Output: List(NonEmptyList(DecodingFailure(String, List(DownField(a), DownField(qux), DownField(baz), DownField(bar))), DecodingFailure(Boolean, List(DownField(b), DownField(qux), DownField(baz), DownField(bar))), DecodingFailure(Int, List(MoveRight, MoveRight, DownArray, DownField(i), DownField(baz), DownField(bar)))))

  println("")
  println("Accumulating nested failures Decode failures (Show[] output):")
  // Output: List(DecodingFailure at .bar.baz.qux.a: String, DecodingFailure at .bar.baz.qux.b: Boolean, DecodingFailure at .bar.baz.i[2]: Int)
  println(result.swap.toList.flatMap(_.map(_.show).toList))
}
