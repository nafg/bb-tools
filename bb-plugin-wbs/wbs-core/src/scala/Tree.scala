package wbscore

import io.circe.{Decoder, JsonObject}
import io.circe.derivation.{Configuration, ConfiguredDecoder}
import io.circe.yaml.scalayaml.parser

/** The shape of an estimate file, exactly as the `wbs` skill defines it. */
enum Tree:
  case Hours(hours: Double)
  case Range(low: Double, high: Double)
  case Placeholder(note: String)
  case Group(entries: List[Entry])

/** One key of a group: a named work item, or an `if <proposition>` fork. */
enum Entry:
  case Item(name: String, value: Tree)
  case Fork(proposition: String, branches: Branches)

/** The value under an `if` key: `then` and/or `else`, each a group of work items. */
final case class Branches(thenBranch: Option[Tree.Group], elseBranch: Option[Tree.Group])

object Branches:
  private given Configuration = Configuration.default.withStrictDecoding.withTransformMemberNames {
    case "thenBranch" => "then"
    case "elseBranch" => "else"
    case other        => other
  }
  given Decoder[Branches] =
    ConfiguredDecoder.derived[Branches].ensure(b => b.thenBranch.nonEmpty || b.elseBranch.nonEmpty, "an 'if' needs a 'then' or an 'else'")

object Tree:
  private val IfPrefix = "if "

  /** Only actual numbers: circe's `Decoder[Double]` would accept an empty value (YAML null) as NaN. */
  private given Decoder[Double] = Decoder.decodeJsonNumber.map(_.toDouble)

  given Decoder[Tree] = Decoder.recursive { tree =>
    given Decoder[Tree] = tree
    Decoder[Double].map(Hours(_))
      .or(Decoder[(Double, Double)].map(Range(_, _)))
      .or(Decoder[String].map(Placeholder(_)))
      .or(Decoder[Group].map(identity))
  }

  given Decoder[Group] = Decoder.instance { c =>
    c.as[JsonObject].flatMap { obj =>
      obj.toList.traverse { (key, value) =>
        val at = c.downField(key)
        if key.startsWith(IfPrefix) then at.as[Branches].map(Entry.Fork(key.drop(IfPrefix.length).trim, _))
        else at.as[Tree].map(Entry.Item(key, _))
      }.map(Group(_))
    }
  }

  def parse(source: String): Either[Exception, List[Entry]] =
    parser.parse(source).flatMap(_.as[Group]).map(_.entries)

  extension [A, E, B](xs: List[A])
    private def traverse(f: A => Either[E, B]): Either[E, List[B]] =
      xs.foldRight(Right(Nil): Either[E, List[B]])((a, acc) => for { b <- f(a); rest <- acc } yield b :: rest)
