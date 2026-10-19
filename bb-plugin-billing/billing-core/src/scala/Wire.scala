package billingcore

/** A JSON value with no platform behind it: the shape of what crosses between the page and the plugin. */
enum Wire:
  case Str(value: String)
  case Num(value: Double)
  case Bool(value: Boolean)
  case Arr(items: Vector[Wire])
  case Obj(fields: Vector[(String, Wire)])
  case Null

  /** The field of an object, when this is an object that has it and it is not null. */
  def field(name: String): Option[Wire] =
    this match
      case Obj(fields) => fields.collectFirst { case (`name`, v) if v != Null => v }
      case _           => None

  def asStr: Option[String]                 = this match { case Str(v) => Some(v); case _ => None }
  def asNum: Option[Double]                 = this match { case Num(v) => Some(v); case _ => None }
  def asBool: Option[Boolean]               = this match { case Bool(v) => Some(v); case _ => None }
  def asArr: Option[Vector[Wire]]           = this match { case Arr(v) => Some(v); case _ => None }
  def asObj: Option[Vector[(String, Wire)]] = this match { case Obj(v) => Some(v); case _ => None }

object Wire:
  def obj(fields: (String, Wire)*): Wire.Obj  = Obj(fields.toVector)
  def strings(values: Iterable[String]): Wire = Arr(values.toVector.map(Str.apply))
  def opt(value: Option[Wire]): Wire          = value.getOrElse(Null)
