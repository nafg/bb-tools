package billingcore

import scala.scalajs.js

/** `Wire` as the plain JavaScript values bb's rpc carries, and back. */
object WireJs:
  def toJs(wire: Wire): js.Any =
    wire match
      case Wire.Str(v)      => v
      case Wire.Num(v)      => v
      case Wire.Bool(v)     => v
      case Wire.Arr(items)  => js.Array(items.map(toJs)*)
      case Wire.Obj(fields) =>
        val o = js.Dynamic.literal()
        fields.foreach((k, v) => o.updateDynamic(k)(v match { case Wire.Null => null; case other => toJs(other) }))
        o
      case Wire.Null        => null

  def fromJs(value: js.Any): Wire =
    (value: Any) match
      case null                          => Wire.Null
      case s: String                     => Wire.Str(s)
      case d: Double                     => Wire.Num(d)
      case b: Boolean                    => Wire.Bool(b)
      case a: js.Array[?]                => Wire.Arr(a.asInstanceOf[js.Array[js.Any]].toVector.map(fromJs))
      case o if js.typeOf(o) == "object" =>
        val d = o.asInstanceOf[js.Dynamic]
        Wire.Obj(js.Object.keys(o.asInstanceOf[js.Object]).toVector.map(k => k -> fromJs(d.selectDynamic(k))))
      case _                             => Wire.Null
