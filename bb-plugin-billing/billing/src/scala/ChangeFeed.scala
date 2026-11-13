package billing

import bbplugin.BbApi
import billingcore.{BillingEvent, Change, WireJs}

/** Where every write says what it changed. Whatever keeps results drops the ones the changes touch, then pages are told
  * the same changes, so they too refetch only what the changes touch.
  */
final class ChangeFeed(bb: BbApi):
  private var keepers = Vector.empty[Vector[Change] => Unit]

  /** `drop` is told every batch of changes, before pages are. */
  def keep(drop: Vector[Change] => Unit): Unit = keepers :+= drop

  def apply(changes: Change*): Unit =
    if changes.nonEmpty then
      val batch = changes.toVector
      keepers.foreach(_(batch))
      bb.realtime.publish("billing", WireJs.toJs(BillingEvent.Changed(batch).encode))
