package conversecore

import scala.collection.mutable

/** Converts one channel of audio from one sample rate to another as it
  * arrives in pieces, by windowed-sinc interpolation. What it produces does
  * not depend on how the input was divided into pieces.
  */
final class StreamResampler private (
    kernel: StreamResampler.Kernel,
    /** Input still needed for output to come. */
    kept: IArray[Float],
    /** The input index of `kept(0)`. */
    keptStart: Long,
    /** The index of the next output sample. */
    nextOutput: Long
):
  import kernel.{fromRate, toRate, halfWidth}

  /** The resampler with `input` taken in, and the output it completed. */
  def feed(input: IArray[Float]): (StreamResampler, IArray[Float]) =
    val all       = kept ++ input
    val available = keptStart + all.length
    // An output sample is complete once the input reaching its far side has.
    val ready  = countWhile(k => firstInput(k) + 2 * halfWidth <= available)
    val output = IArray.tabulate(ready)(i => sample(all, keptStart, available, nextOutput + i))
    val next   = nextOutput + ready
    val drop   = (firstInput(next) - keptStart).max(0).min(all.length.toLong).toInt
    (new StreamResampler(kernel, all.drop(drop), keptStart + drop, next), output)

  /** The rest of the output, taking the input to have ended. */
  def finish: IArray[Float] =
    val available = keptStart + kept.length
    val total     = (available * toRate + fromRate - 1) / fromRate
    IArray.tabulate((total - nextOutput).max(0).toInt)(i => sample(kept, keptStart, available, nextOutput + i))

  /** The first input index output `k` draws on. */
  private def firstInput(k: Long): Long = k * fromRate / toRate - halfWidth + 1

  private def countWhile(complete: Long => Boolean): Int =
    var n = 0
    while complete(nextOutput + n) do n += 1
    n

  /** Output `k` from the input held in `input` (starting at `start`), with
    * silence before the stream began and after `available`.
    */
  private def sample(input: IArray[Float], start: Long, available: Long, k: Long): Float =
    val weights = kernel.weights(((k * fromRate) % toRate).toInt)
    val first   = firstInput(k)
    var sum     = 0.0
    var i       = 0
    while i < weights.length do
      val n = first + i
      if n >= 0 && n < available then sum += input((n - start).toInt) * weights(i)
      i += 1
    sum.toFloat

object StreamResampler:
  def apply(fromRate: Int, toRate: Int): StreamResampler =
    val divisor = BigInt(fromRate).gcd(BigInt(toRate)).toInt
    new StreamResampler(Kernel(fromRate / divisor, toRate / divisor), IArray.empty, 0, 0)

  /** Zero crossings of the sinc on each side of an output sample. */
  private val ZeroCrossings = 16

  /** The interpolation weights for each fractional input position an output
    * sample can fall at, with the rates in lowest terms so there are `toRate`
    * of them.
    */
  private final class Kernel(val fromRate: Int, val toRate: Int):
    /** Below the lower of the two Nyquist frequencies, so downsampling does
      * not fold higher frequencies into the output.
      */
    private val cutoff = math.min(1.0, toRate.toDouble / fromRate)

    /** Input samples on each side of an output sample. */
    val halfWidth: Int = math.ceil(ZeroCrossings / cutoff).toInt

    private val byPhase = mutable.Map.empty[Int, IArray[Double]]

    /** Weights for the `2 * halfWidth` inputs from `firstInput`, for an
      * output at `phase / toRate` past an input sample.
      */
    def weights(phase: Int): IArray[Double] =
      byPhase.getOrElseUpdate(
        phase, {
          val frac = phase.toDouble / toRate
          val raw = IArray.tabulate(2 * halfWidth) { i =>
            val t = frac + halfWidth - 1 - i
            sinc(cutoff * t) * blackman(t / halfWidth)
          }
          // Normalized so a constant input stays constant.
          val total = raw.sum
          raw.map(_ / total)
        }
      )

    private def sinc(x: Double): Double =
      if x == 0 then 1.0 else math.sin(math.Pi * x) / (math.Pi * x)

    private def blackman(x: Double): Double =
      if math.abs(x) >= 1 then 0.0 else 0.42 + 0.5 * math.cos(math.Pi * x) + 0.08 * math.cos(2 * math.Pi * x)
