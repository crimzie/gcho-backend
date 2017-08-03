import play.api.libs.json.Writes

/**
  * Created by crimson on 12/8/16.
  */
trait Parser[A] {
  val seq: Seq[A]
  val tjs: Writes[A]

  protected def parseInt(x: String): Int =
    try {
      x.toInt
    } catch {
      case _: Throwable => 0
    }

  protected def parseDouble(x: String): Double =
    try {
      x.toDouble
    } catch {
      case _: Throwable => 0.0
    }
}
