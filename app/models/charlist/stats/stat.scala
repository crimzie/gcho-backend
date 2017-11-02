package models.charlist
package stats

import com.github.dwickern.macros.NameOf
import io.swagger.annotations.ApiModelProperty
import play.api.libs.functional.syntax._
import play.api.libs.json._

sealed abstract class Stat[A <: AnyVal, This](implicit x: scala.math.Numeric[A]) {

  import x._

  val delta: A
  val base : A
  val bonus: A
  val cpMod: Int
  val cp   : Int
  @ApiModelProperty(required = false, value = "Calculated value.")
  val value: A = delta + base + bonus

  protected def copy(
      delta: A = delta,
      base: A = base,
      bonus: A = bonus,
      cpMod: Int = cpMod,
      cp: Int = cp): This

  def calc(base: A, bonus: A, cost: Int, costMod: Int = 0): This = copy(
    delta = if (lt(delta + base + bonus, fromInt(0))) fromInt(0) - base - bonus else delta,
    base = base,
    bonus = bonus,
    cpMod = 100 + costMod,
    cp = Charlist rndUp delta.toDouble * cost * math.max(20, 100 + costMod) * .01)
}

case class StatInt(
    delta: Int = 0,
    @ApiModelProperty(required = false, value = "Calculated value.") base: Int = 0,
    @ApiModelProperty(required = false, value = "Calculated value.") bonus: Int = 0,
    @ApiModelProperty(required = false, value = "Calculated value.") cpMod: Int = 0,
    @ApiModelProperty(required = false, value = "Calculated value.") cp: Int = 0) extends Stat[Int, StatInt] {
  override def copy(
      delta: Int = delta,
      base: Int = base,
      bonus: Int = bonus,
      cpMod: Int = cpMod,
      cp: Int = cp): StatInt = StatInt(delta, base, bonus, cpMod, cp)
}

object StatInt {
  private  val reads : Reads[StatInt]  = ((JsPath \ NameOf.nameOf[StatInt](_.delta)).read[Int] ~
    Reads.pure(0) ~ Reads.pure(0) ~ Reads.pure(0) ~ Reads.pure(0)) (StatInt.apply _)
  implicit val format: Format[StatInt] = Format(reads, Json.writes[StatInt])
}

case class StatDouble(
    delta: Double = 0,
    @ApiModelProperty(required = false, value = "Calculated value.") base: Double = 0,
    @ApiModelProperty(required = false, value = "Calculated value.") bonus: Double = 0,
    @ApiModelProperty(required = false, value = "Calculated value.") cpMod: Int = 0,
    @ApiModelProperty(required = false, value = "Calculated value.") cp: Int = 0) extends Stat[Double, StatDouble] {
  override def copy(
      delta: Double = delta,
      base: Double = base,
      bonus: Double = bonus,
      cpMod: Int = cpMod,
      cp: Int = cp): StatDouble = StatDouble(delta, base, bonus, cpMod, cp)
}

object StatDouble {
  private  val reads : Reads[StatDouble]  = ((JsPath \ NameOf.nameOf[StatInt](_.delta)).read[Double] ~
    Reads.pure(0.0) ~ Reads.pure(0.0) ~ Reads.pure(0) ~ Reads.pure(0)) (StatDouble.apply _)
  implicit val format: Format[StatDouble] = Format(reads, Json.writes[StatDouble])
}
