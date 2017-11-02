package models.charlist
package stats

import com.github.dwickern.macros.NameOf
import io.swagger.annotations.ApiModelProperty
import models.charlist.skill.SkillBaseAttribute._
import play.api.libs.functional.syntax._
import play.api.libs.json.{Format, JsPath, Json, Reads}

case class Stats(
    @ApiModelProperty(required = false) st: StatInt = Stats.si,
    @ApiModelProperty(required = false) dx: StatInt = Stats.si,
    @ApiModelProperty(required = false) iq: StatInt = Stats.si,
    @ApiModelProperty(required = false) ht: StatInt = Stats.si,
    @ApiModelProperty(required = false) will: StatInt = Stats.si,
    @ApiModelProperty(required = false) per: StatInt = Stats.si,
    @ApiModelProperty(required = false) liftSt: StatInt = Stats.si,
    @ApiModelProperty(required = false) strikeSt: StatInt = Stats.si,
    @ApiModelProperty(required = false) hp: StatInt = Stats.si,
    @ApiModelProperty(required = false) fp: StatInt = Stats.si,
    @ApiModelProperty(required = false) basicSpeed: StatDouble = Stats.sd,
    @ApiModelProperty(required = false) basicMove: StatInt = Stats.si) {
  def cp: Int = (0 /: Seq(st, dx, iq, ht, will, per, liftSt, strikeSt, hp, fp, basicSpeed, basicMove)) (_ + _.cp)

  @ApiModelProperty(hidden = true)
  val byName = Map(
    ST -> st.value,
    DX -> dx.value,
    IQ -> iq.value,
    HT -> ht.value,
    WILL -> will.value,
    PER -> per.value)
}

object Stats {
  private lazy val si                    = StatInt()
  private lazy val sd                    = StatDouble()
  private      val reads : Reads[Stats]  = (
    (JsPath \ NameOf.nameOf[Stats](_.st)).readWithDefault(si) ~
      (JsPath \ NameOf.nameOf[Stats](_.dx)).readWithDefault(si) ~
      (JsPath \ NameOf.nameOf[Stats](_.iq)).readWithDefault(si) ~
      (JsPath \ NameOf.nameOf[Stats](_.ht)).readWithDefault(si) ~
      (JsPath \ NameOf.nameOf[Stats](_.will)).readWithDefault(si) ~
      (JsPath \ NameOf.nameOf[Stats](_.per)).readWithDefault(si) ~
      (JsPath \ NameOf.nameOf[Stats](_.liftSt)).readWithDefault(si) ~
      (JsPath \ NameOf.nameOf[Stats](_.strikeSt)).readWithDefault(si) ~
      (JsPath \ NameOf.nameOf[Stats](_.hp)).readWithDefault(si) ~
      (JsPath \ NameOf.nameOf[Stats](_.fp)).readWithDefault(si) ~
      (JsPath \ NameOf.nameOf[Stats](_.basicSpeed)).readWithDefault(sd) ~
      (JsPath \ NameOf.nameOf[Stats](_.basicMove)).readWithDefault(si)) (Stats.apply _)
  implicit     val format: Format[Stats] = Format(reads, Json.writes[Stats])
}
