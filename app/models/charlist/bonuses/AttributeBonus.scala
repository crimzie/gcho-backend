package models.charlist
package bonuses

import com.github.dwickern.macros.NameOf
import io.swagger.annotations.ApiModelProperty
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class AttributeBonus(
    @ApiModelProperty(required = true, allowableValues = "st,dx,iq,ht,will,fright check,perception,vision,hearing," +
      "taste smell,touch,dodge,parry,block,speed,move,hp,fp,sm,lifting st,striking st")
    attr: String = BonusToAttribute.ST,
    @ApiModelProperty(required = false)
    perLvl: Boolean = false,
    bonus: Double = 0)

object AttributeBonus {
  private  val attrErr                        =
    JsonValidationError(s"Attribute can only be one of: ${BonusToAttribute.canBe mkString ", "}")
  private  val reads : Reads[AttributeBonus]  = (
    (JsPath \ NameOf.nameOf[AttributeBonus](_.attr)).read[String](Reads.filter(attrErr)(BonusToAttribute.canBe)) ~
      (JsPath \ NameOf.nameOf[AttributeBonus](_.perLvl)).readWithDefault(false) ~
      (JsPath \ NameOf.nameOf[AttributeBonus](_.bonus)).read[Double]) (AttributeBonus.apply _)
  implicit val format: Format[AttributeBonus] = Format(reads, Json.writes[AttributeBonus])
}
