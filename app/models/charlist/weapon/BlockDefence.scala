package models.charlist.weapon

import com.github.dwickern.macros.NameOf
import io.swagger.annotations.ApiModelProperty
import play.api.libs.functional.syntax._
import play.api.libs.json.{Format, JsPath, Json, Reads}

case class BlockDefence(
    name: String,
    @ApiModelProperty(required = false)
    available: Boolean = true,
    @ApiModelProperty(required = false)
    grip: String = "Default",
    skill: String,
    @ApiModelProperty(required = false, allowEmptyValue = true)
    spc: Option[String] = None,
    @ApiModelProperty(required = true, allowableValues = "range[0, 3]")
    db: Int = 1,
    @ApiModelProperty(required = false)
    notes: String = "")

object BlockDefence {
  private  val reads : Reads[BlockDefence]  = (
    (JsPath \ NameOf.nameOf[BlockDefence](_.name)).read[String] ~
      (JsPath \ NameOf.nameOf[BlockDefence](_.available)).readWithDefault(true) ~
      (JsPath \ NameOf.nameOf[BlockDefence](_.grip)).readWithDefault("Default") ~
      (JsPath \ NameOf.nameOf[BlockDefence](_.skill)).read[String] ~
      (JsPath \ NameOf.nameOf[BlockDefence](_.spc)).readNullable[String] ~
      (JsPath \ NameOf.nameOf[BlockDefence](_.db))
        .read[Int](Reads.max(3).orElse(Reads pure 3) <~ Reads.min(0) orElse Reads.pure(0)) ~
      (JsPath \ NameOf.nameOf[BlockDefence](_.notes)).readWithDefault("")) (BlockDefence.apply _)
  implicit val format: Format[BlockDefence] = Format(reads, Json.writes[BlockDefence])
}
