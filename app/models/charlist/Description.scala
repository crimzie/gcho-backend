package models.charlist

import com.github.dwickern.macros.NameOf
import io.swagger.annotations.ApiModelProperty
import play.api.libs.functional.syntax._
import play.api.libs.json.{Format, JsPath, Json, Reads}

case class Description(
    @ApiModelProperty(required = false) age: String = "",
    @ApiModelProperty(required = false) height: String = "",
    @ApiModelProperty(required = false) weight: String = "",
    @ApiModelProperty(required = false) bio: String = "")

object Description {
  private  val reads : Reads[Description]  = (
    (JsPath \ NameOf.nameOf[Description](_.age)).readWithDefault("") ~
      (JsPath \ NameOf.nameOf[Description](_.height)).readWithDefault("") ~
      (JsPath \ NameOf.nameOf[Description](_.weight)).readWithDefault("") ~
      (JsPath \ NameOf.nameOf[Description](_.bio)).readWithDefault("")) (Description.apply _)
  implicit val format: Format[Description] = Format(reads, Json.writes[Description])
}
