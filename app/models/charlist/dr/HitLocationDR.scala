package models.charlist
package dr

import play.api.libs.json.{Format, Json}

case class HitLocationDR(front: DrSet = DrSet(), rear: DrSet = DrSet())

object HitLocationDR {
  implicit val format: Format[HitLocationDR] = Json.format[HitLocationDR]
}
