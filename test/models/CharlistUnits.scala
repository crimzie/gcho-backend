package models

import models.charlist.Charlist
import org.scalatestplus.play.PlaySpec
import play.api.libs.json.{JsValue, Json}

class CharlistUnits extends PlaySpec {

  "Charlist" must {
    "recalc successfully" in {
      val char = Charlist(player = "")
      char.calc() mustBe char.calc()
    }

    "serialize and deserialize successfully" in {
      val defaultChar: Charlist = Charlist(player = "").calc()
      val json: String = Json toJsObject defaultChar toString()
      val jsvChar: JsValue = Json parse json
      val char: Charlist = Json fromJson[Charlist] jsvChar get;
      char.calc() mustBe char.calc()
    }
  }

}
