package models.charlist
package bonuses

trait SkillBased {
  val skill       : String
  val skillCompare: String
  val spc         : Option[String]
  val spcCompare  : Option[String]
}
