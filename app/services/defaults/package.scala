package services

import scala.util.Try

package object defaults {

  implicit class StringWithParsing(val s: String) extends AnyVal {
    def asInt: Int = Try(s.toInt) getOrElse 0

    def asDbl: Double = Try(s.toDouble) getOrElse 0.0
  }

}
