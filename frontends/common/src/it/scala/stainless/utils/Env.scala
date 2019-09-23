/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package utils

object Env {

  def getBoolean(name: String): Option[Boolean] = {
    sys.env.get(name).flatMap {
      case "true"  => Some(true)
      case "false" => Some(false)
      case _       => None
    }
  }


  def getBooleanOrDefault(name: String, default: => Boolean): Boolean = {
    getBoolean(name).getOrElse(default)
  }

}
