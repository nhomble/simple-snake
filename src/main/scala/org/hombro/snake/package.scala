package org.hombro

package object snake {

  type Color = String
  type Cell = (Int, Int)

  val BLACK = "#000000"
  val GREEN = "#00FF00"
  val RED = "#FF0000"

  val BORDER: Color = BLACK
  val FOOD: Color = RED
  val BODY: Color = GREEN

  def keyDirection(code: Int): Option[Direction] = {
    code match {
      case 87 => Some(North())
      case 65 => Some(West())
      case 83 => Some(South())
      case 68 => Some(East())
      case 38 => Some(North())
      case 37 => Some(West())
      case 40 => Some(South())
      case 39 => Some(East())
      case _ => None
    }
  }

  sealed trait Direction {
    def opposite(): Direction
  }

  case class North() extends Direction {
    override def opposite(): Direction = South()
  }

  case class South() extends Direction {
    override def opposite(): Direction = North()
  }

  case class East() extends Direction {
    override def opposite(): Direction = West()
  }

  case class West() extends Direction {
    override def opposite(): Direction = East()
  }

}
