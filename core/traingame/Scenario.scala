package traingame

import java.awt.Color

sealed trait ConnColor {
  import ConnColor._
  def name = toString
  lazy val toAWT = this match {
    case Red => Color.RED
    case Orange => Color.ORANGE
    case Yellow => Color.YELLOW
    case Green => Color.GREEN
    case Blue => Color.BLUE
    case Purple => Color.MAGENTA
    case Black => Color.DARK_GRAY
    case White => Color.WHITE
  }
}
object ConnColor {
  case object Red extends ConnColor
  case object Orange extends ConnColor
  case object Yellow extends ConnColor
  case object Green extends ConnColor
  case object Blue extends ConnColor
  case object Purple extends ConnColor
  case object Black extends ConnColor
  case object White extends ConnColor
  val connColors = Vector[ConnColor](Red, Orange, Yellow, Green, Blue, Purple, Black, White)
  val lookup = connColors.map(col => col.name -> col).toMap
}

class City(var pos: V2, var name: String)
class Connection(var from: City, var to: City, var color: Option[ConnColor], var length: Int)

// do not actually mutate the scenario once a game has started, or things will likely break
class Scenario(var name: String, var width: Double, var height: Double) {
  
  var edges = Set[Connection]()
  var cities = Set[City]()
  def connections(to: City) = edges.filter(c => c.from == to || c.to == to)
  
}