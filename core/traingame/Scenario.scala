package traingame

import java.awt.Color

sealed trait ConnColor {
  import ConnColor._
  def name = toString
  def toAWT = this match {
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
  val connColors = Vector(Red, Orange, Yellow, Green, Blue, Purple, Black, White)
  val lookup = connColors.map(col => col.name -> col).toMap
}

trait CityData {
  def pos: V2
  def name: String
}
trait ConnectionData[scenario <: Scenario] {
  def from: scenario#City
  def to: scenario#City
  def color: Option[ConnColor]
  def length: Int
}
trait Scenario {
  
  type City <: CityData
  type Connection <: ConnectionData[this.type]
  
  def width: Double
  def height: Double
  
  def edges: Set[Connection]
  def cities: Set[City] = edges.flatMap(edge => Set(edge.from, edge.to))
  def name: String
  
}

// do not actually mutate the scenario once a game has started, or things will likely break
class MutableScenario(var name: String, var width: Double, var height: Double) extends Scenario {
  
  class City(var pos: V2, var name: String) extends CityData
  class Connection(var from: City, var to: City, var color: Option[ConnColor], var length: Int) extends ConnectionData[this.type]
  
  var edges = Set[Connection]()
  private var _cities = Set[City]()
  override def cities = _cities
  def cities_=(newCities: Set[City]) = {
    _cities = newCities
  }
  
}