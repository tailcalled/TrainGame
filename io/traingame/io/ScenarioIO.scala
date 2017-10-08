package traingame.io

import traingame._

object ScenarioIO {
  
  def load(data: JSON): Scenario = {
    val JSON.Array(Vector(JSON.Number(width), JSON.Number(height))) = data.dict("size")
    val result = new Scenario(data.dict("name").text.value, width, height)
    for (cityData <- data.dict("cities").array.values.map(_.dict)) {
      val JSON.Array(Vector(JSON.Number(x), JSON.Number(y))) = cityData.dict("pos")
      val city = new City(V2(x, y), cityData("name").text.value)
      result.cities += city
    }
    def lookupCity(name: String) = result.cities.find { x => x.name == name }.get
    for (connData <- data.dict("conns").array.values.map(_.dict)) {
      val color = connData("color") match {
        case JSON.Nil => None
        case JSON.Text(text) => Some(ConnColor.lookup(text))
        case _ => throw new Exception
      }
      val conn = new Connection(
          lookupCity(connData("from").text.value), lookupCity(connData("to").text.value),
          color, connData("length").number.intValue)
      result.edges += conn
    }
    result
  }
  def save(scenario: Scenario): JSON = {
    JSON.Dict(Map(
      "name" -> JSON.Text(scenario.name),
      "size" -> JSON.Array(Vector(JSON.Number(scenario.width), JSON.Number(scenario.height))),
      "cities" -> JSON.Array(scenario.cities.toVector.map(city =>
        JSON.Dict(Map(
          "pos" -> JSON.Array(Vector(JSON.Number(city.pos.x), JSON.Number(city.pos.y))),
          "name" -> JSON.Text(city.name)
        ))
      )),
      "conns" -> JSON.Array(scenario.edges.toVector.map(edge =>
        JSON.Dict(Map(
          "from" -> JSON.Text(edge.from.name),
          "to" -> JSON.Text(edge.to.name),
          "color" -> edge.color.map(c => JSON.Text(c.name)).getOrElse(JSON.Nil),
          "length" -> JSON.Number(edge.length)
        ))
      ))
    ))
  }
  
}