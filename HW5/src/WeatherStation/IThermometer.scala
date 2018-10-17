package WeatherStation

trait IThermometer {
  def getMeanTemperature(cities : List[String]): Double
}