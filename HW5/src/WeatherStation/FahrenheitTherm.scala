package WeatherStation

class FahrenheitTherm extends CelsiusTherm with IThermometer {
  def getMeanTemperature(cities: List[String]): Double = {
    var sum = cities.map(computeTemp(_)).sum
    sum /= cities.size
    sum = sum*(9/5) + 32
    sum
  }
}
