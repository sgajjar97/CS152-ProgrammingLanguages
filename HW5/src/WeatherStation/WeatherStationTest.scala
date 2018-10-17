package WeatherStation

object WeatherStationTest extends App {
  println("Avg Temp: " + (new FahrenheitTherm).getMeanTemperature(List("SF", "San Jose", "Oakland", "Santa Clara")))
}

//> OUTPUT:
//Avg Temp: 56.0