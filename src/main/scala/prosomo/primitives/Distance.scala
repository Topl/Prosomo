package prosomo.primitives

/*
 arc-length distance calculator
 */

object Distance {
  def calculate(lat1: Double, lon1: Double, lat2: Double, lon2: Double, unit: String):Double =
    DistanceCalculator.distance(lat1: Double, lon1: Double, lat2: Double, lon2: Double, unit: String)
}
