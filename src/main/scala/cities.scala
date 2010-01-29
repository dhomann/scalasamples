import util.matching.Regex
import util.matching.Regex.Match
import Math._

val MetersPerKilometer = 1000.0;
object Earth {
  val Radius: Double = 6378000.0; // in meters
}

/**
 * Represents a coordinate in degrees and minutes of arc.
 * TODO: extend to support seconds
 */
class Coordinate(val degrees: Int, val minutes: Double, val heading: Char) {
  require("NSWE".toCharArray.contains(heading))
  require(degrees >= 0)
  require(minutes >= 0)
  
  override def toString: String = {
    List(degrees formatted "%d", "° ", minutes formatted "%.0f", "' ", heading).mkString
  }

  /**
   * Convert coordinate value to radians, with negative values for Western and Southern hemispheres.
   */
  def toRadians:Double = {
    val decDeg: Double = degrees + (minutes / 60.0)
    Math toRadians (heading match {
      case 'W' => -decDeg
      case 'S' => -decDeg
      case _ => decDeg
    })
  }
}
object Coordinate {
  /**
   * Factory method provides a convenient way to create a {@link Coordinate} from its String representation, e.g.
   * <code>53° 33' N</code>
   */
  def apply(s: String): Coordinate = {
    val m: Option[Match] = new Regex("(\\d+)°\\s+(\\d+)'\\s+([NSWE])").findFirstMatchIn(s)
    m match {
      case Some(md) => new Coordinate(md.group(1).toInt, md.group(2).toDouble, md.group(3).charAt(0))
      case None => throw new IllegalArgumentException
    }
  }
}

/** Implicitly converts a coordinate to radians, when a Double type is required. */
implicit def coordinateInRadians(coord: Coordinate): Double = coord.toRadians

/** Implicitly creates a coordinate value from a String. */
implicit def stringToCoordinate(s: String): Coordinate = Coordinate(s)

/**
 * A Location has two coordinates.
 * TODO: require heading is N/S for latitude and W/E for longitude
 */
class Location(val latitude: Coordinate, val longitude: Coordinate) {
  override def toString: String = List(latitude, ", ", longitude).mkString

  /**
   * Calculate the distance to another Location using the Haversine formula, as described here:
   * <a href="http://www.movable-type.co.uk/scripts/latlong.html">
   *   Calculate distance, bearing and more between Latitude/Longitude points
   * </a>
   */
  def distanceTo(other: Location): Double = {
    def sqr(x: Double) = x * x

    val dlat = other.latitude.toRadians - latitude.toRadians
    val dlong = other.longitude.toRadians - longitude.toRadians
    val a = sqr(sin(dlat/2)) + cos(latitude) * cos(other.latitude) * sqr(sin(dlong/2))
    val c = 2 * atan2(sqrt(a), sqrt(1-a))
    c * Earth.Radius
  }
}

/**
 * Located trait gives objects a location.
 */
trait Located {
  // abstract parameter-less method
  val location: Location

  // Note: this would not be possible with a Java interface
  def distanceTo(that: Located): Double = this.location distanceTo that.location
}

// City implements the location field in the Located trait via its class parameter
class City(val name: String, val location: Location) extends Located {
  override def toString = name
}

val Paris = new City("Paris", new Location("48° 51' N", "2° 21' E"))
val Hamburg = new City("Hamburg", new Location("53° 33' N", "10° 0' E"))
val Prague = new City("Prag", new Location("50° 5' N", "14° 25' E"))
val NewYorkCity = new City("New York City", new Location("40° 43' N", "74° 0' W"))
val Sydney = new City("Sydney", new Location("33° 51' S", "151° 12' E"))
val Singapore = new City("Singapore", new Location("1° 17' N", "103° 50' E"))
val SanFrancisco = new City("San Francisco", new Location("37° 46' N", "122° 26' W"))

val cities = List(Paris, Hamburg, Prague, NewYorkCity, Sydney, Singapore, SanFrancisco)

cities.foreach { c => println(c.name + ": " + c.location) }

println

// formats a Double meter value to km String
def km(meters: Double) = meters/MetersPerKilometer formatted "%.0f km"

// Calls the Located#distanceTo method on Hamburg directly
println("Hamburg-Paris: " + km(Hamburg.distanceTo(Paris)))

// create a new function value Located => Double.
// This is a function with just one argument, returning the distance from Hamburg.
val distanceFromHH = Hamburg.distanceTo(_)
cities.foreach(city => println("Hamburg to " + city + ": " + km(distanceFromHH(city))))

// Sort by distance from home
// #sort is called with another function value, comparing the distances to Hamburg of two cities
println("By distance from HH: " + cities.sort(distanceFromHH(_) < distanceFromHH(_)).mkString(", "))

// Find close cities
// #filter is called with a function value, which takes a city as an argument and returns true,
// if it's less than 1000km from HH
println("Closer than 1000 km: " + cities.filter(distanceFromHH(_) < 1000*MetersPerKilometer).mkString(", "))

// Partition all cities by their longitudinal hemisphere. Results in two lists of cities.
var hemispheres = cities.partition(
  _.location.longitude.heading match {
    case 'W' => true
    case 'E' => false
  }
)
println("Hemispheres: " + hemispheres)

// compute total length of path recursively
def pathLength(path: List[City]): Double = path match {
  case l1 :: l2 :: remainder => {
    println(l1 + " - " + l2 + ": " + km(l1.distanceTo(l2)))
    l1.distanceTo(l2) + pathLength(l2 :: remainder)
  }
  case _ => 0.0
}
println("Travel distance for " + cities + ": " + km(pathLength(cities)))

// creates a list of tuples (city -> nearest city)
val nearestCitiesTuples =
for {
  city <- cities                     // iterate over all cities
  others = cities.filter(_ != city)  // others are all the other cities without the current one
} yield                              // yield a tuple [City, City]
  (city,                                                 // the current city
    others.reduceLeft { (c1, c2) =>                      // reduce the other cities, finding the one which is closest
      if (city.distanceTo(c1) < city.distanceTo(c2))     // the result of one reduction step is passed into the next as c1
        c1
      else
        c2
    })

// converts list of tuples to Map of city -> nearest city.
// It's _* because Map() takes a variable number of arguments.
println("Nearest cities: " + Map(nearestCitiesTuples.toArray: _*))