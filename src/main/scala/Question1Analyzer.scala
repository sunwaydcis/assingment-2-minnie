//Question1Analyzer.scala
import DataUtils._
import ChartUtils._

//Analyzer for finding the country with highest number of bookings
object Question1Analyzer extends Analyzer[HotelBooking] {
  def label = "Highest Booking Country Analysis"

  //Parse CSV row into HotelBooking object
  def parse(row: String, header: Array[String]): Option[HotelBooking] = {
    val cols = splitRow(row)

    // Use for-comprehension to safely extract all required fields
    for {
      hotel <- safeGet(cols, header.indexOf("Hotel Name"))
      country <- safeGet(cols, header.indexOf("Origin Country"))
      price <- safeDouble(cols, header.indexOf("Booking Price[SGD]"))
      discountStr <- safeGet(cols, header.indexOf("Discount"))
      margin <- safeDouble(cols, header.indexOf("Profit Margin"))
      visitors <- safeInt(cols, header.indexOf("No. Of People"))
    }
    yield HotelBooking(
      hotel,
      country,
      price,
      parseDiscount(discountStr),
      margin,
      visitors
    )
  }

  //Main analysis logic for question 1
  def analyze(rows: List[String], header: Array[String]): Unit = {
    val parsed = rows.flatMap(parse(_, header))

    if (parsed.nonEmpty) {
      //Group bookings by country and count occurrences
      val countryBookings = parsed.groupBy(_.country)
        .view.mapValues(_.size)
        .toMap

      //Find country with maximum bookings
      val (topCountry, count) = countryBookings.maxBy(_._2)

      //Display results
      println("1. 🏆 COUNTRY WITH HIGHEST NUMBER OF BOOKINGS")
      println(s"   ► Country: $topCountry")
      println(s"   ► Number of bookings: $count")

      //Prepare top 10 countries for bar chart
      val topCountries = countryBookings.toList
        .sortBy(-_._2) //Sort descending by count
        .take(10)
        .map((country, count) => (country, count.toDouble))

      barChart("TOP 10 COUNTRIES BY BOOKINGS", topCountries)

      showStatistics(parsed) //Show additional statistics
    } else {
      println("No valid booking data found for analysis")
    }
  }

  //Display statistical insights
  override def showStatistics(bookings: List[HotelBooking]): Unit = {
    val totalCountries = bookings.map(_.country).toSet.size
    val avgBookingsPerCountry = bookings.size.toDouble / totalCountries
    val dataQuality = dataQualityScore(bookings)

    println(s"\n📈 Statistical Insights:")
    println(f"   • Unique countries: $totalCountries")
    println(f"   • Average bookings per country: $avgBookingsPerCountry%.1f")
    println(f"   • Data quality score: $dataQuality%.1f%%")
  }

  //Calculate data quality score based on valid bookings
  private def dataQualityScore(bookings: List[HotelBooking]): Double = {
    val total = bookings.size
    if (total == 0) return 0.0

    //Count bookings with valid data
    val validBookings = bookings.count { b =>
      b.bookingPrice > 0 && b.visitors > 0 && b.profitMargin >= 0
    }
    (validBookings.toDouble / total) * 100
  }
}


