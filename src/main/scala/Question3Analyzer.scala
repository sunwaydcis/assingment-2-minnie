import DataUtils._
import ChartUtils._

object Question3Analyzer extends Analyzer[HotelBooking] {
  def label = "Most Profitable Hotel Analysis"

  
  def parse(row: String, header: Array[String]): Option[HotelBooking] = {
    val cols = splitRow(row)

    for {
      hotel <- safeGet(cols, header.indexOf("Hotel Name"))
      originCountry <- safeGet(cols, header.indexOf("Origin Country"))
      destinationCountry <- safeGet(cols, header.indexOf("Destination Country"))
      destinationCity <- safeGet(cols, header.indexOf("Destination City"))
      price <- safeDouble(cols, header.indexOf("Booking Price[SGD]"))
      discountStr <- safeGet(cols, header.indexOf("Discount"))
      margin <- safeDouble(cols, header.indexOf("Profit Margin"))
      visitors <- safeInt(cols, header.indexOf("No. Of People"))
    } yield HotelBooking(
      hotel,
      originCountry,
      price,
      parseDiscount(discountStr),
      margin,
      visitors
    )
  }

  // Helper method to extract city and country along with booking
  private def parseWithLocation(row: String, header: Array[String]): Option[(HotelBooking, String, String)] = {
    val cols = splitRow(row)

    for {
      hotel <- safeGet(cols, header.indexOf("Hotel Name"))
      originCountry <- safeGet(cols, header.indexOf("Origin Country"))
      destinationCountry <- safeGet(cols, header.indexOf("Destination Country"))
      destinationCity <- safeGet(cols, header.indexOf("Destination City"))
      price <- safeDouble(cols, header.indexOf("Booking Price[SGD]"))
      discountStr <- safeGet(cols, header.indexOf("Discount"))
      margin <- safeDouble(cols, header.indexOf("Profit Margin"))
      visitors <- safeInt(cols, header.indexOf("No. Of People"))
    } yield (
      HotelBooking(hotel, originCountry, price, parseDiscount(discountStr), margin, visitors),
      destinationCity,
      destinationCountry
    )
  }

  def analyze(rows: List[String], header: Array[String]): Unit = {
    // Use helper method to parse with location
    val parsedWithLocation = rows.flatMap(parseWithLocation(_, header))

    if (parsedWithLocation.nonEmpty) {
      println("3. MOST PROFITABLE HOTEL (Visitors × Profit Margin)")

      // Group by hotel AND location (hotel, city, country)
      val hotelGroups = parsedWithLocation.groupBy { case (booking, city, country) =>
        (booking.hotel, city, country)
      }

      // Calculate metrics for each hotel location
      val hotelMetrics = hotelGroups.map { case ((hotelName, city, country), bookingsWithLocation) =>
        val bookings = bookingsWithLocation.map(_._1) // Extract just the HotelBooking objects

        // Factor 1: Total number of visitors
        val totalVisitors = bookings.map(_.visitors).sum

        // Factor 2: Average profit margin
        val avgProfitMargin = bookings.map(_.profitMargin).sum / bookings.size

        val totalBookings = bookings.size

        (hotelName, city, country, totalVisitors, avgProfitMargin, totalBookings)
      }.toList

      // Get min and max for normalization
      val allVisitors = hotelMetrics.map(_._4)
      val allMargins = hotelMetrics.map(_._5)

      val minVisitors = allVisitors.min
      val maxVisitors = allVisitors.max
      val minMargin = allMargins.min
      val maxMargin = allMargins.max

      // Calculate normalized scores
      val hotelsWithScores = hotelMetrics.map { case (hotel, city, country, visitors, margin, bookings) =>
        // Normalize visitors: (visitors - min) / (max - min)
        val visitorScore = if (maxVisitors > minVisitors)
          (visitors - minVisitors).toDouble / (maxVisitors - minVisitors)
        else 0.5

        // Normalize profit margin: (margin - min) / (max - min)
        val marginScore = if (maxMargin > minMargin)
          (margin - minMargin) / (maxMargin - minMargin)
        else 0.5

        // Combined score: 50% visitors + 50% profit margin
        val combinedScore = (visitorScore + marginScore) / 2

        (hotel, city, country, combinedScore, visitorScore, marginScore, visitors, margin, bookings)
      }

      // Find hotel with highest combined score
      val mostProfitable = hotelsWithScores.maxBy(_._4)

      println(s"\n MOST PROFITABLE HOTEL:")
      println(s"   ► Hotel: ${mostProfitable._1}")
      println(s"   ► City: ${mostProfitable._2}")
      println(s"   ► Country: ${mostProfitable._3}")
      println(f"   ► Combined Score: ${mostProfitable._4 * 100}%.2f")
      println(s"   ► Total Visitors: ${mostProfitable._7}")
      println(f"   ► Average Profit Margin: ${mostProfitable._8 * 100}%.1f%%")
      println(s"   ► Total Bookings: ${mostProfitable._9}")

      // Show score breakdown
      println(s"\n SCORE CALCULATION:")
      println(f"   • Visitor Score:       ${mostProfitable._5 * 100}%.2f (${mostProfitable._7} visitors)")
      println(f"   • Profit Margin Score: ${mostProfitable._6 * 100}%.2f (${mostProfitable._8 * 100}%.1f%%)")
      println(f"   • Combined Score:      ${mostProfitable._4 * 100}%.2f")

      // Show normalization ranges
      println(s"\n NORMALIZATION RANGES:")
      println(s"   • Visitors: $minVisitors to $maxVisitors")
      println(f"   • Profit Margin: ${minMargin * 100}%.1f%% to ${maxMargin * 100}%.1f%%")

      // Top hotels bar chart (show hotel name only)
      val topProfitableHotels = hotelsWithScores
        .sortBy(-_._4)
        .take(8)
        .map { case (hotel, city, country, score, _, _, visitors, margin, _) =>
          // Create display label with hotel and location
          val location = if (city.nonEmpty && country.nonEmpty) s" ($city)" else ""
          val displayName = if (hotel.length > 20) hotel.take(17) + "..." + location else hotel + location
          (displayName, score * 100)
        }

      barChart("TOP PROFITABLE HOTELS (Combined Score)", topProfitableHotels)

      // Simple statistics
      val totalVisitorsAll = parsedWithLocation.map(_._1.visitors).sum
      val avgMarginAll = parsedWithLocation.map(_._1.profitMargin).sum / parsedWithLocation.size * 100

      println(s"\n OVERVIEW:")
      println(s"   • Total Visitors Analyzed: $totalVisitorsAll")
      println(f"   • Average Profit Margin: $avgMarginAll%.1f%%")
      println(s"   • Total Hotel Locations Analyzed: ${hotelMetrics.size}")

    } else {
      println("No valid hotel data found for profitability analysis")
    }
  }

  override def showStatistics(bookings: List[HotelBooking]): Unit = {
    // Empty implementation
  }
}