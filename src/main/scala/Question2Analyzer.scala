import DataUtils._
import ChartUtils._

object Question2Analyzer extends Analyzer[Question2Analyzer.HotelWithLocation] {
  def label = "Most Economical Hotel Analysis"

  case class HotelWithLocation(
                                hotel: String,
                                city: String,
                                country: String,
                                bookingPrice: Double,
                                discount: Double,
                                profitMargin: Double,
                                visitors: Int
                              )

  def parse(row: String, header: Array[String]): Option[HotelWithLocation] = {
    val cols = splitRow(row)

    for {
      hotel <- safeGet(cols, header.indexOf("Hotel Name"))
      destinationCity <- safeGet(cols, header.indexOf("Destination City"))
      destinationCountry <- safeGet(cols, header.indexOf("Destination Country"))
      price <- safeDouble(cols, header.indexOf("Booking Price[SGD]"))
      discountStr <- safeGet(cols, header.indexOf("Discount"))
      margin <- safeDouble(cols, header.indexOf("Profit Margin"))
      visitors <- safeInt(cols, header.indexOf("No. Of People"))
    }
    yield HotelWithLocation(
      hotel,
      destinationCity,
      destinationCountry,
      price,
      parseDiscount(discountStr),
      margin,
      visitors
    )
  }

  def analyze(rows: List[String], header: Array[String]): Unit = {
    val parsed = rows.flatMap(parse(_, header))

    if (parsed.nonEmpty) {
      // Group by hotel + city + country
      val hotelStats = parsed.groupBy(h => (h.hotel, h.city, h.country)).map {
        case ((hotel, city, country), bookings) =>
          val avgPrice = bookings.map(_.bookingPrice).sum / bookings.size
          val avgDiscount = bookings.map(_.discount).sum / bookings.size
          val avgProfitMargin = bookings.map(_.profitMargin).sum / bookings.size
          val transactionCount = bookings.size

          (hotel, city, country, avgPrice, avgDiscount, avgProfitMargin, transactionCount)
      }.toList

      if (hotelStats.nonEmpty) {
        // Extract lists for normalization
        val prices = hotelStats.map(_._4)
        val discounts = hotelStats.map(_._5)
        val margins = hotelStats.map(_._6)

        // Find min and max for each of the criteria
        val minPrice = prices.min
        val maxPrice = prices.max
        val minDiscount = discounts.min
        val maxDiscount = discounts.max
        val minMargin = margins.min
        val maxMargin = margins.max

        // Calculate normalized scores for each hotel location
        val hotelScores = hotelStats.map {
          case (hotel, city, country, avgPrice, avgDiscount, avgProfitMargin, count) =>

            // Normalize price: lower price = higher score
            val priceScore = if (maxPrice - minPrice > 0)
              (1 - ((avgPrice - minPrice) / (maxPrice - minPrice))) * 100
            else 50.0

            // Normalize discount: higher discount = higher score
            val discountScore = if (maxDiscount - minDiscount > 0)
              ((avgDiscount - minDiscount) / (maxDiscount - minDiscount)) * 100
            else 50.0

            // Normalize profit margin: lower margin = higher score
            val marginScore = if (maxMargin - minMargin > 0)
              (1 - ((avgProfitMargin - minMargin) / (maxMargin - minMargin))) * 100
            else 50.0

            // Composite score: average of three normalized scores
            val compositeScore = (priceScore + discountScore + marginScore) / 3.0

            (hotel, city, country, avgPrice, avgDiscount, avgProfitMargin, count, compositeScore)
        }

        // Find hotel location with the highest composite score
        val (bestHotel, bestCity, bestCountry, bestAvgPrice,
        bestAvgDiscount, bestAvgMargin, bestCount, bestCompositeScore) =
          hotelScores.maxBy(_._8)

        println("\n 2. MOST ECONOMICAL HOTEL")
        println(s"   ► Hotel: $bestHotel")
        println(s"   ► City: $bestCity")
        println(s"   ► Country: $bestCountry")
        println(f"   ► Final Score: $bestCompositeScore%.2f")
        println(f"   ► Average Price: $$$bestAvgPrice%.2f")
        println(f"   ► Average Discount: $bestAvgDiscount%.1f%%")
        println(f"   ► Average Profit Margin: ${bestAvgMargin*100}%.1f%%")
        println(s"   ► Transactions: $bestCount")

        // Show top hotels by composite score
        val topHotels = hotelScores
          .sortBy(-_._8)
          .take(10)
          .map { case (hotel, city, country, _, _, _, _, score) =>
            (s"$hotel - $city - $country", score)
          }

        barChart("TOP MOST ECONOMICAL HOTELS", topHotels)

        showStatistics(parsed)
      }
    } else {
      println("No valid hotel data found for analysis")
    }
  }

  override def showStatistics(bookings: List[HotelWithLocation]): Unit = {
    val avgPrice = bookings.map(_.bookingPrice).sum / bookings.size
    val avgDiscount = bookings.map(_.discount).sum / bookings.size
    val avgMargin = bookings.map(_.profitMargin).sum / bookings.size

    val uniqueHotels = bookings.map(h => (h.hotel, h.city, h.country)).toSet.size

    println(s"\n Overall Statistics:")
    println(f"   • Overall average price: $$$avgPrice%.2f")
    println(f"   • Overall average discount: $avgDiscount%.1f%%")
    println(f"   • Overall average profit margin: ${avgMargin*100}%.1f%%")
    println(s"   • Unique hotel locations: $uniqueHotels")
  }
}