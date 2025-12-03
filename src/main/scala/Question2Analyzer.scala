import DataUtils._
import ChartUtils._

object Question2Analyzer extends Analyzer[HotelBooking] {
  def label = "Most Economical Hotel Analysis"

  def parse(row: String, header: Array[String]): Option[HotelBooking] = {
    val cols = splitRow(row)

    for {
      hotel <- safeGet(cols, header.indexOf("Hotel Name"))
      country <- safeGet(cols, header.indexOf("Origin Country"))
      price <- safeDouble(cols, header.indexOf("Booking Price[SGD]"))
      discountStr <- safeGet(cols, header.indexOf("Discount"))
      margin <- safeDouble(cols, header.indexOf("Profit Margin"))
      visitors <- safeInt(cols, header.indexOf("No. Of People"))
    } yield HotelBooking(
      hotel,
      country,
      price,
      parseDiscount(discountStr),
      margin,
      visitors
    )
  }

  def analyze(rows: List[String], header: Array[String]): Unit = {
    val parsed = rows.flatMap(parse(_, header))

    if (parsed.nonEmpty) {
      println("2. MOST ECONOMICAL HOTEL")

      // Group by hotel and calculate averages for each hotel
      val hotelGroups = parsed.groupBy(_.hotel)

      val hotelMetrics = hotelGroups.map { case (hotelName, bookings) =>
        val avgPrice = bookings.map(_.bookingPrice).sum / bookings.size
        val avgDiscount = bookings.map(_.discount).sum / bookings.size
        val avgProfitMargin = bookings.map(_.profitMargin).sum / bookings.size
        val totalBookings = bookings.size

        (hotelName, avgPrice, avgDiscount, avgProfitMargin, totalBookings)
      }.toList

      // Find hotel with the lowest average price (most economical)
      val mostEconomical = hotelMetrics.minBy(_._2)

      println(s"   ► Most Economical Hotel: ${mostEconomical._1}")
      println(f"   ► Average Booking Price: $$${mostEconomical._2}%.2f")
      println(f"   ► Average Discount: ${mostEconomical._3}%.1f%%")
      println(f"   ► Average Profit Margin: ${mostEconomical._4 * 100}%.1f%%")
      println(s"   ► Based on ${mostEconomical._5} bookings")

      // Show top cheapest hotels for comparison
      val cheapestHotels = hotelMetrics
        .sortBy(_._2)  // Sort by price ascending
        .take(10)
        .map { case (hotel, price, discount, margin, bookings) =>
          // Format hotel name for display
          val shortName = if (hotel.length > 20) hotel.take(17) + "..." else hotel
          (shortName, price)
        }

      // Display bar chart of cheapest hotels
      barChart("CHEAPEST HOTELS (Average Price)", cheapestHotels)

      // Statistical insights
      val correlation = calculatePriceDiscountCorrelation(parsed)
      println(s"\n   STATISTICAL INSIGHTS:")
      println(f"   • Price-Discount Correlation: $correlation%.3f")

      val correlationInterpretation = correlation match {
        case c if c < -0.5 => "Strong negative"
        case c if c < -0.3 => "Moderate negative"
        case c if c < -0.1 => "Weak negative"
        case c if c > 0.5 => "Strong positive"
        case c if c > 0.3 => "Moderate positive"
        case c if c > 0.1 => "Weak positive"
        case _ => "No correlation"
      }
      println(s"   • Interpretation: $correlationInterpretation relationship")
      println(s"   • Note: Negative correlation means cheaper hotels tend to offer higher discounts")

    } else {
      println("No valid hotel data found for analysis")
    }
  }

  private def calculatePriceDiscountCorrelation(bookings: List[HotelBooking]): Double = {
    val prices = bookings.map(_.bookingPrice)
    val discounts = bookings.map(_.discount)

    val avgPrice = prices.sum / prices.size
    val avgDiscount = discounts.sum / discounts.size

    val covariance = bookings.map(b =>
      (b.bookingPrice - avgPrice) * (b.discount - avgDiscount)
    ).sum / bookings.size

    val priceStdDev = Math.sqrt(prices.map(p => Math.pow(p - avgPrice, 2)).sum / prices.size)
    val discountStdDev = Math.sqrt(discounts.map(d => Math.pow(d - avgDiscount, 2)).sum / discounts.size)

    if (priceStdDev * discountStdDev == 0) 0.0
    else covariance / (priceStdDev * discountStdDev)
  }

  override def showStatistics(bookings: List[HotelBooking]): Unit= {
    }
}