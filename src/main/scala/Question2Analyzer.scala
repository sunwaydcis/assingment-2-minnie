//Question2Analyzer.scala - with lecturer's method
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
      println("2. MOST ECONOMICAL HOTEL (Combined Criteria)")

      // Group by hotel and calculate averages for each hotel
      val hotelGroups = parsed.groupBy(_.hotel)

      val hotelMetrics = hotelGroups.map { case (hotelName, bookings) =>
        val avgPrice = bookings.map(_.bookingPrice).sum / bookings.size
        val avgDiscount = bookings.map(_.discount).sum / bookings.size
        val avgProfitMargin = bookings.map(_.profitMargin).sum / bookings.size
        val totalBookings = bookings.size

        (hotelName, avgPrice, avgDiscount, avgProfitMargin, totalBookings)
      }.toList

      // Get min and max values for normalization
      val prices = hotelMetrics.map(_._2)
      val discounts = hotelMetrics.map(_._3)
      val margins = hotelMetrics.map(_._4)

      val minPrice = prices.min
      val maxPrice = prices.max
      val minDiscount = discounts.min
      val maxDiscount = discounts.max
      val minMargin = margins.min
      val maxMargin = margins.max

      // Calculate normalized scores for each hotel
      val hotelsWithScores = hotelMetrics.map { case (hotel, price, discount, margin, bookings) =>
        // 1. Price: LOWER is better, so invert the normalized score
        // Normalized price: (price - min) / (max - min) → higher = more expensive
        // Economical price score: 1 - normalized_price → higher = more economical
        val normalizedPrice = (price - minPrice) / (maxPrice - minPrice)
        val priceScore = 1 - normalizedPrice  // Invert so lower price = higher score

        // 2. Discount: HIGHER is better
        // Normalized discount: (discount - min) / (max - min)
        val discountScore = (discount - minDiscount) / (maxDiscount - minDiscount)

        // 3. Profit Margin: LOWER is better (hotel keeps less profit), so invert
        val normalizedMargin = (margin - minMargin) / (maxMargin - minMargin)
        val marginScore = 1 - normalizedMargin  // Invert so lower margin = higher score

        // Combine scores (equal weight for all three criteria)
        val totalScore = (priceScore + discountScore + marginScore) / 3

        (hotel, totalScore, price, discount, margin, bookings, priceScore, discountScore, marginScore)
      }

      // Find hotel with the highest total score (most economical)
      val mostEconomical = hotelsWithScores.maxBy(_._2)

      println(s"   ► Most Economical Hotel: ${mostEconomical._1}")
      println(f"   ► Average Booking Price: $$${mostEconomical._3}%.2f")
      println(f"   ► Average Discount: ${mostEconomical._4}%.1f%%")
      println(f"   ► Average Profit Margin: ${mostEconomical._5 * 100}%.1f%%")
      println(s"   ► Based on ${mostEconomical._6} bookings")
      println(f"   ► Combined Economical Score: ${mostEconomical._2}%.3f")

      // Show detailed score breakdown
      println(s"\n   SCORE BREAKDOWN:")
      println(f"   • Price Score:        ${mostEconomical._7}%.3f (lower price = higher score)")
      println(f"   • Discount Score:     ${mostEconomical._8}%.3f (higher discount = higher score)")
      println(f"   • Profit Margin Score: ${mostEconomical._9}%.3f (lower margin = higher score)")

      // Prepare top 8 hotels for bar chart
      val topEconomicalHotels = hotelsWithScores
        .sortBy(-_._2)  // Sort by total score descending
        .take(8)
        .map { case (hotel, score, price, discount, margin, bookings, _, _, _) =>
          val shortName = if (hotel.length > 20) hotel.take(17) + "..." else hotel
          (shortName, score * 100)  // Multiply by 100 for better bar chart display
        }

      // Display bar chart
      barChart("TOP ECONOMICAL HOTELS (Combined Score)", topEconomicalHotels)

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

  override def showStatistics(bookings: List[HotelBooking]): Unit = {
    // Empty implementation
  }
}