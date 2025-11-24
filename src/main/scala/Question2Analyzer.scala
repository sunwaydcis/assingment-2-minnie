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

  def analyze(rows: List[String], header: Array[String]): Unit = {
    val parsed = rows.flatMap(parse(_, header))

    if (parsed.nonEmpty) {
      println("2. 💰 MOST ECONOMICAL HOTELS BY DIFFERENT CRITERIA")

      val correlation = calculatePriceDiscountCorrelation(parsed)

      val cheapestHotel = parsed.minBy(_.bookingPrice)
      println("2a. 💵 BY BOOKING PRICE - Most economical hotel:")
      println(s"   ► Hotel: ${cheapestHotel.hotel}")
      println(s"   ► Price: $${cheapestHotel.bookingPrice}")

      val highestDiscountHotel = parsed.maxBy(_.discount)
      println("2b. 🎯 BY DISCOUNT - Most economical hotel:")
      println(s"   ► Hotel: ${highestDiscountHotel.hotel}")
      println(s"   ► Discount: ${highestDiscountHotel.discount}%")

      val lowestMarginHotel = parsed.minBy(_.profitMargin)
      println("2c. 📊 BY PROFIT MARGIN - Most economical hotel:")
      println(s"   ► Hotel: ${lowestMarginHotel.hotel}")
      println(s"   ► Profit Margin: ${(lowestMarginHotel.profitMargin * 100).formatted("%.1f")}%")

      println(s"2d. 📈 STATISTICAL INSIGHT:")
      println(f"   ► Price-Discount Correlation: $correlation%.3f")
      val correlationInterpretation = if (correlation < -0.3) "Strong negative"
      else if (correlation < -0.1) "Weak negative"
      else if (correlation > 0.3) "Strong positive"
      else if (correlation > 0.1) "Weak positive"
      else "No correlation"
      println(s"   ► Interpretation: $correlationInterpretation relationship")

      val hotelsWithValue = parsed.map { hotel =>
        val valueScore = calculateValueScore(hotel)
        (hotel, valueScore)
      }

      val bestValueHotels = hotelsWithValue
        .sortBy(-_._2)
        .take(8)
        .map { case (hotel, score) => (hotel.hotel, score) }

      barChart("🏅 BEST VALUE HOTELS (Overall Score)", bestValueHotels)
      showStatistics(parsed)
    } else {
      println("No valid hotel data found for analysis")
    }
  }

  private def calculateValueScore(hotel: HotelBooking): Double = {
    val normalizedPrice = 1.0 / (hotel.bookingPrice / 100) // Lower price = higher score
    val normalizedDiscount = hotel.discount / 10 // Higher discount = higher score
    val normalizedMargin = (1.0 - hotel.profitMargin) * 10 // Lower margin = higher score
    val popularity = math.log(hotel.visitors + 1) // More visitors = higher score

    (normalizedPrice * 0.4) + (normalizedDiscount * 0.3) +
      (normalizedMargin * 0.2) + (popularity * 0.1)
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
    val avgPrice = bookings.map(_.bookingPrice).sum / bookings.size
    val avgDiscount = bookings.map(_.discount).sum / bookings.size
    val priceStdDev = Math.sqrt(
      bookings.map(b => Math.pow(b.bookingPrice - avgPrice, 2)).sum / bookings.size
    )

    println(s"\n📈 Price Analysis:")
    println(f"   • Average booking price: $$$avgPrice%.2f")
    println(f"   • Average discount: $avgDiscount%.1f%%")
    println(f"   • Price standard deviation: $$$priceStdDev%.2f")

    val budgetHotels = bookings.count(_.bookingPrice < 100)
    val luxuryHotels = bookings.count(_.bookingPrice > 300)
    println(s"   • Budget hotels (<$$100): $budgetHotels")
    println(s"   • Luxury hotels (>$$300): $luxuryHotels")
  }
}