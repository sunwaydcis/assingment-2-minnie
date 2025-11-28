//Question2Analyzer.scala
import DataUtils._
import ChartUtils._

//Analyzer for finding most economical hotels by different criteria
object Question2Analyzer extends Analyzer[HotelBooking] {
  def label = "Most Economical Hotel Analysis"

  //Parse CSV row into HotelBooking object
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

  //Main analysis logic for question 2
  def analyze(rows: List[String], header: Array[String]): Unit = {
    val parsed = rows.flatMap(parse(_, header))

    if (parsed.nonEmpty) {
      println("2. MOST ECONOMICAL HOTELS BY DIFFERENT CRITERIA")

      //Calculate correlation between price and discount
      val correlation = calculatePriceDiscountCorrelation(parsed)

      //Find cheapest hotel by booking price
      val cheapestHotel = parsed.minBy(_.bookingPrice)
      println("2a. BY BOOKING PRICE - Most economical hotel:")
      println(s"   ► Hotel: ${cheapestHotel.hotel}")
      println(s"   ► Price: $${cheapestHotel.bookingPrice}")

      //Find hotel with highest discount
      val highestDiscountHotel = parsed.maxBy(_.discount)
      println("2b. BY DISCOUNT - Most economical hotel:")
      println(s"   ► Hotel: ${highestDiscountHotel.hotel}")
      println(s"   ► Discount: ${highestDiscountHotel.discount}%")

      //Find hotel with lowest profit margin (most competitve pricing)
      val lowestMarginHotel = parsed.minBy(_.profitMargin)
      println("2c. BY PROFIT MARGIN - Most economical hotel:")
      println(s"   ► Hotel: ${lowestMarginHotel.hotel}")
      println(s"   ► Profit Margin: ${(lowestMarginHotel.profitMargin * 100).formatted("%.1f")}%")

      //Statistical insights
      println(s"2d. STATISTICAL INSIGHT:")
      println(f"   ► Price-Discount Correlation: $correlation%.3f")
      //Interpret correlation coefficient
      val correlationInterpretation = if (correlation < -0.3) "Strong negative"
      else if (correlation < -0.1) "Weak negative"
      else if (correlation > 0.3) "Strong positive"
      else if (correlation > 0.1) "Weak positive"
      else "No correlation"
      println(s"   ► Interpretation: $correlationInterpretation relationship")

      //Calculate value scores for all hotels
      val hotelsWithValue = parsed.map { hotel =>
        val valueScore = calculateValueScore(hotel)
        (hotel, valueScore)
      }

      //Get top 8 best value hotels
      val bestValueHotels = hotelsWithValue
        .sortBy(-_._2)
        .take(8)
        .map { case (hotel, score) => (hotel.hotel, score) }

      barChart("BEST VALUE HOTELS (Overall Score)", bestValueHotels)
      showStatistics(parsed)
    } else {
      println("No valid hotel data found for analysis")
    }
  }

  //Calculate comprehensive value score considering multiple factors
  private def calculateValueScore(hotel: HotelBooking): Double = {
    val normalizedPrice = 1.0 / (hotel.bookingPrice / 100) // Lower price = higher score
    val normalizedDiscount = hotel.discount / 10 // Higher discount = higher score
    val normalizedMargin = (1.0 - hotel.profitMargin) * 10 // Lower margin = higher score
    val popularity = math.log(hotel.visitors + 1) // More visitors = higher score

    //weighted combination of factors
    (normalizedPrice * 0.4) + (normalizedDiscount * 0.3) +
      (normalizedMargin * 0.2) + (popularity * 0.1)
  }
  //Calculate Pearson correlation coefficient between price and discount
  private def calculatePriceDiscountCorrelation(bookings: List[HotelBooking]): Double = {
    val prices = bookings.map(_.bookingPrice)
    val discounts = bookings.map(_.discount)

    val avgPrice = prices.sum / prices.size
    val avgDiscount = discounts.sum / discounts.size

    //Calculate covariance
    val covariance = bookings.map(b =>
      (b.bookingPrice - avgPrice) * (b.discount - avgDiscount)
    ).sum / bookings.size

    //Calculate standard deviations
    val priceStdDev = Math.sqrt(prices.map(p => Math.pow(p - avgPrice, 2)).sum / prices.size)
    val discountStdDev = Math.sqrt(discounts.map(d => Math.pow(d - avgDiscount, 2)).sum / discounts.size)

    //Calculate correlation coefficient
    if (priceStdDev * discountStdDev == 0) 0.0
    else covariance / (priceStdDev * discountStdDev)
  }

  //Display price analysis statistics
  override def showStatistics(bookings: List[HotelBooking]): Unit = {
    val avgPrice = bookings.map(_.bookingPrice).sum / bookings.size
    val avgDiscount = bookings.map(_.discount).sum / bookings.size
    val priceStdDev = Math.sqrt(
      bookings.map(b => Math.pow(b.bookingPrice - avgPrice, 2)).sum / bookings.size
    )

    println(s"\nPrice Analysis:")
    println(f"   • Average booking price: $$$avgPrice%.2f")
    println(f"   • Average discount: $avgDiscount%.1f%%")
    println(f"   • Price standard deviation: $$$priceStdDev%.2f")

    //Categorize hotels by price range
    val budgetHotels = bookings.count(_.bookingPrice < 100)
    val luxuryHotels = bookings.count(_.bookingPrice > 300)
    println(s"   • Budget hotels (<$$100): $budgetHotels")
    println(s"   • Luxury hotels (>$$300): $luxuryHotels")
  }
}