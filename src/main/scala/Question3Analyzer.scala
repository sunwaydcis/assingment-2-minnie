//Question3Analyzer.scala
import DataUtils._
import ChartUtils._

//Analyzer for finding most profitable hotels with bias and non-bias approaches
object Question3Analyzer extends Analyzer[HotelBooking] {
  def label = "Most Profitable Hotel Analysis"

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

  //Main analysis logic for question 3
  def analyze(rows: List[String], header: Array[String]): Unit = {
    val parsed = rows.flatMap(parse(_, header))

    if (parsed.nonEmpty) {
      //Calculated profitability using both approaches
      val biasResults = calculateBiasProfitability(parsed)
      val nonBiasResults = calculateNonBiasProfitability(parsed)

      println("3. 🚀 MOST PROFITABLE HOTELS")

      //Show bias analysis results (high-quality data only)
      if (biasResults.nonEmpty) {
        val (biasHotel, biasScore) = biasResults.maxBy(_._2)
        println("   BIAS VERSION (High-quality data only):")
        println(s"   ► Hotel: $biasHotel")
        println(s"   ► Profitability Score: ${"%.2f".format(biasScore)}")
        println(s"   ► Data Quality: ${getDataQuality(parsed, biasHotel)}")
      }

      //Show non-bias analysis results (all data with confidence)
      val (nonBiasHotel, nonBiasScore) = nonBiasResults.maxBy(_._2)
      println("   NON-BIAS VERSION (All data with confidence):")
      println(s"   ► Hotel: $nonBiasHotel")
      println(s"   ► Profitability Score: ${"%.2f".format(nonBiasScore)}")
      println(s"   ► Confidence Level: ${getConfidenceLevel(parsed, nonBiasHotel)}")

      barChart("TOP PROFITABLE HOTELS (Non-Bias)", nonBiasResults.take(8))
      showStatistics(parsed)
    } else {
      println("No valid hotel data found for profitability analysis")
    }
  }

  //Calculate profitability with bias toward high-quality data
  //Only includes hotels with sufficient data for reliable analysis
  def calculateBiasProfitability(bookings: List[HotelBooking]): List[(String, Double)] = {
    bookings.groupBy(_.hotel)
      .filter { case (_, hotelBookings) =>
        hotelBookings.size >= 5 && // Minimum 5 bookings for reliability
        hotelBookings.map(_.visitors).sum >= 20 // Minimum 20 visitors
      }

      .map { case (hotel, hotelBookings) =>
        val totalVisitors = hotelBookings.map(_.visitors).sum
        val avgProfitMargin = hotelBookings.map(_.profitMargin).sum / hotelBookings.size
        //Profitability score = total visitors x average profit margin
        (hotel, totalVisitors * avgProfitMargin)
      }.toList
  }

  //Calculate profitability without bias, using confidence weighting
  //Includes all hotels but weights by data reliability
  def calculateNonBiasProfitability(bookings: List[HotelBooking]): List[(String, Double)] = {
    bookings.groupBy(_.hotel).map { case (hotel, hotelBookings) =>
      val totalVisitors = hotelBookings.map(_.visitors).sum
      val avgProfitMargin = hotelBookings.map(_.profitMargin).sum / hotelBookings.size
      val sampleSize = hotelBookings.size
      // Confidence based on sample size (like senior's data quality weighting)
      val confidence = math.min(sampleSize / 10.0, 1.0)
      //Profitability score weighted by confidence
      (hotel, totalVisitors * avgProfitMargin * confidence)
    }.toList
  }

  //Determine data quality level based on sample size
  private def getDataQuality(bookings: List[HotelBooking], hotel: String): String = {
    val hotelBookings = bookings.filter(_.hotel == hotel)
    val sampleSize = hotelBookings.size
    if (sampleSize >= 10) "Excellent"
    else if (sampleSize >= 5) "Good"
    else "Limited"
  }

  //Convert data quality to confidence percentage
  private def getConfidenceLevel(bookings: List[HotelBooking], hotel: String): String = {
    val quality = getDataQuality(bookings, hotel)
    quality match {
      case "Excellent" => "95%"
      case "Good" => "80%"
      case "Limited" => "60%"
    }
  }

  //Display profitability insights and statistics
  override def showStatistics(bookings: List[HotelBooking]): Unit = {
    //Calculate total estimated profit
    val totalProfit = bookings.map(b => b.bookingPrice * b.profitMargin).sum
    val avgMargin = bookings.map(_.profitMargin).sum / bookings.size
    val totalVisitors = bookings.map(_.visitors).sum

    println(s"\n📈 Profitability Insights:")
    println(f"   • Total estimated profit: $$$totalProfit%.2f")
    println(f"   • Average profit margin: ${avgMargin * 100}%.1f%%")
    println(s"   • Total visitors: $totalVisitors")

    //Show analysis coverage
    val biasResults = calculateBiasProfitability(bookings)
    val nonBiasResults = calculateNonBiasProfitability(bookings)
    println(s"   • Hotels in bias analysis: ${biasResults.size}")
    println(s"   • Hotels in non-bias analysis: ${nonBiasResults.size}")

    //Calculate revenue per visitor if data available
    if (totalVisitors > 0) {
      println(f"   • Revenue per visitor: $$${totalProfit / totalVisitors}%.2f")
    }
  }
}