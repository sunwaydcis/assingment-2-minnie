import DataUtils._
import ChartUtils._

object Question3Analyzer extends Analyzer[HotelBooking] {
  def label = "Most Profitable Hotel Analysis"

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
      println("3. MOST PROFITABLE HOTEL")

      // Group bookings by hotel
      val hotelGroups = parsed.groupBy(_.hotel)

      // Calculate profitability metrics for each hotel
      val hotelProfitability = hotelGroups.map { case (hotelName, bookings) =>
        val totalVisitors = bookings.map(_.visitors).sum
        val totalRevenue = bookings.map(_.bookingPrice).sum
        val totalProfit = bookings.map(b => b.bookingPrice * b.profitMargin).sum
        val avgProfitMargin = bookings.map(_.profitMargin).sum / bookings.size
        val totalBookings = bookings.size

        // Profitability score: total profit (revenue × profit margin)
        // This considers both number of visitors (through revenue) and profit margin
        val profitabilityScore = totalProfit

        (hotelName, profitabilityScore, totalProfit, totalRevenue,
          totalVisitors, avgProfitMargin, totalBookings)
      }.toList

      // Find most profitable hotel
      val mostProfitable = hotelProfitability.maxBy(_._2)

      // Format currency values
      val formattedTotalProfit = f"${mostProfitable._3}%.2f"
      val formattedTotalRevenue = f"${mostProfitable._4}%.2f"

      println(s"   ► Most Profitable Hotel: ${mostProfitable._1}")
      println(s"   ► Total Profit: $$ $formattedTotalProfit")
      println(s"   ► Total Revenue: $$ $formattedTotalRevenue")
      println(s"   ► Total Visitors: ${mostProfitable._5}")
      println(f"   ► Average Profit Margin: ${mostProfitable._6 * 100}%.1f%%")
      println(s"   ► Total Bookings: ${mostProfitable._7}")

      // Calculate profit per visitor for insight
      val profitPerVisitor = mostProfitable._3 / mostProfitable._5
      println(f"   ► Profit per Visitor: $$ $profitPerVisitor%.2f")

      // Prepare top 8 most profitable hotels for bar chart
      val topProfitableHotels = hotelProfitability
        .sortBy(-_._2)  // Sort by profitability score descending
        .take(8)
        .map { case (hotel, score, profit, revenue, visitors, margin, bookings) =>
          // Shorten long hotel names for display
          val shortName = if (hotel.length > 18) hotel.take(15) + "..." else hotel
          (shortName, profit)  // Use actual profit for chart height
        }

      barChart("TOP PROFITABLE HOTELS (Total Profit)", topProfitableHotels)

      // Statistical insights
      showStatistics(parsed)

    } else {
      println("No valid hotel data found for profitability analysis")
    }
  }

  override def showStatistics(bookings: List[HotelBooking]): Unit = {
    // Calculate overall profitability statistics
    val totalProfit = bookings.map(b => b.bookingPrice * b.profitMargin).sum
    val totalRevenue = bookings.map(_.bookingPrice).sum
    val totalVisitors = bookings.map(_.visitors).sum
    val overallMargin = (totalProfit / totalRevenue) * 100

    println(s"\n   PROFITABILITY STATISTICS:")
    println(f"   • Total Industry Profit: $$ $totalProfit%.2f")
    println(f"   • Total Industry Revenue: $$ $totalRevenue%.2f")
    println(f"   • Overall Profit Margin: $overallMargin%.1f%%")
    println(s"   • Total Visitors: $totalVisitors")

    if (totalVisitors > 0) {
      val avgProfitPerVisitor = totalProfit / totalVisitors
      val avgRevenuePerVisitor = totalRevenue / totalVisitors
      println(f"   • Average Profit per Visitor: $$ $avgProfitPerVisitor%.2f")
      println(f"   • Average Revenue per Visitor: $$ $avgRevenuePerVisitor%.2f")
    }

    // Show top 3 hotels by different metrics
    val hotelGroups = bookings.groupBy(_.hotel)

    // Top by total profit
    val topByProfit = hotelGroups.map { case (hotel, hotelBookings) =>
      val profit = hotelBookings.map(b => b.bookingPrice * b.profitMargin).sum
      (hotel, profit)
    }.toList.sortBy(-_._2).take(3)

    println("\n   TOP 3 BY PROFIT:")
    topByProfit.zipWithIndex.foreach { case ((hotel, profit), index) =>
      println(f"   ${index + 1}. $hotel: $$ $profit%.2f")
    }

    // Top by profit margin
    val topByMargin = hotelGroups.map { case (hotel, hotelBookings) =>
      val margin = hotelBookings.map(_.profitMargin).sum / hotelBookings.size
      (hotel, margin * 100)
    }.toList.sortBy(-_._2).take(3)

    println("\n   TOP 3 BY PROFIT MARGIN:")
    topByMargin.zipWithIndex.foreach { case ((hotel, margin), index) =>
      println(f"   ${index + 1}. $hotel: $margin%.1f%%")
    }

    // Top by visitors
    val topByVisitors = hotelGroups.map { case (hotel, hotelBookings) =>
      val visitors = hotelBookings.map(_.visitors).sum
      (hotel, visitors)
    }.toList.sortBy(-_._2).take(3)

    println("\n   TOP 3 BY VISITORS:")
    topByVisitors.zipWithIndex.foreach { case ((hotel, visitors), index) =>
      println(f"   ${index + 1}. $hotel: $visitors visitors")
    }
  }
}