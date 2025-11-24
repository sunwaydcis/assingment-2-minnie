object DataValidator {
  def validateHotelData(bookings: List[HotelBooking]): ValidationReport = {
    val total = bookings.size
    if (total == 0) return ValidationReport(List(), 0.0)

    val checks = List(
      ("Positive Prices", bookings.count(_.bookingPrice > 0), total),
      ("Reasonable Prices", bookings.count(b => b.bookingPrice > 10 && b.bookingPrice < 1000), total),
      ("Valid Discounts", bookings.count(b => b.discount >= 0 && b.discount <= 100), total),
      ("Positive Visitors", bookings.count(_.visitors > 0), total),
      ("Reasonable Profit Margins", bookings.count(b => b.profitMargin >= 0 && b.profitMargin <= 1), total)
    )

    ValidationReport(checks)
  }

  case class ValidationReport(checks: List[(String, Int, Int)], overallScore: Double = 0.0) {
    def printReport(): Unit = {
      if (checks.isEmpty) {
        println("No data to validate")
        return
      }

      println(s"\n🔍 DATA VALIDATION REPORT")
      println("─" * 40)
      checks.foreach { case (check, passed, total) =>
        val percentage = (passed.toDouble / total) * 100
        val status = if (percentage > 90) "✅" else if (percentage > 70) "⚠️" else "❌"
        println(f"$status $check: $passed/$total ($percentage%.1f%%)")
      }
      val overall = checks.map { case (_, passed, total) => passed.toDouble / total }.sum / checks.size * 100
      println(f"📊 Overall Data Quality: $overall%.1f%%")
    }
  }
}
