//DataValidator.scala

//Object for validating hotel booking data quality
object DataValidator {
  //Validates a list of hotel bookings and returns a validation report
  def validateHotelData(bookings: List[HotelBooking]): ValidationReport = {
    val total = bookings.size
    if (total == 0) return ValidationReport(List(), 0.0) //Early return for empty data

    //Define validation checks with descriptive names
    val checks = List(
      ("Positive Prices", bookings.count(_.bookingPrice > 0), total),
      ("Reasonable Prices", bookings.count(b => b.bookingPrice > 10 && b.bookingPrice < 1000), total),
      ("Valid Discounts", bookings.count(b => b.discount >= 0 && b.discount <= 100), total),
      ("Positive Visitors", bookings.count(_.visitors > 0), total),
      ("Reasonable Profit Margins", bookings.count(b => b.profitMargin >= 0 && b.profitMargin <= 1), total)
    )

    ValidationReport(checks) //Create report with all checks
  }

  //Case class to hold validation results
  case class ValidationReport(checks: List[(String, Int, Int)], overallScore: Double = 0.0) {
    //Print formatted validation report to console
    def printReport(): Unit = {
      if (checks.isEmpty) {
        println("No data to validate")
        return
      }

      println(s"\nDATA VALIDATION REPORT")
      println("─" * 40)
      //Print each check with status emoji base on percentage
      checks.foreach { case (check, passed, total) =>
        val percentage = (passed.toDouble / total) * 100
        val status = if (percentage > 90) "✅" else if (percentage > 70) "⚠️" else "❌"
        //Status indicators
        println(f"$status $check: $passed/$total ($percentage%.1f%%)")
      }

      //Calculate and print overall data quality score
      val overall = checks.map { case (_, passed, total) => passed.toDouble / total }.sum / checks.size * 100
      println(f"📊 Overall Data Quality: $overall%.1f%%")
    }
  }
}
