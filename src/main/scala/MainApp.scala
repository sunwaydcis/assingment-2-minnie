import scala.io.Source
import java.nio.file.Paths
import java.io.File

object MainApp {
  def main(args: Array[String]): Unit = {
    println("🏨 === ADVANCED HOTEL BOOKING DATA ANALYSIS === 🏨\n")

    val file = findDatasetFile()

    if (file.exists()) {
      analyzeFile(file)
    } else {
      println("❌ Hotel_Dataset.csv not found")
      println("Please make sure the file is in one of these locations:")
      println("   • src/main/resources/Hotel_Dataset.csv")
      println("   • Project root folder (same as build.sbt)")
      println("   • Current working directory")
    }
  }

  private def findDatasetFile(): File = {
    val resourceUrl = getClass.getResource("/Hotel_Dataset.csv")
    if (resourceUrl != null) {
      return new File(resourceUrl.toURI)
    }

    val rootFile = new File("Hotel_Dataset.csv")
    if (rootFile.exists()) return rootFile

    val currentDirFile = new File("./Hotel_Dataset.csv")
    if (currentDirFile.exists()) return currentDirFile

    new File("Hotel_Dataset.csv")
  }

  private def analyzeFile(file: File): Unit = {
    try {
      println(s"📁 Reading file: ${file.getAbsolutePath}")
      println(s"📊 File size: ${file.length()} bytes")

      val lines = try {
        Source.fromFile(file, "UTF-8").getLines().toList
      } catch {
        case _: Exception =>
          Source.fromFile(file, "ISO-8859-1").getLines().toList
      }

      analyzeData(lines)
    } catch {
      case e: Exception =>
        println(s"❌ Error reading file: ${e.getMessage}")
        e.printStackTrace() // This will show the full error
        println("\n🔧 Troubleshooting tips:")
        println("   • Check if the file is a valid CSV (open in text editor)")
        println("   • Try saving the file with UTF-8 encoding")
        println("   • Ensure the file is not corrupted")
        println("   • Check file permissions")
    }
  }

  private def analyzeData(lines: List[String]): Unit = {
    if (lines.isEmpty) {
      println("Dataset file is empty or could not be read")
      return
    }

    println(s"✅ Successfully read ${lines.size} lines")

    // Check if we have a header and data
    if (lines.size < 2) {
      println("❌ File doesn't contain enough data (need header + at least 1 row)")
      return
    }

    val header = lines.head.split(",").map(_.trim)
    val rows = lines.tail

    println(s"📈 Header columns: ${header.mkString(", ")}")
    println(s"📊 Data rows: ${rows.size}")

    // Test parsing a few rows to check data format
    val sampleParsed = rows.take(5).flatMap(Question1Analyzer.parse(_, header))
    println(s"🔍 Successfully parsed ${sampleParsed.size} sample rows")

    if (sampleParsed.nonEmpty) {

      val allParsed = rows.flatMap(Question1Analyzer.parse(_, header))
      DataValidator.validateHotelData(allParsed).printReport()

      println("🎯 Starting full analysis...\n")

      val analyzers: List[Analyzer[_]] = List(
        Question1Analyzer,
        Question2Analyzer,
        Question3Analyzer
      )

      analyzers.zipWithIndex.foreach { case (analyzer, index) =>
        println("\n" + "═" * 80)
        println(s"🔍 QUESTION ${index + 1}")
        println("═" * 80)
        println(s"📋 ${analyzer.label.toUpperCase}")
        println("─" * 50)
        analyzer.analyze(rows, header)
      }

      println("\n" + "🎉" * 40)
      println("ANALYSIS COMPLETED SUCCESSFULLY!")
      println("🎉" * 40)

      showFinalSummary(rows, header)
    } else {
      println("❌ Could not parse any data rows. Please check CSV format.")
      println("💡 Expected columns: Hotel Name, Origin Country, Booking Price[SGD], Discount, Profit Margin, No. Of People")
    }
  }

  private def showFinalSummary(rows: List[String], header: Array[String]): Unit = {
    val bookings = rows.flatMap(Question1Analyzer.parse(_, header))
    if (bookings.nonEmpty) {
      println(s"\n📈 FINAL SUMMARY")
      println("─" * 30)
      println(s"• Total bookings analyzed: ${bookings.size}")
      println(s"• Unique hotels: ${bookings.map(_.hotel).toSet.size}")
      println(s"• Unique countries: ${bookings.map(_.country).toSet.size}")
      println(f"• Total revenue: $$${bookings.map(_.bookingPrice).sum}%.2f")
      println(f"• Average discount: ${bookings.map(_.discount).sum / bookings.size}%.1f%%")

      val biasHotels = Question3Analyzer.calculateBiasProfitability(bookings).size
      val nonBiasHotels = Question3Analyzer.calculateNonBiasProfitability(bookings).size
      println(s"• High-quality hotels (bias analysis): $biasHotels")
      println(s"• All hotels (non-bias analysis): $nonBiasHotels")
    }
  }
}


