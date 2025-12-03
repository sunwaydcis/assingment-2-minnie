import scala.io.Source
import java.nio.file.Paths
import java.io.File

//Main application object - entry point of the program
object MainApp {
  //Main method - program entry point
  def main(args: Array[String]): Unit = {
    println("=== ADVANCED HOTEL BOOKING DATA ANALYSIS ===\n")

    val file = findDatasetFile() //Locate the dataset file

    if (file.exists()) {
      analyzeFile(file) //Analyse if file exists
    } else {
      //Show error message with helpful information
      println("Hotel_Dataset.csv not found")
      println("Please make sure the file is in one of these locations:")
      println("   • src/main/resources/Hotel_Dataset.csv")
      println("   • Project root folder (same as build.sbt)")
      println("   • Current working directory")
    }
  }

  //Find the dataset file in common locations
  private def findDatasetFile(): File = {
    //Try classpath resources first
    val resourceUrl = getClass.getResource("/Hotel_Dataset.csv")
    if (resourceUrl != null) {
      return new File(resourceUrl.toURI)
    }

    //Try project root
    val rootFile = new File("Hotel_Dataset.csv")
    if (rootFile.exists()) return rootFile

    //Try current directory
    val currentDirFile = new File("./Hotel_Dataset.csv")
    if (currentDirFile.exists()) return currentDirFile

    new File("Hotel_Dataset.csv") //Return non-existent file as last resort
  }

  //Analyze the found CSV file
  private def analyzeFile(file: File): Unit = {
    try {
      println(s"📁 Reading file: ${file.getAbsolutePath}")
      println(s"📊 File size: ${file.length()} bytes")

      //Try reading with UTF-8, fall back to ISO-8859-1 if needed
      val lines = try {
        Source.fromFile(file, "UTF-8").getLines().toList
      } catch {
        case _: Exception =>
          Source.fromFile(file, "ISO-8859-1").getLines().toList
      }

      analyzeData(lines) //Proceed with data analysis
    } catch {
      case e: Exception =>
        //Comprehensive error handling with troubleshooting tips
        println(s"Error reading file: ${e.getMessage}")
        e.printStackTrace() // This will show the full error
        println("\nTroubleshooting tips:")
        println("   • Check if the file is a valid CSV (open in text editor)")
        println("   • Try saving the file with UTF-8 encoding")
        println("   • Ensure the file is not corrupted")
        println("   • Check file permissions")
    }
  }

  //Main data analysis pipeline
  private def analyzeData(lines: List[String]): Unit = {
    if (lines.isEmpty) {
      println("Dataset file is empty or could not be read")
      return
    }

    println(s"✅ Successfully read ${lines.size} lines")

    // Check if we have a header and data
    if (lines.size < 2) {
      println("File doesn't contain enough data (need header + at least 1 row)")
      return
    }

    val header = lines.head.split(",").map(_.trim) //Extract column headers
    val rows = lines.tail //Data rows (excluding header)

    println(s"📈 Header columns: ${header.mkString(", ")}")
    println(s"📊 Data rows: ${rows.size}")

    // Test parsing a few rows to check data format
    val sampleParsed = rows.take(5).flatMap(Question1Analyzer.parse(_, header))
    println(s"Successfully parsed ${sampleParsed.size} sample rows")

    if (sampleParsed.nonEmpty) {
      //Parse all data and run validation
      val allParsed = rows.flatMap(Question1Analyzer.parse(_, header))
      DataValidator.validateHotelData(allParsed).printReport()

      println("Starting full analysis...\n")

      //Define all analyzers to run
      val analyzers: List[Analyzer[_]] = List(
        Question1Analyzer,
        Question2Analyzer,
        Question3Analyzer
      )

      //Run each analyzer with formatted output
      analyzers.zipWithIndex.foreach { case (analyzer, index) =>
        println("\n" + "═" * 80)
        println(s"QUESTION ${index + 1}")
        println("═" * 80)
        println(s"${analyzer.label.toUpperCase}")
        println("─" * 50)
        analyzer.analyze(rows, header)
      }

      //Completion message
      println("\n" + "-" * 40)
      println("ANALYSIS COMPLETED SUCCESSFULLY!")
      println("-" * 40)

      showFinalSummary(rows, header) //Show final summary
    } else {
      //Data parsing failed
      println("Could not parse any data rows. Please check CSV format.")
      println("Expected columns: Hotel Name, Origin Country, Booking Price[SGD], Discount, Profit Margin, No. Of People")
    }
  }

  //Display final summary of the analysis
  private def showFinalSummary(rows: List[String], header: Array[String]): Unit = {
    val bookings = rows.flatMap(Question1Analyzer.parse(_, header))
    if (bookings.nonEmpty) {
      println(s"\nFINAL SUMMARY")
      println("─" * 30)
      println(s"• Total bookings analyzed: ${bookings.size}")
      println(s"• Unique hotels: ${bookings.map(_.hotel).toSet.size}")
      println(s"• Unique countries: ${bookings.map(_.country).toSet.size}")
      val totalRevenue = bookings.map(_.bookingPrice).sum
      println(f"• Total revenue: $$$totalRevenue%.2f")
      val avgDiscount = bookings.map(_.discount).sum / bookings.size
      println(f"• Average discount: $avgDiscount%.1f%%")


      // Calculate and display overall profitability metrics
      val totalProfit = bookings.map(b => b.bookingPrice * b.profitMargin).sum
      println(f"• Total profit: $$$totalProfit%.2f")
      val avgProfitMargin = bookings.map(_.profitMargin).sum / bookings.size * 100
      println(f"• Average profit margin: $avgProfitMargin%.1f%%")
    }
  }
}