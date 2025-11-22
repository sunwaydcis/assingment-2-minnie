package myfirstscala

import scala.io.Source
import scala.util.{Try, Using}

case class HotelBooking(
  hotel: String,
  country: String,
  bookingPrice: Double,
  discount: Double,
  profitMargin: Double,
  visitors: Int
)

object HotelAnalysis {
  
  def main(args: Array[String]): Unit = {
    println("=== Hotel Booking Data Analysis ===\n")
    
    // Load the actual dataset
    val bookings = loadDataset("Hotel_Dataset.csv")
  }
}