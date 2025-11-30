//Case class representing a single hotel booking record
case class HotelBooking (
  hotel: String, //Hotel name
  country: String, //Country of origin
  bookingPrice: Double, // Price in SGD
  discount: Double, //Discount percentage
  profitMargin: Double, //Profit margin (0-1)
  visitors: Int //Number of people
)

//Case class representing aggregated statistics for a hotel
case class HotelStats(
  hotel: String, //Hotel name
  totalBookings: Int, //Total number of bookings
  totalVisitors: Int, //Total number of visitors
  averagePrice: Double, //Average booking price
  averageDiscount: Double, //Average discount
  averageProfitMargin: Double //Average profit margin
)

//Case class representing aggregated statistics for a country
case class CountryStats(
  country: String, //Country name
  totalBookings: Int, //Total bookings from country
  totalVisitors: Int, //Total visitors from country
  averageBookingPrice: Double, //Average booking price
  mostPopularHotel: String //Most booked hotel for this country
)
