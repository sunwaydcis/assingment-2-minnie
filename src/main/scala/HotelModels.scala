case class HotelBooking (
  hotel: String,
  country: String,
  bookingPrice: Double,
  discount: Double,
  profitMargin: Double,
  visitors: Int
)

case class HotelStats(
  hotel: String,
  totalBookings: Int,
  totalVisitors: Int,
  averagePrice: Double,
  averageDiscount: Double,
  averageProfitMargin: Double
)


case class CountryStats(
  country: String,
  totalBookings: Int,
  totalVisitors: Int,
  averageBookingPrice: Double,
  mostPopularHotel: String
)
