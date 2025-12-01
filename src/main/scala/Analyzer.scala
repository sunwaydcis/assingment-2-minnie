//Generic trait defining the interface for all data analyzer
//Demonstrates polymorphism through type parameter T.
trait Analyzer[T] { //T is the type of data being analyzed
  def label: String //Display name for this analyzer
  def parse(row: String, header: Array[String]): Option[T]
  def analyze(rows: List[String], header: Array[String]): Unit //Parse CSV row into domain object
  def showStatistics(data: List[T]): Unit = {} // Default empty implementation for statistics display
}
