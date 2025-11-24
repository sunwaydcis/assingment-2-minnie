trait Analyzer[T] {
  def label: String
  def parse(row: String, header: Array[String]): Option[T]
  def analyze(rows: List[String], header: Array[String]): Unit
  def showStatistics(data: List[T]): Unit = {} // Default empty implementation
}
