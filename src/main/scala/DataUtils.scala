//DataUtils.scala
import scala.util.Try

//Utility object for data parsing and validation
object DataUtils {
  //Splits CSV row while handling quoted fields with commas
  //Uses regular expressions to only split on commas outside quotes
  def splitRow(row: String): Array[String] =
    row.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)")
      .map(_.trim.stripPrefix("\"").stripSuffix("\""))

  //Safely get column value by index, returns Option [String]
  def safeGet(cols: Array[String], index: Int): Option[String] =
    if (index >= 0 && index < cols.length)
      Option(cols(index)).filter(_.nonEmpty) //Only return non-empty values
    else None

  //Safely parse column to Int, returns Option [Int]
  def safeDouble(cols: Array[String], index: Int): Option[Double] =
    safeGet(cols, index).flatMap(s => Try(s.toDouble).toOption) //Try parsing, convert to Option

  //Parse discount string that could be percentage or decimal
  def safeInt(cols: Array[String], index: Int): Option[Int] =
    safeGet(cols, index).flatMap(s => Try(s.toInt).toOption)

  def parseDiscount(discountStr: String): Double = {
    Try {
      if (discountStr.contains("%")) {
        discountStr.replace("%", "").toDouble //Remove % and parse
      } else {
        discountStr.toDouble
      }
    }.getOrElse(0.0) //Return 0.0 if parsing fails
  }
}