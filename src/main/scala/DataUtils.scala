import scala.util.Try

object DataUtils {
  def splitRow(row: String): Array[String] =
    row.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)")
      .map(_.trim.stripPrefix("\"").stripSuffix("\""))

  def safeGet(cols: Array[String], index: Int): Option[String] =
    if (index >= 0 && index < cols.length)
      Option(cols(index)).filter(_.nonEmpty)
    else None

  def safeDouble(cols: Array[String], index: Int): Option[Double] =
    safeGet(cols, index).flatMap(s => Try(s.toDouble).toOption)

  def safeInt(cols: Array[String], index: Int): Option[Int] =
    safeGet(cols, index).flatMap(s => Try(s.toInt).toOption)

  def parseDiscount(discountStr: String): Double = {
    Try {
      if (discountStr.contains("%")) {
        discountStr.replace("%", "").toDouble
      } else {
        discountStr.toDouble
      }
    }.getOrElse(0.0)
  }
}