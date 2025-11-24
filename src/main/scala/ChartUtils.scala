object ChartUtils {
  def barChart(title: String, data: List[(String, Double)], maxBars: Int = 8): Unit = {
    if (data.isEmpty) return

    val maxValue = data.map(_._2).max
    val maxLabelLength = data.map(_._1.length).max

    println(s"\n📊 $title")
    println("─" * 60)

    data.take(maxBars).foreach { case (label, value) =>
      val barLength = if (maxValue > 0) ((value / maxValue) * 40).toInt else 0
      val bar = "█" * barLength
      val percentage = if (maxValue > 0) f"${(value / maxValue) * 100}%.1f%%" else "0%"

      println(f"${label.padTo(maxLabelLength, ' ')} | $bar%-40s $value%.2f ($percentage)")
    }
  }

  def progressBar(value: Double, max: Double, length: Int = 30): String = {
    val progress = ((value / max) * length).toInt
    "█" * progress + "░" * (length - progress)
  }
}