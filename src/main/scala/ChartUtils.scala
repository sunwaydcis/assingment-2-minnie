//Utility object for creating console-based charts and visualizations
object ChartUtils {
  //Create a bar chart in the console
  //title:Chart title
  //data: list of (label, value) pairs to display
  //maxBars: Maximum number of bars to show
  def barChart(title: String, data: List[(String, Double)], maxBars: Int = 8): Unit = {
    if (data.isEmpty) return //Early return if no data

    val maxValue = data.map(_._2).max //Find maximum value for scaling
    val maxLabelLength = data.map(_._1.length).max //Find longest label for padding

    println(s"\n$title") //Chart title with emoji
    println("─" * 60) //Horizontal line for visual separation

    //display each bar
    data.take(maxBars).foreach { case (label, value) =>
      val barLength = if (maxValue > 0) ((value / maxValue) * 40).toInt else 0 //calculate bar length (0-40)
      val bar = "█" * barLength //Create bar using block characters
      val percentage = if (maxValue > 0) f"${(value / maxValue) * 100}%.1f%%" else "0%" //Calculate percentage

      println(f"${label.padTo(maxLabelLength, ' ')} | $bar%-40s $value%.2f ($percentage)")
    }
  }

  //Creates a progress bar string
  // value: current value
  // max: Maximum value
  //length: length of the progress bar in characters
  def progressBar(value: Double, max: Double, length: Int = 30): String = {
    val progress = ((value / max) * length).toInt //Calculate progress length
    "█" * progress + "░" * (length - progress) //Filled + empty parts
  }
}