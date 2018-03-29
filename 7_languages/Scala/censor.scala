trait Censor {
	// val curses = Map("Shoot" -> "Pucky", "Darn" -> "Beans")
	var curses = Map[String, String]()
	io.Source.fromFile("censor.config").getLines().foreach { line =>
		println("configLine: " + line)
		val words = line.split(":")
		curses += words(0) -> words(1)
	}

	def censor(origText: String) = curses.foldLeft(origText)((text, curseEntry) => 
										text.replaceAll(curseEntry._1, curseEntry._2))
}

class Text(val content: String) extends Censor {
	def censored() = this.censor(this.content)
}

val s = new Text("some text Shoot and Darn Darn")

println("original content:")
println(s.content)
println("censored:")
println(s.censored)
