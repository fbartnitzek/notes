import scala.io.Source

object FileDemo2 {
	def main(args: Array[String]){
		val fileName = "text.txt"
		println("Following is the content read for " + fileName  + ":")

		Source.fromFile(fileName).foreach{
			print
		}
	}
}
