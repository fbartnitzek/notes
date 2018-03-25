import java.io._
import scala.io.Source

object FileDemo {
	def main(args: Array[String]){
		val writer = new PrintWriter(new File("text.txt"))

		writer.write("Hello Scala")
		writer.close()

		//println("Following is the content read:")
		//Source.fromFile("test.txt").foreach{
		//	print
		//}
	}
}
