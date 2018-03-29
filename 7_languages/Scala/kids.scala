import akka.actor._
import akka.actor.Actor._

case object Poke
case object Feed

class Kid() extends Actor {
	def act() {
		loop {
			react {
				case Poke => {
					println("Ow...")
					println("Quit it...")
				}
				case Feed => {
					println("Gurgle...")
					println("Burp...")
				}
			}
		}
	}
}

val bart = new Kid().start
val lisa = new Kid().start
println("Ready to poke and feed...")
bart ! Poke
list ! Poke
bart ! Feed
lisa ! Feed
