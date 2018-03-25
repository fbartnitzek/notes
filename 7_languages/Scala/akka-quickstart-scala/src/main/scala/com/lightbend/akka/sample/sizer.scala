package com.lightbend.akka.sample

import akka.actor.{ Actor, ActorLogging, ActorRef, ActorSystem, Props }
import scala.io._

object PageLoader {
	def getPageSize(url: String) = Source.fromURL(url)("ISO-8859-1").mkString.length
}

class SizerActor() extends Actor {
	def receive = {
		case (url, size) =>
			println("Size for " + url + ": " + size)
	}
}

object Sizer extends App {
	val urls = List( "https://www.twitter.com/",
					"https://www.google.com/",
					"https://www.cnn.com/" )

	def timeMethod(method: () => Unit) = {
		val start = System.nanoTime
		method()
		val end = System.nanoTime
		println("Method took " + (end - start)/1000000000.0 + " seconds.")
	}

	def getPageSizeSequentially() = {
		for(url <- urls){
			println("Size for " + url + ": " + PageLoader.getPageSize(url))
		}
	}

	def getPageSizeConcurrently() = {
		val system = ActorSystem("testSystem")
		val sizer = system.actorOf(Props[SizerActor], "sizer-actor")
		for(url <- urls){
			sizer ! (url, PageLoader.getPageSize(url)) 
		}
	}

	println("Sequential run:")
	timeMethod { getPageSizeSequentially }

	println("Concurrent run:")
	timeMethod { getPageSizeConcurrently }
}


