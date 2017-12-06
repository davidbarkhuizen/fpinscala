import scala.annotation.tailrec

object Module {

	def fib(n : Int) : Int = {

		@tailrec		
		def loop(m : Int, aleph: Int, beth: Int) : Int = {

			val next = aleph + beth
			
			if (m >= n - 2)		next
			else				loop(m+1, beth, next)
		}


		loop(0, 1, 0)
	}

	def main(args : Array[String]) : Unit = {
		println(fib(1))
	}
}