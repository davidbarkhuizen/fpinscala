package fpinscala.examples

object Module {

	def curry[A,B,C](f: (A,B) => C): A => (B => C) = {

		// return a function which
		// 		takes an A
		// 		returns a function that takes a B and returns a C

		def g(a: A): B => C = {

			def h(b: B): C =
				f(a,b)				

			h
		}

		g
	}

	def intCharToString(i: Int, c: Char): String =
		i.toString() + c.toString()

	def main(args : Array[String]) : Unit = {
		
		val takesAnInt = curry(intCharToString)
		val takesAChar = takesAnInt(1)

		val s = takesAChar('c')

		println(s)
	}
}