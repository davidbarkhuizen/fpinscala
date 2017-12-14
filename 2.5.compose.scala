package fpinscala.examples

object Module {

	def compose[A, B, C](f: B => C, g: A => B) : A => C = {

		def h(a: A): C = 
			f(g(a))

		h
	}

	def g(c: Char): String = 
		c.toString()

	def f(s: String): Int = 
		s.toInt

	def main(args : Array[String]) : Unit = {
		
		val composed = compose(f, g)

		val i = composed('7')
		println(i)
	}
}