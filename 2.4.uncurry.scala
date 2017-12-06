object Module {
	
	def uncurry[A, B, C](f: A => (B => C)): (A, B) => C = {

		// takes a function that
		// 		takes an A
		//		returns a function that takes a B and returns a C
		// 
		// returns a function that
		//		take an A and a B
		//		returns a C

		def g(a: A, b: B): C = {
			f(a)(b)
		}

		g
	}

	def g(i: Int): (Char => String) = {

		def h(c: Char): String =
			i.toString() + c.toString()

		h
	}

	def main(args : Array[String]) : Unit = {

		var u = uncurry(g)
		var s = u(1, 'c')
		println(s)
	}
}