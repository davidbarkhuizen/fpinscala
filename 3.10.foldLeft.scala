package fpinscala.examples

object Module {

	def foldLeftBad[A,B](as: List[A], z: B)(f: (B, A) => B): B = {

		@annotation.tailrec		
		def loop(n: Int, acc: B) : B = 

			if (n >= as.length)			acc
			else						loop(n + 1, f(acc, as(n)))

		loop(0, z)
	}

	// not tail recursive
	// pushes a new frame onto the stack for each element
	// => danger of stack overflow
	//
	def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B =

		l match {
			case Nil => z
			case x::xs => f(x, foldRight(xs, z)(f))
		}

	@annotation.tailrec
	def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = 

		l match {
			case Nil 	=> z
			case h::t 	=> foldLeft(t, f(z,h))(f)
		}

	def main(args : Array[String]) : Unit = {
		
		val resultBad = foldLeftBad(List(1,2,3), 0)((x, acc) => acc + x)
		println(resultBad)

		val resultGood = foldLeftGood(List(1,2,3), 0)((x, acc) => acc + x)
		println(resultGood)
	}
}