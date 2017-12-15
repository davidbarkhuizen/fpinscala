package fpinscala.examples

object Module {

	def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B =

		l match {
			case Nil => z
			case x::xs => f(x, foldRight(xs, z)(f))
		}

	def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
		foldRight(l.reverse, z)( (a,b) => f(b,a)  )

	def main(args : Array[String]) : Unit = {
		
		val l = List(1,2,3,4)

		val reversedL = foldLeft(l, Nil:List[Int])((x, l) => l::x)
		println(reversedL)
	}
}