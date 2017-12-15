package fpinscala.examples

object Module {

	@annotation.tailrec
	def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = 

		l match {
			case Nil 	=> z
			case h::t 	=> foldLeft(t, f(z,h))(f)
		}

	def main(args : Array[String]) : Unit = {
		
		val l = List(1,2,3,4)

		val reversedL = foldLeft(l, Nil:List[Int])((x, l) => l::x)
		println(reversedL)
	}
}