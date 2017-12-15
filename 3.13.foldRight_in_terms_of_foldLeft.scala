package fpinscala.examples

object Module {

	@annotation.tailrec
	def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = 

		l match {
			case Nil 	=> z
			case h::t 	=> foldLeft(t, f(z,h))(f)
		}

	def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B =
		foldLeft(l.reverse, z)( (b,a) => f(a,b) )

	def main(args : Array[String]) : Unit = {
		
		val l = List(1,2,3,4)

		// TODO

		val printFromRightDiscard = foldRight(l, Nil:List[Int])((x, l) => { println(x); Nil:List[Int] })
	}
}