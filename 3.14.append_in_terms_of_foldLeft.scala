package fpinscala.examples

object Module {

	
	def appendViaFoldLeft[A](a1: List[A], a2: List[A]): List[A] =		
		
		// def foldLeft[B](z: B)(f: (B, A) => B): B
		//
		a2.foldLeft(a1.reverse)((bAcc, a) => a::bAcc).reverse

	def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] =		
		
		// foldRight[B](z: B)(op: (A, B) ⇒ B): B
		//
		a1.foldRight(a2)((x,l) =>  x::l)

	def main(args : Array[String]) : Unit = {
		
		val l1 = List(1,2)
		val l2 = List(3,4)

		val viaLeft = appendViaFoldLeft(l1, l2)
		println("viaLeft")
		println(viaLeft)

		val viaRight = appendViaFoldRight(l1, l2)
		println("viaRight")
		println(viaRight)
	}
}