package fpinscala.examples

object Module {

	
	def flatMap[A](ll: List[List[A]]): List[A] =
		
		// def foldLeft[B](z: B)(f: (B, A) => B): B
		//
		ll.foldLeft(Nil:List[A])( (acc, l) => acc:::l  )	
		
	def main(args : Array[String]) : Unit = {
		
		val l1 = List(1,2)
		val l2 = List(3,4)

		val ll = List(l1, l2)

		val l = flatMap(ll)

		println(l)
	}
}