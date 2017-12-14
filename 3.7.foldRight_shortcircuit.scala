package fpinscala.examples

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

	def apply[A](as: A*): List[A] =
		if (as.isEmpty) Nil
		else Cons(as.head, apply(as.tail: _*))
}

object Module {

	def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B, haltIf: A => Boolean, onHalt: B): B =

		as match {
			case Nil => z
			case Cons(x, xs) if (haltIf(x) == true) => onHalt
			case Cons(x, xs) => { 
				println(x)
				f(x, foldRight(xs, z)(f, haltIf, onHalt))
			}
		}

	def product2(ns: List[Double]) = 
		foldRight(ns, 1.0)(_ * _, (x) => x == 0, 0)

	def main(args : Array[String]) : Unit = {
		
		val l = List(1.0, 2.0, 3.0, 4.0, 5.0)
		val product = product2(l)
		println(product)

		val lHalt = List(1.0, 0, 3.0, 4.0, 5.0)
		val haltProduct = product2(lHalt)
		println(haltProduct)
	}
}