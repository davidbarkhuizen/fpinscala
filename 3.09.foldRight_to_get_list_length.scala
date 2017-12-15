package fpinscala.examples

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

	def apply[A](as: A*): List[A] =
		if (as.isEmpty) 	Nil
		else 				Cons(as.head, apply(as.tail: _*))
}

object Module {

	def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =

		as match {
			case Nil => z
			case Cons(x, xs) => f(x, foldRight(xs, z)(f))
		}

	def main(args : Array[String]) : Unit = {
		
		val result = foldRight(List(1,2,3), 0)((x, acc) => acc + x)
		println(result)
	}
}