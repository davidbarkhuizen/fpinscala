package fpinscala.examples

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

	def apply[A](as: A*): List[A] =
		if (as.isEmpty) Nil
		else Cons(as.head, apply(as.tail: _*))

	def sum(ints: List[Int]): Int = ints match {
		case Nil => 0
		case Cons(x, xs) => x + List.sum(xs)
	}

	def init[A](l: List[A]) : List[A] =

		l match {
			case Nil => Nil
			case Cons(_, Nil) => Nil
		    case Cons(h, t) => Cons(h, List.init(t))
		}
}

object Module {

	def main(args : Array[String]) : Unit = {
		
		val l = List(1,2,3,4,5)
		println(l)
		val everythingButTheLastElement = List.init(l)
		println(everythingButTheLastElement)
	}
}