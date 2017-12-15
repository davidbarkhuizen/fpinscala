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

	def dropWhile[A](l: List[A], predicate: A => Boolean) : List[A] =

		l match {
			case Nil => Nil
		    case Cons(h, t) => predicate(h) match {
		    	case false => l
		    	case true => List.dropWhile(t, predicate)
		    }
		}

	def groupedDropWhile[A](l: List[A])(predicate: A => Boolean) : List[A] =

		l match {
			case Nil => Nil
		    case Cons(h, t) => predicate(h) match {
		    	case false => l
		    	case true => List.groupedDropWhile(t)(predicate)
		    }
		}

}

object Module {

	def main(args : Array[String]) : Unit = {
		
		val l = List(1,2,3,4,5)
		println(l)
		val truncated = List.dropWhile(l, (x:Int) => x <= 3)
		println(truncated)

		val groupedTruncated = List.groupedDropWhile(l)((x) => x <= 3) 
		print(groupedTruncated)
	}
}