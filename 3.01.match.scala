
package fpinscala.examples

// sealed => all implementations of this trait must be within this physical file
//
// A+ = variance notation 
// => A is a positive/covariant parameter of List
// => List[B] is regarded as a subtype of List[A] if B is a subtype of A
//
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

// List companion object
//
object List {

	// variadic function (varargs)
	//
	// https://en.wikipedia.org/wiki/Variadic_function
	// = a variadic function is a function of indefinite arity, i.e., one which accepts a variable number of arguments

	// apply
	//
	// https://en.wikipedia.org/wiki/Apply
	// = apply is a function that applies functions to arguments
	//
	// define function
	// (x:Int) => x + 1
	//
	// assign fn reference to variable
	// val f = (x:Int) => x + 1
	//
	// execute function
	// f(1)
	// or
	// f.apply(1) 
	//
	def apply[A](as: A*): List[A] =
		if (as.isEmpty) Nil
		else Cons(as.head, apply(as.tail: _*))

	def sum(ints: List[Int]): Int = ints match {
		case Nil => 0
		case Cons(x, xs) => x + List.sum(xs)
	}
}

object Module {

	def main(args : Array[String]) : Unit = {
		
		val x = List(1,2,3,4,5) match {
			case Cons(x, Cons(2, Cons(4, _))) => x
			case Nil => 42
			case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
			case Cons(h, t) => h + List.sum(t)
			case _ => 101
		}

		println(x)
	}
}