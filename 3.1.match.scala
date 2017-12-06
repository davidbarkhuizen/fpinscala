// sealed => all implementations of this trait must be within this physical file
//
// A+ = variance notation 
// => A is a positive/covariant parameter of XList
// => XList[B] is regarded as a subtype of XList[A] if B is a subtype of A
//
sealed trait XList[+A]
case object Nil extends XList[Nothing]
case class Cons[+A](head: A, tail: XList[A]) extends XList[A]

// XList companion object
//
object XList {

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
	def apply[A](as: A*): XList[A] =
		if (as.isEmpty) Nil
		else Cons(as.head, apply(as.tail: _*))

	def sum(ints: XList[Int]): Int = ints match {
		case Nil => 0
		case Cons(x, xs) => x + XList.sum(xs)
	}
}

object Module {

	def main(args : Array[String]) : Unit = {
		
		val x = XList(1,2,3,4,5) match {
			case Cons(x, Cons(2, Cons(4, _))) => x
			case Nil => 42
			case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
			case Cons(h, t) => h + XList.sum(t)
			case _ => 101
		}

		println(x)
	}
}