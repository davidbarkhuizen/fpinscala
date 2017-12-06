import scala.annotation.tailrec

sealed trait XList[+A]
case object Nil extends XList[Nothing]
case class Cons[+A](head: A, tail: XList[A]) extends XList[A]

object XList {

	def apply[A](as: A*): XList[A] =
		if (as.isEmpty) Nil
		else Cons(as.head, apply(as.tail: _*))

	def sum(ints: XList[Int]): Int = ints match {
		case Nil => 0
		case Cons(x, xs) => x + XList.sum(xs)
	}

	def init[A](l: XList[A]) : XList[A] =

		l match {
			case Nil => Nil
			case Cons(_, Nil) => Nil
		    case Cons(h, t) => Cons(h, XList.init(t))
		}
}

object Module {

	def main(args : Array[String]) : Unit = {
		
		val l = XList(1,2,3,4,5)
		println(l)
		val everythingButTheLastElement = XList.init(l)
		println(everythingButTheLastElement)
	}
}