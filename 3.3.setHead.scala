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

	def setHead[A](l: XList[A], newHead: A) : XList[A] = 
		l match {
			case Nil => Nil
			case Cons(_, t) => Cons(newHead, t)
		}
}

object Module {

	def main(args : Array[String]) : Unit = {
		
		val l = XList(1,2,3,4,5)
		println(l)
		val newList = XList.setHead(l, 5)
		println(newList)
	}
}