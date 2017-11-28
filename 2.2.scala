import scala.annotation.tailrec

object Module {

	def isSorted[A](as: Array[A], isOrdered: (A, A) => Boolean) : Boolean = {

		@tailrec		
		def loop(n: Int) : Boolean = 

			if (n + 1 >= as.length)					true
			else if (!isOrdered(as(n), as(n + 1)))	false
			else 									loop(n + 1)

		loop(0)
	}

	def main(args : Array[String]) : Unit = {

		val isOrdered = (first:Int,next:Int) => first <= next

		val itemsNotSorted = Array(1,2,3,1,4,5)
	
		println(isSorted(itemsNotSorted, isOrdered))
		println(isSorted(itemsNotSorted.sorted, isOrdered))
	}
}