object SequenceTutorial{
	
	def main(args:Array[String]){
	
	//All List operatino valid for Vector as well as for Array,Strings and ranges
	val a = Vector(1,2,3,4,5,6,7)
	println(a)
	println("Prepand ->"+(a:+12) +"\nAppend ->"+(12+:a))
	println("Head ->"+(a.head) +"\nTail ->"+(a.tail))
	println("Map ->"+(a map (x=>x*x)))
	/*xs exists p true if there is an element x of xs such that p(x) holds,false otherwise.
	xs forall p true if p(x) holds for all elements x of xs, false otherwise.
    xs zip ys A sequence of pairs drawn from corresponding elements of sequences xs and ys.
	xs.unzip Splits a sequence of pairs xs into two sequences consisting of the first, respectively second halves of all pairs.
	xs.flatMap f Applies collection-valued function f to all elements of
	xs and concatenates the results
	xs.sum The sum of all elements of this numeric collection.
	xs.product The product of all elements of this numeric collection
	xs.max The maximum of all elements of this collection (an Ordering must exist)
	xs.min The minimum of all elements of this collection */
	
	println(a flatMap (x=>List("."+x*x+".")))
	println(prime(7))
	println(prime(14))
	
	
	//Ranges are stored as lower bound, upper bound and step
	val r1 = 1 to 8
	val r2 = 1 until 8
	val r3 = 1 to 8 by 2
	val r4 = 1 until 8 by 2
	println(r1.toList)
	println(r2.toList)
	println(r3.toList)
	println(r4.toList)
	
	}
	
	def prime(a:Int):Boolean = (2 until a) forall (d=>(a%d!=0))
}
