import scala.math.Ordering

object listTutorial {
	def main (args:Array[String]){
	
		//LIST FLATTEN
		println(listFlatten(List(List(1, 1), 2, List(3, List(5, 8)))))
		
		//MergeSort using List
		println (msort(List(5,4,3,5,3,4,6,12,3,1,-10,-1,666,6,7,2,1)))
		
		//MergeSort using Pair
	    println (msortpair(List(5,4,3,5,3,4,6,12,3,1,-10,-1,666,6,7,2,1)))
		
		//Genric MergeSort using Type T
		println (genricmsortpair(List(5,4,3,5,3,4,6,12,3,1,-10,-1,666,6,7,2,1))( (a:Int,b:Int)=>a<b))
		println (genricmsortpair(List('a','b','a','b','a','b','a'))( (a:Char,b:Char)=>a<b))
		
		//Genric MergeSort using scala.math.Ordering
		println (genricorderingmsortpair(List(5,4,3,5,3,4,6,12,3,1,-10,-1,666,6,7,2,1))(Ordering[Int]))
		println (genricorderingmsortpair(List('a','b','a','b','a','b','a'))(Ordering[Char]))
		
		//Genric MergeSort using scala.math.Ordering and implicit keyword
		println (genricorderingmsortpair(List(5,4,3,5,3,4,6,12,3,1,-10,-1,666,6,7,2,1))(Ordering[Int]))
		println (genricorderingmsortpair(List('a','b','a','b','a','b','a'))(Ordering[Char]))
		
		//Map each element by a function 
		println (msortpair(List(5,4,3,5,3,4,6,12,3,1,666,6,7,2,1)) map (x=>x*x))           		//squaring a list using map
		  
		//Filter each element of a list on aparticular condition
		println (msortpair(List(5,4,3,5,3,5,4,6,12,3,1,666,6,7,2,1)) filter (x=> (x>10) && (x<50)))
		println (msortpair(List(5,4,3,5,3,4,5,6,12,3,1,666,6,7,2,1)) filterNot (x=> (x>10) && (x<50)))
		println (msortpair(List(5,4,3,5,5,3,4,6,12,3,1,666,6,7,2,1)) takeWhile (x=> (x%2) > 0 ))
		println (msortpair(List(5,4,3,5,5,3,4,6,12,3,1,666,6,7,2,1)) dropWhile (x=> (x%2) > 0 ))
		println (msortpair(List(5,4,3,5,5,3,4,6,12,3,1,666,6,7,2,1)) span (x=> (x%2) > 0 ))      //pair of takeWhile and dropWhile
		println (msortpair(List(5,4,3,5,5,3,4,6,12,3,1,666,6,7,2,1)) partition (x=> (x%2) > 0 )) //pair of filter and filterNot 
		println (pack(List('a', 'a', 'a', 'b', 'c', 'c', 'a')))
		println (encode(List('a', 'a', 'a', 'b', 'c', 'c', 'a')))
		
		//Reduce 
		        //You can transfer a fold into an infix operator notation (writing in between):
				//This example fold using the accumulator function x
				//fold x [A, B, C, D]
				//thus equals
				//A x B x C x D
				//Now you just have to reason about the associativity of your operator (by putting parentheses!).
				//If you have a left-associative operator, you'll set the parentheses like this
				//((A x B) x C) x D
				//Here, you use a left fold. Example (haskell-style pseudocode)
				//foldl (-) [1, 2, 3] == (1 - 2) - 3 == 1 - 2 - 3 // - is left-associative
				//If your operator is right-associative (right fold), the parentheses would be set like this:
				//A x (B x (C x D))
				//Example: Cons-Operator
				//foldr (:) [] [1, 2, 3] == 1 : (2 : (3 : [])) == 1 : 2 : 3 : [] == [1, 2, 3]
				//In general, arithmetic operators (most operators) are left-associative, so foldl is more widespread. But in the other cases, infix notation + parentheses is quite useful.
		val a = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
		val b = List('a','b', 'c', 'd')
        //println((b foldLeft a)((a,b)=>a::b))
        //Error value :: is not a member of Char
        //(b foldLeft a)((a,b)=>a::b)
		println((b foldRight a)((a,b)=>a::b))
		
	   //Sort group
	   val fruit = List("apple","pear","orange","pine-apple")
	   println(fruit sortWith (_.length > _.length))
	   println( fruit groupBy (_.head))
	   println (fruit.sorted)
	   
	

	}  
		
	def listTutorialBasic={
		val rx=List(1,2,3,4,5,6)
		val tx='a'::'b'::'c'::'d'::'e'::'f'::Nil
		println(rx)
		println(tx)
		println(rx::tx)
		println(rx:::tx)
		println(rx.head+" "+rx.tail+" "+rx.length+" "+rx.init+" "+rx.last+" "+rx.take(2)+" "+rx.drop(2))
		println(rx ++ tx+" " +rx.reverse+" "+rx.updated(3,'a')+" "+rx.contains(4)+" "+rx.indexOf(5))
		
	}
	
	def listFlatten(list:List[Any]):List[Any]= list match {
		case List()=>List()
		case x::xs =>if (x.isInstanceOf[List[Any]]) listFlatten(x.asInstanceOf[List[Any]]):::listFlatten(xs) else  x::listFlatten(xs)
	}
	
	def merge(l:List[Int],r:List[Int]):List[Int]={
		l match {
			case Nil=>r
			case x::xs=>r match {
				case Nil=>l
				case y::ys=> if(x<y) x::merge(xs,r)
				              else   y::merge(l,ys) 
			          }
				 }
	}
	
	def msort(list:List[Int]):List[Int]= {
	     val n=list.length/2
	     if(n==0) list
	     else { val (l,r) = list.splitAt(n);  merge(msort(l),msort(r)) }
	}
	
	def mergepair(l:List[Int],r:List[Int]):List[Int]= (l,r) match {
		case (l,Nil) => l
		case (Nil,r) => r
		case (l,r)   => if(l.head < r.head) l.head::mergepair(l.tail,r)
						else  r.head::mergepair(l,r.tail)
	}
	
	def msortpair(list:List[Int]):List[Int]= {
	     val n=list.length/2
	     if(n==0) list
	     else { val (l,r) = list.splitAt(n);  mergepair(msortpair(l),msortpair(r)) }
	}
	
	def genricmergepair[T](l:List[T],r:List[T])(f:(T,T)=>Boolean):List[T]= (l,r) match {
		case (l,Nil) => l
		case (Nil,r) => r
		case (l,r)   => if (f(l.head,r.head)) l.head::genricmergepair(l.tail,r)(f)
						else  r.head::genricmergepair(l,r.tail)(f)
	}
	
	def genricmsortpair[T](list:List[T])(f:(T,T)=>Boolean):List[T]= {
	     val n=list.length/2
	     if(n==0) list
	     else { val (l,r) = list.splitAt(n);  genricmergepair(genricmsortpair(l)(f),genricmsortpair(r)(f))(f) }
	}
	
	def genricorderingmergepair[T](l:List[T],r:List[T])(ord:Ordering[T]):List[T]= (l,r) match {
		case (l,Nil) => l
		case (Nil,r) => r
		case (l,r)   => if (ord.lt(l.head,r.head)) l.head::genricorderingmergepair(l.tail,r)(ord)
						else  r.head::genricorderingmergepair(l,r.tail)(ord)
	}
	
	def genricorderingmsortpair[T](list:List[T])(ord:Ordering[T]):List[T]= {
	     val n=list.length/2
	     if(n==0) list
	     else { val (l,r) = list.splitAt(n);  genricorderingmergepair(genricorderingmsortpair(l)(ord),genricorderingmsortpair(r)(ord))(ord) }
	}
	def explicitgenricorderingmergepair[T](l:List[T],r:List[T])(implicit ord:Ordering[T]):List[T]= (l,r) match {
		case (l,Nil) => l
		case (Nil,r) => r
		case (l,r)   => if (ord.lt(l.head,r.head)) l.head::explicitgenricorderingmergepair(l.tail,r)
						else  r.head::explicitgenricorderingmergepair(l,r.tail)
	}
	
	def explicitgenricorderingmsortpair[T](list:List[T])(implicit ord:Ordering[T]):List[T]= {
	     val n=list.length/2
	     if(n==0) list
	     else { val (l,r) = list.splitAt(n);  genricorderingmergepair(explicitgenricorderingmsortpair(l),explicitgenricorderingmsortpair(r))(ord) }
	}
	
	def pack(list:List[Char]):List[List[Char]] = {
		 val (a,b) = list span (x=> x==list.head)
		 if (b==Nil) List(a)
		 else a::pack(b)
	}
	
	def encode(list:List[Char]):List[(Char,Int)]=pack(list) map (x=>(x.head,x.length))
	
}

