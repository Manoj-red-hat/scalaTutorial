object merge {
	def main (args:Array[String]){
		val rx=List(1,2,3,4,5,6)
		val tx='a'::'b'::'c'::'d'::'e'::'f'::Nil
		//println(rx)
		//println(tx)
		//println(rx::tx)
		//println(rx:::tx)
		//println(rx.head+" "+rx.tail+" "+rx.length+" "+rx.init+" "+rx.last+" "+rx.take(2)+" "+rx.drop(2))
		//println(rx ++ tx+" " +rx.reverse+" "+rx.updated(3,'a')+" "+rx.contains(4)+" "+rx.indexOf(5))
		
		//LIST FLATTEN
		//println(listFlatten(List(List(1, 1), 2, List(3, List(5, 8)))))
		//println(listFlatten(List(List(3,List(5, 8)))))
		//println(listFlatten(List(List(List('a',1)))))
		//print (msort(List(5,4,3,5,3,4,6,12,3,1,-10,-1,666,6,7,2,1)))
		print (msort(List(3,4,6,12,3,1,-10)))
		//Merge
		

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
				case y::ys=> if(x<y) l:::r 
				              else   r:::l 
			          }
				 }
	}
	def msort(list:List[Int]):List[Int]= {
	     val n=list.length/2
	     if(n==0) list
	     else { val (l,r) = list.splitAt(n);  merge(msort(l),msort(r)) }
	}
}

