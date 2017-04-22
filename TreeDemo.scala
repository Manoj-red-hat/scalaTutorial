// A scala object which helps in undersatnding how to think,
// tree data structure in terms of functional programming 

object TreeDemo {
   def main(args: Array[String]) {
      var a = new Empty
      var t = (((a.incl(3)).incl(2)).incl(4)).incl(1)
      var z = (((a.incl(7)).incl(6)).incl(5)).incl(8)
      println(z)
      println(z.remove(6))
      println(t.union(z))
      println(t.union(z.remove(6)))
      println((t.union(z.remove(6))).printDescendingOrder)
     
   }
}

//Traits. Similar to interfaces in Java, traits are used to define object
// types by specifying the signature of the supported methods
trait Tree{
	//Remove a node fro tree
	def remove(n:Int):Tree
	//Include a node in tree
	def incl(n:Int):Tree
	//Join to trees
	def union(t:Tree):Tree
	//Print in Descending order
	def printDescendingOrder
}

//Empty denotes terminal node of a branch
class Empty extends Tree{
	override def toString:String = "."
	def incl(head:Int):Tree=new NonEmpty(head,new Empty,new Empty)
	def union(t:Tree):Tree=t 
	def remove(n:Int):Tree= throw new Error("empty")
	def printDescendingOrder=throw new Error("Descending")

}

//Non-Empty denotes a subtree of a tree
class NonEmpty(head:Int,left:Tree,right:Tree) extends Tree{
	override def toString:String = "{"+left+head+right+"}"
	
	def incl(el:Int):Tree={
		if(this.head >el) new NonEmpty(this.head,left.incl(el),right)
		else if (this.head < el) new NonEmpty(this.head,left,right.incl(el))
		else this
	}          

	def union(t:Tree):Tree={	
	  left union (right union (t.incl(head)))
	}
	
	def remove(n:Int):Tree={
		if (head==n) left.union(right)
		else if(head>n) ((left.remove(n)).union(right)).incl(head)
		else left.union(right.remove(n)).incl(head)
	}
	
	def printDescendingOrder={
		if (this.left.isInstanceOf[NonEmpty]) this.left.printDescendingOrder
		print(head + "->")
		if (this.right.isInstanceOf[NonEmpty]) this.right.printDescendingOrder
	}
	
}
