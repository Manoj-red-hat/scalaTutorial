/*
 * MapTutorial.scala
 * 
 * Copyright 2017 Manoj Kumar <enigma@linux-a9b1.suse>
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301, USA.
 * 
 * 
 */

import scala.collection.mutable.{Map => MuMap} //Mutable Map import
import scala.collection.immutable.{TreeMap => TMap} //Mutable Map import

object MapTutorial {
	
	def main (args:Array[String]){
		
		// There are two types of Map 
		// 1) Class scala.collection.immutable.Map 
		// 2) Class scala.collection.mutable map,need to import explicitly
		
		val mutableMap=MuMap(1->'a',2->'c',3->'d',4->'e',5->'f')
		val imMap=Map(1->'a',2->'c',3->'d',4->'e',5->'f')
		val treeMap=TMap(1->'a',2->'c',3->'d',4->'e',5->'f')
		
		println("Mutable Map ## "+mutableMap.mkString("::"))
		println("Immutable Map ## "+imMap.mkString("::"))
		
		// Pattern Matching on Maps
		def isExist(a:Int)=imMap.get(a) match {
			case Some(b) => println("Value corresponding to key "+a+" is "+b)
			case None    => println("Value Not found")
		}
		isExist(4)
		isExist(10)
		
		//Map on Map
		imMap map {case(a,b)=>if(a%2==0) println("Value corresponding to key "+a+" is "+b)}
		println(imMap map {case(a,b)=>if(a%2==0) (a+10->b) else (a->b)})
		
		//Filter on Map
		println(imMap filter {case(a,b)=>(a%2==0) } )
		
		//Append
		println(imMap+(6->'z'))
		println(imMap - (5))
		println(imMap ++ List((6->'z'),(7->'q')))
		println(imMap -- List(1,2))
		println(imMap.contains(7))
		println(imMap.size)
		println(imMap.keySet)
		println(imMap.values)
		println(imMap.isEmpty)
		println(imMap.get(7)) //return None
		val t1=imMap withDefaultValue "<UNKNOWN>"  //cause to not to throw exception
		println (t1(100))
		//println(imMap.(7))    //throw exception
		
		//TreeMap sorted map based on red black tree
		println(treeMap)
		
		
	   //Polynomial multiplication  
	   class poly (val terms:Map[Int,Double]){
		   val term=this.terms withDefaultValue 0.0 
		   def + (other:poly):poly= new poly(terms ++ (other.terms map { case(a,b)=>a->(b+term(a))})) //optimize way
		   
		   //def + (other:poly):poly= new poly(terms ++ (other.terms map adjust))     // Functional Way
																					  //++ will replace data from right to left  
		   //def adjust(term:(Int,Double)):(Int,Double)={
			   //val (a:Int,b:Double) =term
			   //terms get a match {
				   //case Some(b1:Double) => a->(b+b1)
				   //case None     => a->b
			   //}
		   //}
		   //def + (other:poly):poly={                                                 // imperitive way
			   //var res=Map[Int,Double]()
				   //val it = this.terms.iterator
				   //res=other.terms
				   //while( it.hasNext){
					   //val (a:Int,b:Double)=it.next
					   //if(other.terms.contains(a)) res=res + (a->(b+other.terms.getOrElse(a,0.0)))
					   //else res=res + (a->b)
				   //}

		  // new poly(res)
	   //}
         }
       class RefinedPoly(val terms:Map[Int,Double]){  
		   //Auxillary constructor to primary constructor
		   def this(Bindings:(Int,Double)*)=this(Bindings.toMap)  
		   val term=this.terms withDefaultValue 0.0
		   //Optimize because of tail recursion implementation of foldLeft 	 
		   def + (other:RefinedPoly):RefinedPoly = new RefinedPoly((other.terms foldLeft term){case(map,(exp,coeff))=>map + (exp->(coeff+term(exp)))}) 
		   override def toString= (for ((exp,coff)<-terms.toList.sorted.reverse) yield coff+"x^"+exp) mkString " + "
	   }
	   
	   
	   
	   val p1 = new poly(Map(1->2.0,2->4.0,3->6.2))
	   val p2 = new poly(Map(0->4.0,2->3.4))
	   println((p1+p2).terms)
	   val rp1 = new RefinedPoly(1->2.0,2->4.0,3->6.2)
	   val rp2 = new RefinedPoly(0->4.0,2->3.4)
       println((rp1+rp2))
		
	}
}

