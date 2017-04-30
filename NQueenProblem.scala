/*
 * NQueenProblem.scala
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


object NQueenProblem {
	
	def main (args:Array[String]){
		def queens(n:Int):Set[List[Int]]={
			def placeQueens(k:Int):Set[List[Int]]=
				if(k==0) Set(List())
				else for {
					queens<-placeQueens(k-1)
					col <- 0 until n
					if isSafe(col,queens)
				    } yield col::queens
			placeQueens(n)
		}
		
			println (((queens(8)) map show) mkString ("\n"))
	}
	
	def isSafe(Col: Int,queens: List[Int]):Boolean={
		val row=queens.length
		val queenswithRow=(row-1 to 0 by -1) zip queens
		queenswithRow forall {
			case (r,c)=> c!=Col && math.abs(Col-c)!=(row-r)
		}
	}
	
	def show(queens: List[Int])={
		val lines =
			for (col<-queens.reverse)
			yield Vector.fill(queens.length)(" * ").updated(col," X ").mkString
			 "\n"+(lines mkString "\n")
	}
	

}

