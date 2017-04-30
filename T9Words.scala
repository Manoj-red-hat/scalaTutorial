/*
 * T9Words.scala
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

import scala.io.Source
object T9Words {
	
	def main (args:Array[String]){
		
		val in = Source.fromURL("http://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords.txt")
		
		val words=in.getLines.toList filter ( x=> x forall (t=>t.isLetter))
		
		val keypad=Map(2->"ABC",3->"DEF",4->"GHI",5->"JKL",6->"MNO",7->"PQRS",8->"TUV",9->"WXYZ")
		
		val mapKeyToValue = for( (key,value)<-keypad;char<-value) yield (char->key)
		
		def wordToNumber(word:String):String = (word.toUpperCase map (x=>mapKeyToValue(x))).mkString
		
		val dicitionarytoNumber:Map[String,Seq[String]] = words groupBy wordToNumber  withDefaultValue Seq()
		
			
		def encode(number:String):Set[List[String]]={
			if (number.isEmpty) Set(List())
			else{ 
				for {
					x <- 1 to number.length
					(a,b)=number.splitAt(x)
					word <- dicitionarytoNumber(a)
					rest <- encode(b) 
					} yield word :: rest 
				  }.toSet
		}
		
	println(encode("7225247386") map (_ mkString " "))
	}
}

