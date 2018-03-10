// Part 1 about Regular Expression Matching
//==========================================

abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp   // alternative OR
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp   // sequence AND
case class STAR(r: Rexp) extends Rexp             // star


// some convenience for typing in regular expressions

import scala.language.implicitConversions
import scala.language.reflectiveCalls

def charlist2rexp(s: List[Char]): Rexp = s match {
	case Nil => ONE
	case c::Nil => CHAR(c)
	case c::s => SEQ(CHAR(c), charlist2rexp(s))
}
implicit def string2rexp(s: String): Rexp = charlist2rexp(s.toList)

implicit def RexpOps (r: Rexp) = new {
	def | (s: Rexp) = ALT(r, s)
	def % = STAR(r)
	def ~ (s: Rexp) = SEQ(r, s)
}

implicit def stringOps (s: String) = new {
	def | (r: Rexp) = ALT(s, r)
	def | (r: String) = ALT(s, r)
	def % = STAR(s)
	def ~ (r: Rexp) = SEQ(s, r)
	def ~ (r: String) = SEQ(s, r)
}
/*
val ch : CHAR = new CHAR('c');
println(ch);// prints CHAR(c)
println(ch.c);// prints c

val alt  : ALT = new ALT(new CHAR('c'), new CHAR('s'));
println(alt);
println(ZERO);
val ch1 : CHAR = new CHAR('_');
println(ch1);
*/
// (1a) Complete the function nullable according to
// the definition given in the coursework; this
// function checks whether a regular expression
// can match the empty string


def nullable (r: Rexp) : Boolean = r match{

	//println(r);

	case ZERO => false;
	case ONE => true;
	case CHAR(c) => false;
	/*
	case CHAR(_) => if(r != CHAR('\0')){

				println(r);
				false;

			} else {
				println(r);
				true;

			}
	*/

	case ALT(r1,r2) => nullable(r1)||nullable(r2);

	case SEQ(r1,r2)	=> nullable(r1)&&nullable(r2);

	case STAR(r1)=> true; //maybe r only?

}
/*
println(nullable(ZERO));
println(nullable(ONE));
println(nullable(new CHAR('c')));
println(nullable(new CHAR('\0')));
println(nullable(new ALT(new CHAR('c'), new CHAR('z'))));
println(nullable(new ALT(new CHAR('\0'), new CHAR('\0'))));
println(nullable(new ALT(new CHAR('c'), new CHAR('\0'))));
println(nullable(new SEQ(new CHAR('1'), new CHAR('\0'))));
println(nullable(new SEQ(new CHAR('\0'), new CHAR('\0'))));
println(nullable(new SEQ(new CHAR('$'), new CHAR('-'))));
println(nullable(new STAR(new ALT(new CHAR('*'), new CHAR('0')))));
*/
//val ch : CHAR = new CHAR(None);
//println(nullable())
//println(nullable('c'));
//CHAR ch = new CHAR('c');


// (1b) Complete the function der according to
// the definition given in the coursework; this
// function calculates the derivative of a
// regular expression w.r.t. a character
/*

def der (c: Char, r: Rexp) : Rexp = r match{
  //inside brackets are objects
  case ZERO => ZERO
  case ONE => ONE
  case CHAR(d) => if (c==d) ONE
  else ZERO
  /* case ALT(r1,r2) => ALT(der(c,r1),der(c,r2)) */
  //case ALT(r1,r2) => der(c,r1)|der(c,r2)
  /* case SEQ(r1,r2) => if (nullable(r1)) ALT(SEQ(der(c,r1),r2), der(c,r2))
                    else SEQ(der(c,r1),r2)  */
  //case SEQ(r1,r2) => if (nullable(r1)) (der(c,r1)|r2)~der(c,r2)
  //else der(c,r1)~r2
  /* case STAR(r)=> SEQ(der(c,r),STAR(r))   */
  case STAR(r)=> der(c,r)|(r%)  //might not work
}
*/
 //JAMES DER METHOD

def der (c: Char, r: Rexp) : Rexp = r match{

	case ZERO => 	ZERO;
	case ONE =>	ZERO;
	case CHAR(d) =>	if(c == d){

		ONE;

	} else {

		ZERO;

	}

	case ALT(r1,r2)=>	ALT((der(c,r1)),(der(c,r2)));
	//(der(c,CHAR(r1))),(der(c,CHAR(r2)))

	case SEQ(r1,r2)=>	if(nullable(r1)){

		ALT(SEQ(der(c,r1),r2),der(c,r2))

	}else {

		SEQ(der(c,r1),r2);

	}
	case STAR(r)=>		SEQ(der(c,r),STAR(r));

} 

val a : CHAR = new CHAR('a');
val b : CHAR = new CHAR('b');
val c : CHAR = new CHAR('c');

val r : Rexp = SEQ(SEQ(a,b),c);// (a.b).c

/*
println(der('a',a));
println(der('a',b));
println(der('a',c));

println(der('a',r));
println(der('b',r));
println(der('c',r));

//val r1 : Rexp = der('a',r);// (a.b).c

val r1 = SEQ(ALT(SEQ(ZERO,b),ZERO),c);

println(der('b',b));

println(der('a',r1));
println(der('b',r1));
println(der('c',r1));

val r2 : Rexp = der('b',r1);
println(der('a',r2));
println(der('b',r2));
println(der('c',r2));

val rLast : Rexp = der('c',r2);
println(nullable(rLast));
*/


// (1c) Complete the function der according to
// the specification given in the coursework; this
// function simplifies a regular expression;
// however it does not simplify inside STAR-regular
// expressions


def simp(r: Rexp) : Rexp = r match {

	case SEQ(r1,r2) => (simp(r1),simp(r2)) match {
			
	
    			case (r1, ZERO) => ZERO
    			case (ZERO, r2) => ZERO
    			case (r1, ONE) => r1

    			case (ONE, r2) => r2
    			case (r1,r2) => SEQ(r1,r2)
			case _=> r
			
  		}

 	 case ALT(r1,r2) => (simp(r1),simp(r2)) match {

    			case (r1, ZERO) => r1
    			case (ZERO, r2) => r2
   			case (r1, r2) => if (r1 == r2) r1
    			else ALT (r1,r2)
    			case _=>r

  	}

  	case _ => r

}
/*
val testChar : CHAR = new CHAR('c');
//println(simp(testChar));


//println("testing simplifications")
val myexpr : SEQ = new SEQ(testChar,ZERO);
val myexpr1 : ALT = new ALT(testChar,ZERO);
//println(simp(myexpr))

//println(simp(myexpr1));
val compEx : Rexp = ALT(ALT(ONE,a),b);

//println(simp(compEx));
val d : CHAR = new CHAR('d');
//val myCompEx : Rexp = SEQ(ALT(ALT(ONE,b),c),SEQ(d,ZERO));//ALT(SEQ(ALT(a,ZERO),ONE),SEQ(ALT(ALT(ONE,b),c),SEQ(d,ZERO)));
val myCompEx : Rexp = ALT(SEQ(ALT(a,ZERO),ONE),SEQ(ALT(ALT(ONE,b),c),SEQ(d,ZERO)));
//println(simp(myCompEx));

*/
// (1d) Complete the two functions below; the first
// calculates the derivative w.r.t. a string; the second
// is the regular expression matcher taking a regular
// expression and a string and checks whether the
// string matches the regular expression

def ders (s: List[Char], r: Rexp) : Rexp = s match{

	case Nil => r;
	case s::sx => ders(sx,simp(der(s,r)));

}


def matcher(r: Rexp, s: String): Boolean = {

	//val rex = ders(s.toList, r);ders(s.toList, r)

	nullable(ders(s.toList, r));

}

//println(matcher(SEQ(SEQ(a,b),c), "abc"));
//println(matcher(ALT(STAR(SEQ(a,a)),SEQ(b,b)),""));
//println(matcher(ALT(STAR(SEQ(a,a)),SEQ(b,b)),"aabb"));
//println(matcher(STAR(ALT(STAR(SEQ(a,a)),SEQ(b,b))),"aabb"));

// (1e) Complete the function below: it searches (from the left to
// right) in string s1 all the non-empty substrings that match the
// regular expression -- these substrings are assumed to be
// the longest substrings matched by the regular expression and
// assumed to be non-overlapping. All these substrings in s1 are replaced
// by s2.

//WORKS
def replace(r: Rexp, s1: String, s2: String): String = {
	
	var charList : List[Char] = s1.toList;
	
	//println(""+charList)
	//-> compare the match with each character in the list ... until you get to the end...
	// then make sure
	// you replace with the next...
	// then subtract....
	// and 
	var stringToUse : String = s1; 
	var currentString : String = "";
	var currentLastMatch : String = "";
	var resultString : String = "";
	// per replacement.
	
	
	while(stringToUse.length > 0){

		var isFound : Boolean = false;

		charList = stringToUse.toList;

		var count : Int = 0;

		for(ch <- charList) {
		
			currentString += ""+ch 
		
			if(matcher(r,currentString) && !nullable(currentString)){
				
				//println(currentString);
				currentLastMatch = currentString;			
				isFound = matcher(r,currentString) && !nullable(currentString)
				count = currentString.length;
				//println(count);

			}
	
			

		}

		//println("count"+count);
		
		//println("droppedList"+stringToUse);

		if(!isFound){
			
			stringToUse = stringToUse.drop(1);
			resultString += charList(0);
						
			
		} else {
			stringToUse = stringToUse.drop(count);
			resultString += s2;				
					
		}
		
		currentString = "";
		currentLastMatch = "";
		isFound = false;
		
		//println(charList);

	}
	
	resultString;
	
}

/*
def replace(r: Rexp, s1: String, s2: String): String = {
	
	replaceCall(r,s1,s2,"","");	
	
}
*/
/*	
def replaceCall(r: Rexp, s1: String, s2: String,result : String, tempMem: String): String = {
	
	//println(s1);
	//var charList : List[Char] = s1.toList;
	if(s1.length == 0){// PROGRSSION TOWARDS 0.
		
		result;		
		
	}  
	
	else if(matcher(r,tempMem) && tempMem > 0){

		val result2= result+s2;
		val tempMem2 = s1.drop(tempMem.size);
		replaceCall(r,tempMem2,s2,result2,tempMem2);

	} else if (!matcher(r,tempMem)){

		//result += s2;
		// put back in tempMem = s1;
		replaceCall(r,s1.drop(1),s2,result+s1.drop(0),s1.drop(1));

	} else if(){
				
					
			
	} else if(){
	
	}
	
	else {
		
		result;		
			
	}
	
}
*/
	//println(""+charList)
	//-> compare the match with each character in the list ... until you get to the end...
	// then make sure
	// you replace with the next...
	// then subtract....
	// and 
	//val stringToUse : String = s1; 
	//val currentString : String = "";
	//val currentLastMatch : String = "";
	//val resultString : String = "";
	// per replacement.

	
	//for(ch <- s1 if(matcher(r,s1.substring(0, ch.toList))));
	//val result = for(ch <- s1.toList )
	//println(result);
	
	
	
	/*
	while(stringToUse.length > 0){

		charList = stringToUse.toList;
		
		val currentLM = for(ch <- s1.toList) yield {
			
			currentString += ""+ch; 
		
			if(matcher(r,currentString)){

				currentLastMatch = currentString;			
				currentString;

			}

		}
		
		//println(currentLM.flatten);
		//println("count"+count);
		
		//println("droppedList"+stringToUse);

		if(currentLastMatch.length == 0){
			
			stringToUse = stringToUse.drop(1);
			resultString += charList(0);
						
		} else {

			stringToUse = stringToUse.drop(currentLastMatch.length);
			resultString += s2;				
					
		}
		
		currentString = "";
		currentLastMatch = "";
		
		
		//println(charList);

	}
	
	resultString;
	*/

var si1 : String = "aabb"; 
var s1 : String = "aabbbaaaaaaabaaaaabbaaaabb"; 
var s2 : String = "c";
var regex1 : Rexp = ALT(STAR(SEQ(CHAR('a'),CHAR('a'))),SEQ(CHAR('b'),CHAR('b')));
println(replace(regex1,s1,s2));
//println(replace(regex1,si1,s2));

// some testing data
// the supposedly 'evil' regular expression (a*)* b
val EVIL = SEQ(STAR(STAR(CHAR('a'))), CHAR('b'))
println(matcher(EVIL, "a" * 1000 ++ "b"))
println(matcher(EVIL, "a" * 1000))


def time_needed[T](i: Int, code: => T) = {
	val start = System.nanoTime()
	for (j <- 1 to i) code
	val end = System.nanoTime()
	(end - start)/(i * 1.0e9)
}

for (i <- 1 to 5000001 by 500000) {
	println(i + " " + "%.5f".format(time_needed(2, matcher(EVIL, "a" * i))))
}
