package multitool

import collection.mutable.HashMap
import java.io.StringReader

class SymbolTable() {

  var strings = scala.collection.mutable.ArrayBuffer[String]()
  var ids = new HashMap[String,Int]()
  def size = strings.length
   
  def apply(i : Int) = strings(i)

  def add(s : String) = ids.getOrElseUpdate(s,{
	strings += s
	size - 1
  })
  
}

class CFGSymbolTable() {

  val syms = new SymbolTable()
  val terms = new SymbolTable()
    
  def growTree(s : String) : ParseTree = {
    if(s.length == 0)
      throw new Exception("Cannot grow a tree from an empty string")

	def isWhitespace(c : Char) = List(' ','\t','\r','\n') contains c

	def readNode(stream : StringReader) : NonTerminalNode = {

	  var ntStr = new StringBuffer()
	  var c = stream.read.toChar
	  while(!isWhitespace(c)) {ntStr append c; c = stream.read.toChar}
	  while(isWhitespace(c)) {c = stream.read.toChar} 
	  var kids = List[NonTerminalNode]()
	  var sym = syms.add(ntStr.toString)
	  
      while(stream.ready) {
		c match {
		  case '(' => kids ::= readNode(stream)
		  case ')' => {
            val ret = new ProtoNode(sym,kids.reverse)
            return ret
          }
		  case _ if isWhitespace(c) => {}
		  case _ => {
			var termStr = new StringBuffer()
			while(c != ')') {
			  termStr append c
			  c = stream.read.toChar
		    }  
			if(termStr.toString == "<>") {
			  return new UnderspecifiedNode(sym,null)
			}
			else {
              val ret = new PreTerminalNode(sym,new TerminalNode(terms.add(termStr.toString)))
              return ret
            }
		  }	
		}
		c = stream.read.toChar
	  }
	  null
	}
	var stringstream = new StringReader(s.trim)
	stringstream.read 
	new ParseTree(readNode(stringstream))
  }
  
  def size : Tuple2[Int,Int] = {(syms.size,terms.size)}

}
