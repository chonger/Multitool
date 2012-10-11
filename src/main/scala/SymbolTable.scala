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
  
  def lazyReader(filE : String, doMe : (ParseTree) => Unit) : Unit = {
    var filedata : List[String] = Nil
    import java.io.{File,FileReader,BufferedReader}
    val br = new BufferedReader(new FileReader(new File(filE)))
    var l = br.readLine()
    while(l != null) {
      if(l.length > 0)
        filedata ::= l
      l = br.readLine()
    }
    br.close()
    filedata = filedata.reverse

    lazyReader(filedata,doMe)
  }

  def lazyReader(filedata : List[String], doMe : (ParseTree) => Unit) : Unit = {
    var ind =0
    val lastTree = ("" /: filedata)((a,b) => {
      if(b.charAt(0) != '(') 
        (a + b) 
      else {
        if(a.length > 0) {
          val tree = growTree(a)  
          ind += 1
          doMe(tree)  
        }
        b //start a new one off with the new b
      }
    })
    doMe(growTree(lastTree))
  }
 
  def read(filename : String) = {
    var trees = List[ParseTree]()
    lazyReader(filename,(t : ParseTree) => {trees ::= t})
    trees.reverse
  }

  //prints each tree on a single line in full text format
  def write(filename : String, data : List[ParseTree]) = {
    println("writing " + data.length + " trees to " + filename + " in treebank format")
    import java.io.{File,FileWriter,BufferedWriter}
	var bw = new BufferedWriter(new FileWriter(new File(filename)))

	data.foreach(d => {
      bw.write(d.fString(this) + "\n")
    })
	bw.close
  }

}
