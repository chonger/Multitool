package multitool

object Unker {

  def swunk(dox : Array[XMLDoc[ParseTree]], st : CFGSymbolTable) = {
    val unker =  new WordlistUnker("src/main/resources/smart_common_words.txt",st)    
    dox.map(_.map(s => {
      unker.unkTree(s)
    }))
  }

  

}

abstract class Unker(st : CFGSymbolTable) {
  //override these two
  def unk_?(pn : PreTerminalNode) : Boolean 
  def unkToken(pt : Int, s : String, pos : Int) : String 

  def unkTree(t : ParseTree) : ParseTree = {
    
    val terminalOrder = t.terminals
      
    def findIndex(tn : TerminalNode) : Int = {
      for{i <- 0 to terminalOrder.length - 1} {
    	if(tn eq terminalOrder(i))
    	  return i
      }
      throw new Exception("Not Found")
    } 
      
    def unkNode(nt : NonTerminalNode) : NonTerminalNode = {
	  nt match {
		case pn : PreTerminalNode => {
		  if(unk_?(pn)) {
		    val index = findIndex(pn.kid)
		    val unkedS = st.terms.add(unkToken(pn.symbol,st.terms(pn.kid.terminal),index))
		    new PreTerminalNode(pn.symbol,new TerminalNode(unkedS))
		  } else
	        new PreTerminalNode(pn.symbol,new TerminalNode(pn.kid.terminal))
		}
		case pn : ProtoNode => {
          new ProtoNode(pn.symbol,pn.children.map(unkNode(_)))
        }
		case un : UnderspecifiedNode => {
		  if(un.assignment != null) 
		    new UnderspecifiedNode(un.symbol,unkNode(un.assignment))
		  else
		    new UnderspecifiedNode(un.symbol,null)
	    }                             
      }
    }
    new ParseTree(unkNode(t.root))
  }

  def bpUnk(pt : Int, s : String, pos : Int) : String = {
	
	def myUpper(c : Char) = c.isUpper || Character.isTitleCase(c)
	
	val sb = new StringBuffer("UNK")
	
	var wLen = s.length
	if(wLen == 0)
	  throw new Exception
	var numCaps = s.filter(myUpper(_)).length
	var hasDigit = !s.filter(_.isDigit).isEmpty 
	var hasDash = !s.filter(_ == '-').isEmpty
	var hasLower = wLen != numCaps
    
	var firstChar = s(0)
	var lowered = s.toLowerCase
	if(myUpper(firstChar)) {
	  if(pos == 0 && numCaps == 1) {
		sb.append("-INITC")
        
		if((st.terms.strings contains lowered) && 
           !unk_?(new PreTerminalNode(pt,new TerminalNode(st.terms.ids(lowered))))) {//we know it lowercased
             sb.append("-KNOWNLC")
		   }

	  } else sb.append("-CAPS")
	} else if (!Character.isLetter(firstChar) && numCaps > 0) {
	  sb.append("-CAPS")
	} else if(hasLower) sb.append("-LC")
    
	if(hasDigit) sb.append("-NUM")
	if(hasDash) sb.append("-DASH")
	
	if(lowered.endsWith("s") && wLen >3) {
	  if(!(List('s','i','u') contains lowered(wLen - 2))) sb.append("-s")
	} else if(wLen >= 5 && !hasDash && !(hasDigit && numCaps > 0)) {
	  
	  val suffixes = List("ed","ing","ion","er","est","ly","ity","y","al")
	  def getSuffix() : String = {
		for{s <- suffixes}{if(lowered.endsWith(s)) return s}
		return null
	  }
	  
	  val suffix : String= getSuffix()
	  if(suffix != null)
		sb.append("-" + suffix)
	}
    
	sb.toString  
  }

}


/**
 *  
 *   UNK all words that do not appear in a wordlist with a single UNK symbol
 * 
 */ 
class WordlistUnker(wordlistFile : String, st : CFGSymbolTable) extends Unker(st) {

  import java.io._

  val bw = new BufferedWriter(new FileWriter("/home/chonger/dump/unked.txt"))

  var words = new scala.collection.mutable.HashSet[String]()

  var lines = io.Source.fromFile(wordlistFile).getLines.foreach(l => words += l.trim)
  
  override def unk_?(pn : PreTerminalNode) : Boolean = {
    val str = st.terms(pn.kid.terminal)
    !(words contains str.toLowerCase)      
  }

  override def unkToken(pt : Int, s : String, pos : Int) : String = "UNK"

}

/**
 *
 *  UNK any preterminal that doesnt appear in the data provided 
 *
 */ 
class UnkFromData(data : List[ParseTree], st : CFGSymbolTable) extends Unker(st) {

  val lexicon = new scala.collection.mutable.HashSet[PreTerminalNode]()

  data.foreach(t => lexicon ++= t.preterminals)

  override def unk_?(s : PreTerminalNode) = !(lexicon contains s)
  
  override def unkToken(pt : Int, s : String, pos : Int) : String = {
    var ret = bpUnk(pt,s,pos)
    ret
  }
}

/**
 *
 * UNK anything which occurs at most n times (so n=1 removes all pts that occur only once)
 *
 */ 
class UnkLeastCommon(n : Int, filE : String, st : CFGSymbolTable) extends Unker(st) {
  import scala.collection.mutable.{HashMap,HashSet}
  import java.io.{BufferedReader,FileReader}

  val counts = new HashMap[PreTerminalNode,Int]()
  val lexicon = new HashSet[PreTerminalNode]()

  val br = new BufferedReader(new FileReader(filE))

  var line = br.readLine()

  while(line != null) {
    val t = st.growTree(line)
    t.preterminals.foreach(pn => {
	  counts(pn) = counts.getOrElse(pn,0) + 1
    })
    line = br.readLine()
  }

  br.close()

  counts.keySet.foreach(k => {
	if(counts(k) > n)
	  lexicon += k
  })
  counts.clear()
  println("Got a lexicon of size " + lexicon.size)

  override def unk_?(s : PreTerminalNode) = !(lexicon contains s)

  override def unkToken(pt : Int, s : String, pos : Int) : String = bpUnk(pt,s,pos)
  

}

