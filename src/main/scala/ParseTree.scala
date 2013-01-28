package multitool

object TreeTools {

  /**
   * get a hashset of all the PCFG rules used in a list of parse trees
   */ 
  def cfgSet(treez : List[ParseTree]) = {
    val pcfgSet = new scala.collection.mutable.HashSet[ParseTree]()
    treez.foreach(tree => {
      val nnodes = tree.nonterminals.filter(!_.isInstanceOf[UnderspecifiedNode])
      nnodes.foreach(n => {
        n match {
          case pn : PreTerminalNode => {
            pcfgSet += new ParseTree(pn)
          }
          case in : InternalNode => {
            val newkids = in.children.map(n2 => new UnderspecifiedNode(n2.symbol,null))
            pcfgSet += new ParseTree(new ProtoNode(in.symbol,newkids))
          }
        }
      })
    })    
    pcfgSet
  }

  /**
   * Copy a treenode and all of its children
   */
  def deepCopy(n : NonTerminalNode) : NonTerminalNode = {
	  n match {
	    case ptn : PreTerminalNode => new PreTerminalNode(ptn.symbol,new TerminalNode(ptn.kid.terminal))
	    case pn : ProtoNode => new ProtoNode(pn.symbol,pn.children.map(deepCopy(_)))
	    case un : UnderspecifiedNode => {
	      if(un.assignment != null) 
	    	new UnderspecifiedNode(un.symbol,deepCopy(un.assignment))
	      else
	    	  new UnderspecifiedNode(un.symbol,null)
        }                             
	  }
  }
}

/**
 * Using the case class formalization will create hashCode and equals which apply to the whole tree
 */
abstract class TreeNode {
  def toString(symTab : CFGSymbolTable) : String
}

class TerminalNode(val terminal : Int) extends TreeNode {
  override def equals(any : Any) : Boolean = {
	any match {
	  case t : TerminalNode => terminal == t.terminal
	  case _ => false
	}	
  }
  override def hashCode() : Int = terminal.hashCode()
  override def toString(symTab : CFGSymbolTable) : String = {
    symTab.terms(terminal)
  }
}
abstract class NonTerminalNode(val symbol : Int) extends TreeNode {
  def children : List[TreeNode] 
  def rule : TreeRule
}
class PreTerminalNode(override val symbol : Int,
						   val kid : TerminalNode) extends NonTerminalNode(symbol) {
  def children : List[TerminalNode]= List(kid)
  override def rule = new TerminalRule(symbol,kid.terminal)
  override def equals(any : Any) : Boolean = {
	any match {
	  case t : PreTerminalNode => symbol == t.symbol && kid == t.kid
	  case _ => false
	}	
  }
  override def hashCode() : Int = symbol.hashCode() ^ kid.hashCode()
  override def toString(symTab : CFGSymbolTable) : String = {
    symTab.syms(symbol) + " -> " + kid.toString(symTab)
  }
}
abstract class InternalNode(override val symbol : Int) extends NonTerminalNode(symbol) {
	override def children() : List[NonTerminalNode]
}
class UnderspecifiedNode(override val symbol : Int,
							  val assignment : NonTerminalNode) extends InternalNode(symbol) {
  var foot = false
  override def children = if(assignment == null) Nil else List(assignment)
  override def rule = null
  override def equals(any : Any) : Boolean = {
	any match {
	  case t : UnderspecifiedNode => symbol == t.symbol && foot == t.foot
	  case _ => false
	}	
  }
  override def hashCode() : Int = symbol.hashCode() ^ foot.hashCode()
  override def toString(symTab : CFGSymbolTable) : String = {
    val kidStr = if(assignment == null) "" else " -> " + symTab.syms(assignment.symbol)
    if(foot)
      "*" + symTab.syms(symbol) + "*" + kidStr
    else
      "[" + symTab.syms(symbol) + "]" + kidStr
  }
}
class ProtoNode(override val symbol : Int,
                         val kids : List[NonTerminalNode]) extends InternalNode(symbol) {
  override def children = kids
  override def rule = new ProtoRule(symbol,kids.map(_.symbol))
  override def equals(any : Any) : Boolean = {
	any match {
	  case t : ProtoNode => kids.length == t.kids.length && ((symbol == t.symbol) /: (kids zip t.kids))((a,b) => a && b._1 == b._2)
	  case _ => false
	}	
  }
  override def hashCode() : Int = (symbol.hashCode() /: kids)(_ ^ _.hashCode())
  override def toString(symTab : CFGSymbolTable) : String = {
    symTab.syms(symbol) + " -> " + kids.map(x => symTab.syms(x.symbol)).toArray.mkString(" ")
  }
}

//a wrapper for hashing based on pointer equality
class RefWrapper(val n : NonTerminalNode) {
	override def equals(any : Any) : Boolean = {
			any match {
			case t : RefWrapper => {
				return n eq t.n
			}
			case _ => false
			}	
	}
	override def hashCode() : Int = n.hashCode()
}

abstract class TreeRule(val lhs : Int) {
  def node() : NonTerminalNode
}
class ProtoRule(override val lhs : Int, 
				val children : List[Int]) extends TreeRule(lhs) {
	 def rhs = children
	 override def hashCode() : Int = {
		var shift = 0
	    (lhs.toInt /: children)((a,b) => {shift += 2; a + (b << shift)})
	 }
	 override def equals(any : Any) : Boolean = {
	  any match {
	    case u : ProtoRule => u.lhs == this.lhs && u.rhs == this.rhs
	    case _ => false	
	  }
	 }
  override def node() : NonTerminalNode = {
    new ProtoNode(lhs,children.map(x => new UnderspecifiedNode(x,null)))
  }
}
class TerminalRule(override val lhs : Int,
				   val terminal : Int) extends TreeRule(lhs) {
	override def hashCode() : Int = {
	    lhs.toInt ^ (terminal << 8)
	 }
	 override def equals(any : Any) : Boolean = {
	  any match {
	    case u : TerminalRule => u.lhs == this.lhs && u.terminal == this.terminal
	    case _ => false	
	  }
	 }
  override def node() : NonTerminalNode = {
    new PreTerminalNode(lhs,new TerminalNode(terminal))
  }
} 

class ParseTree(val root : NonTerminalNode) {

  def fString(symTab : CFGSymbolTable) = {
    pString(symTab).replaceAll("\\s+"," ")
  }

  def pString(symTab : CFGSymbolTable) = {
	recTreeToString(symTab,root,"")
  }  
  
  def recTreeToString(st : CFGSymbolTable, n : TreeNode, offset : String) : String = {
    
    def getSpcStr(n : Int) = 0.until(n).map(x => " ").toArray.mkString("")
    
    n match {
      case un : UnderspecifiedNode => {
        var symstr = st.syms(un.symbol)
        
        var ret = "(" + symstr + " "
        if(un.assignment != null) {
        	var spcstr = getSpcStr(symstr.length + 2)
        	ret + recTreeToString(st,un.assignment,offset) + ")"
        } else {
          if(un.foot)
            ret + "*)"
          else
            ret + "<>)"
        }
      }
      case nt : NonTerminalNode =>
        var symstr = st.syms(nt.symbol)
    	var ret = "(" + symstr + " "
    	var spcstr = getSpcStr(symstr.length + 2)
    
    	ret += (recTreeToString(st,nt.children(0),offset + spcstr) /: nt.children.drop(1))((a,b) => {
    	    a + "\n" + offset + spcstr + recTreeToString(st,b,offset + spcstr)})
    	ret + ")"
      case tn : TerminalNode => {
        st.terms(tn.terminal)  
      }
    }
  }  
  
  def deepCopy() = new ParseTree(TreeTools.deepCopy(root))
  
  def nodes : List[TreeNode] = recGetNodes(root)
  def recGetNodes(n : TreeNode) : List[TreeNode] = {
 	n match { 
 	  case nt : NonTerminalNode => nt :: nt.children.flatMap(recGetNodes(_))
 	  case _ => List(n)
 	}
  }
  
  def underspecs : List[UnderspecifiedNode] = 
 	(nodes filter 
     (n => n.isInstanceOf[UnderspecifiedNode] && 
 	  n.asInstanceOf[UnderspecifiedNode].assignment == null)).asInstanceOf[List[UnderspecifiedNode]]
  
  def nonterminals : List[NonTerminalNode] = 
    (nodes filter (_.isInstanceOf[NonTerminalNode])).asInstanceOf[List[NonTerminalNode]]

  def preterminals : List[PreTerminalNode] = 
    (nodes filter (_.isInstanceOf[PreTerminalNode])).asInstanceOf[List[PreTerminalNode]]
  
  def terminals : List[TerminalNode] = 
    (nodes filter (_.isInstanceOf[TerminalNode])).asInstanceOf[List[TerminalNode]]
  
  def sentence(sTab : CFGSymbolTable) : String = {
    val yld = nodes filter (n => n.isInstanceOf[TerminalNode] || 
                              (n.isInstanceOf[UnderspecifiedNode] && 
                               n.asInstanceOf[UnderspecifiedNode].assignment == null))    

    yld.toArray.map({
      case tn : TerminalNode => sTab.terms(tn.terminal)
      case un : UnderspecifiedNode => "*" + sTab.syms(un.symbol) + "*"
    }).mkString(" ")
  }

  def sentence(sTab : CFGSymbolTable, span : (Int,Int)) : String = {
    val w = terminals.map(x => sTab.terms(x.terminal))
    val w2 = w.slice(0,span._1) ::: List("[") ::: w.slice(span._1,span._2) ::: List("]") ::: w.drop(span._2)
    w2.toArray.mkString(" ")
  }
  
  /**
   *          HASH FUNCTION
   */ 
  override def equals(any : Any) : Boolean = {
 	  //println("calling ParseTree equals")
 	any match {
	  case t : ParseTree => {
	    if(hashCode == t.hashCode)
	      return t.root == this.root
	    false
	  }
	  case _ => false
	}
  }
  lazy val myHash = root.hashCode
  override def hashCode() : Int = myHash
  
  def isPCFG() = (depth() == 1)
  def depth() = {
    def recD(n : TreeNode) : Int = {
      n match {
        case p : PreTerminalNode => 1
        case un : UnderspecifiedNode => 0
        case pn : ProtoNode => (0 /: pn.children)((a,b) => math.max(a,recD(b)+1))
      }
    }
    recD(root)
  }

  def getSpans() : Array[(Int,Int)] = {
    import scala.collection.mutable.{HashMap,HashSet}
    val hMap = new HashMap[RefWrapper,(Int,Int)]()

    def recSpan(n : NonTerminalNode, st : Int) : (Int,Int) = {
      val rw = new RefWrapper(n)
      if(hMap contains rw) {
        hMap(rw)
      } else {
        val span = n match {
          case ptn : PreTerminalNode => (st,st+1)
	      case pn : ProtoNode => {
            var accum = 0
            val kLens = pn.children.map(x => {
              val sp = recSpan(x,st + accum)
              val len = sp._2 - sp._1
              accum += len
              len
            })
            val tLen = (0 /: kLens)(_ + _)
            (st,st+tLen)
          }
        }
        hMap += rw -> span
        span
      }
    }

    recSpan(root,0)

    nonterminals.map(x => {
      val rw = new RefWrapper(x)
      hMap(rw)
    }).toArray
  }

  def getTree() : ParseTree = {this}

  def getBNPs(st : CFGSymbolTable) : List[ProtoNode] = {
    nonterminals.filter(x => st.syms(x.symbol) == "NP").map(_.asInstanceOf[ProtoNode]).filter(y => {
      new ParseTree(y).nonterminals.filter(x => st.syms(x.symbol) == "NP").length == 1
    })
  }

}

