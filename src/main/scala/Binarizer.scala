package multitool

class Trinarizer(st : CFGSymbolTable) {

  def getH(tree : ParseTree, hf : HeadFinder, nt : InternalNode, lr : Option[Boolean]) = {
    val kids = nt.children
    var headInd = 0 //leftmost by default
    lr match {
      case Some(ls) => {
        if(ls)
          headInd = kids.length - 1
      }
      case None => {
        var ind = 0
        kids.foreach(k => {
          if(hf(k) eq hf(nt))
            headInd = ind
          ind += 1
        })
      }
    }                
    headInd
  }
  
  def makeBar(n : Int, left : Boolean) : Int = {
	var s = st.syms(n)
	if(s.indexOf('@') < 0 && s.indexOf('+') < 0) {//not yet a bar
	  if(left)
        s = "@" + s
      else
        s = "+" + s
    }
	st.syms.add(s)
  }

  def isBarSymbol(nt : NonTerminalNode) : Boolean = {
    (st.syms(nt.symbol).indexOf('@') >= 0 ||
     st.syms(nt.symbol).indexOf('+') >= 0)
  }

  def transform(tree : ParseTree) : ParseTree = {	  
    
    val hf = new HeadFinder(st,tree)
    
    def recTri(n : NonTerminalNode,lr : Option[Boolean]) : NonTerminalNode = {
      n match {
	    case pt : PreTerminalNode => pt
        case nt : InternalNode => {
    	  val kids = nt.children
    	  val sym = nt.symbol
    	  if(kids.length <= 2) {
    		
            new ProtoNode(sym,kids.map(a => recTri(a,None)))
    	    
          } else {
            val headInd = getH(tree,hf,nt,lr)
            
            val fst = kids.slice(0,headInd)
            val khead = recTri(kids(headInd),None)
            val lst = kids.drop(headInd + 1)
            
            var k1 : List[NonTerminalNode] = Nil
            if(fst.length > 0)
              k1 = List(recTri(new ProtoNode(makeBar(sym,true),fst),Some(true)))
            var k2 : List[NonTerminalNode] = Nil
            if(lst.length > 0)
              k2 = List(recTri(new ProtoNode(makeBar(sym,false),lst),Some(false)))
            
            val newKids = k1 ::: List(khead) ::: k2
            
            new ProtoNode(sym,newKids)
          }
        }
      }
    }
    	
	new ParseTree(recTri(tree.root,None))
  }
 
  def revert(tree : ParseTree) : ParseTree = {

    def rec2(n : NonTerminalNode) : List[NonTerminalNode] = {
      n match {
        case in : InternalNode => {
          in.children.flatMap(c => {
            if(isBarSymbol(c)) {
              rec2(c)
            } else {
              List(recUTri(c))
            }
          })
        }
      }
    }

    def recUTri(n : NonTerminalNode) : NonTerminalNode = {
      n match {
	    case pt : PreTerminalNode => pt
        case nt : InternalNode => {
    	  val kids = nt.children
    	  val sym = nt.symbol
    	  
          val newKids : List[NonTerminalNode] = kids.flatMap(k => {
            if(isBarSymbol(k)) {
              k match {
                case in : InternalNode => rec2(in)
              }
            } else {
              List(recUTri(k))
            }
          })

          new ProtoNode(sym,newKids)
        }
      }
    }

	new ParseTree(recUTri(tree.root)) 
  }  

}

class HBinarizer(st : CFGSymbolTable, lbl : String) extends Trinarizer(st) {

  override def transform(tree : ParseTree) : ParseTree = {	  
          
    val hf = new HeadFinder(st,tree)
    
    def recBi(n : NonTerminalNode) : NonTerminalNode = {
      n match {
	    case pt : PreTerminalNode => pt
        case nt : InternalNode => {
    	  val kids = nt.children
    	  val sym = nt.symbol
          val str = st.syms(sym)
          val headInd = getH(tree,hf,nt,None)

          val hStr = st.syms(kids(headInd).symbol)
          val bSym = lbl match {
            case "HEAD" => st.syms.add("@" + hStr)
            case "PARENT" => st.syms.add("@" + str)
            case "HP" => st.syms.add("@" + hStr + "-" + str)
            case _ => throw new Exception(lbl + " is not a valid binarizer - [HEAD,PARENT,HP]")
          }
          val fst = kids.slice(0,headInd)
          val khead = new ProtoNode(bSym,List(recBi(kids(headInd))))
          val lst = kids.drop(headInd + 1)
         
          val rtree : NonTerminalNode = (khead /: lst)((a,b) => {
            new ProtoNode(bSym,List(a,recBi(b)))
          })
            
          val ltree = (rtree /: fst.reverse)((a,b) => {
            new ProtoNode(bSym,List(recBi(b),a))
          })
          
          new ProtoNode(sym,List(ltree))
          
        }
      }
    }
    	
	new ParseTree(recBi(tree.root))
    
  }

}

class LBinarizer(st : CFGSymbolTable) extends Trinarizer(st) {

  override def getH(tree : ParseTree, hf : HeadFinder, nt : InternalNode,lr : Option[Boolean]) = {
    0
  }

}

class Binarizer(st : CFGSymbolTable) extends Trinarizer(st) {

  override def getH(tree : ParseTree, hf : HeadFinder, nt : InternalNode,lr : Option[Boolean]) = {
    nt.children.length - 1
  }

}
