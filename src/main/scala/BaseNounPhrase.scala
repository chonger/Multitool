package multitool

/**
 *
 *  In order to find syntactic patterns, it can help to collapse Base Noun Plrases
 *
 *
 * 
 */ 

object BaseNounPhraseCompressor {

  def compressTree(t : ParseTree, st : CFGSymbolTable) : ParseTree = {
    compressTree(t,st,nnjjcompress)
  }

  def compressTreeNNP(t : ParseTree, st : CFGSymbolTable) : ParseTree = {
    compressTree(t,st,nnpcompress)
  }

  def compressTree(t : ParseTree, st : CFGSymbolTable, f : (ProtoNode,CFGSymbolTable) => ProtoNode) : ParseTree = {
   
    def compressCopy(n : NonTerminalNode) : NonTerminalNode = {
	  n match {
	    case ptn : PreTerminalNode => new PreTerminalNode(ptn.symbol,new TerminalNode(ptn.kid.terminal))
	    case pn : ProtoNode => {
          if(st.syms(pn.symbol) == "NP") {
            val n = nnjjcompress(pn,st)
            new ProtoNode(n.symbol,n.children.map(compressCopy(_)))
          } else
            new ProtoNode(pn.symbol,pn.children.map(compressCopy(_)))
        }
	    case un : UnderspecifiedNode => {
	      if(un.assignment != null) 
	    	new UnderspecifiedNode(un.symbol,compressCopy(un.assignment))
	      else
	    	new UnderspecifiedNode(un.symbol,null)
        }                             
	  }
    }

    new ParseTree(compressCopy(t.root))

  }

  //compress sequences of NNP/NNPS
  def nnpcompress(n : ProtoNode, st : CFGSymbolTable) : ProtoNode = {
    val newK = (List[NonTerminalNode]() /: n.children)((a,b) => {
      if(a.length == 0)
        List(b)
      else {
        val sym = st.syms(b.symbol)
        if(sym == "NNP" && st.syms(a(0).symbol) == "NNP") {
          a
        } else if (sym == "NNPS" && st.syms(a(0).symbol) == "NNP") {
          b :: a.drop(1)
        } else if (sym == "NNPS" && st.syms(a(0).symbol) == "NNPS") {
          a
        } else {
          b :: a
        }
      }
    })
    new ProtoNode(n.symbol,newK.reverse)
  }

  //compress sequences of syms list into their last member
  def nnjjcompress(n : ProtoNode, st : CFGSymbolTable) : ProtoNode = {
    val syms = List("NN","NNS","JJ","NNP","NNPS")

    var doin = false
    val newK = (List[NonTerminalNode]() /: n.children)((a,b) => {
      val sym = st.syms(b.symbol)
      if(syms contains sym) {
        val r = {
          if(doin)
            b :: a.drop(1)
          else
            b :: a
        }
        doin = true    
        r
      } else {
        doin = false
        b :: a
      }
    })
    
    new ProtoNode(n.symbol,newK.reverse)
  }
}
