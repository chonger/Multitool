package multitool

object BNP {

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
