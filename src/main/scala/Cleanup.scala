package multitool

object RemoveTrace {

  def apply(t : ParseTree, st : CFGSymbolTable) = {

    def rec(n : NonTerminalNode) : NonTerminalNode = {
      val sym = st.syms(n.symbol)
      n match {
        case pn : PreTerminalNode => {
          if(sym == "-NONE-")
            null
          else {
            new PreTerminalNode(st.syms.add(sym.split("-")(0)),pn.kid)
          }
        }
        case pn : ProtoNode => {
          val kids = pn.children.map(x => rec(x)).filter(_ != null)
          if(kids.length == 0)
            null
          else
            new ProtoNode(st.syms.add(sym.split("-")(0)),kids)
        }
      }
    }

    new ParseTree(rec(t.root))

  }

}
