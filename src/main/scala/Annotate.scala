package multitool

class UnaryAnnotate(st : CFGSymbolTable) {

  def apply(tree : ParseTree) : ParseTree = {
    
    def recProc(n : NonTerminalNode, parent : String) : NonTerminalNode = {
      var mySym = st.syms(n.symbol)

      n match {
        case pn : ProtoNode => {

          if(pn.children.length == 1)
            mySym += "-U"

          val newSym = st.syms.add(mySym)
          new ProtoNode(newSym,pn.children.map(x => recProc(x,mySym)))
        } 
        case ptn : PreTerminalNode => {
          
          //no siblings (DT and RB)

          val newSym = st.syms.add(mySym)
          new PreTerminalNode(newSym,ptn.kid)
        }
      }
    }

    val rSym = st.syms(tree.root.symbol)
    new ParseTree(new ProtoNode(tree.root.symbol,tree.root.asInstanceOf[ProtoNode].children.map(c => recProc(c,rSym))))

  }
}

class ParentAnnotate(data : List[ParseTree], val st : CFGSymbolTable) {

  import scala.collection.mutable.{HashSet,HashMap}

  val doAnn = new HashSet[String]()

  {
    val pts = new HashMap[String,Int]()
    val ps = new HashMap[String,Int]()

    data.foreach(t => {
      t.nonterminals.foreach(_ match {
        case n : ProtoNode => {
          val s = st.syms(n.symbol)
          ps(s) = ps.getOrElse(s,0) + 1
        }
        case n : PreTerminalNode => {
          val s = st.syms(n.symbol)
          pts(s) = pts.getOrElse(s,0) + 1
        }
      })
    })

    pts.iterator.toArray.sortWith(_._2 > _._2).slice(0,(pts.size * .75).toInt).foreach({
      case (s,c) => doAnn += s
    })

    ps.iterator.toArray.sortWith(_._2 > _._2).slice(0,(ps.size * .5).toInt).foreach({
      case (s,c) => doAnn += s
    })

  }

  def apply(tree : ParseTree) : ParseTree = {
    
    def recProc(n : NonTerminalNode, parent : String) : NonTerminalNode = {
      val mySym = st.syms(n.symbol)

      var comb = if((doAnn contains mySym) && (doAnn contains parent)) {
        mySym + "^" + parent
      } else {
        mySym
      }

      n match {
        case pn : ProtoNode => {

          //basenp?

          //dominates vp?

          

          val newSym = st.syms.add(comb)
          new ProtoNode(newSym,pn.children.map(x => recProc(x,mySym)))
        } 
        case ptn : PreTerminalNode => {
          
          //no siblings (DT and RB)

          val newSym = st.syms.add(comb)
          new PreTerminalNode(newSym,ptn.kid)
        }
      }
    }

    val rSym = st.syms(tree.root.symbol)
    new ParseTree(new ProtoNode(tree.root.symbol,tree.root.asInstanceOf[ProtoNode].children.map(c => recProc(c,rSym))))

  }

  def apply(dox : Array[XMLDoc[ParseTree]]) : Array[XMLDoc[ParseTree]] = {
    dox.map(_.map(x => apply(x)))
  }

}

