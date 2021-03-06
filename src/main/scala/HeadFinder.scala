package multitool

class HeadFinder(st : CFGSymbolTable, t : ParseTree) {
    
  import scala.collection.mutable.HashMap
  
  val headMap = new HashMap[RefWrapper,PreTerminalNode]()

  def recHead(n : NonTerminalNode) : PreTerminalNode = {
    n match {
      case pt : PreTerminalNode => pt
      case it : InternalNode => {
        val kidheads = it.children.map(x => recHead(x))
        val sym = st.syms(n.symbol)
        val ksyms = it.children.map(x => st.syms(x.symbol))

        val head = kidheads(pickHead(sym,ksyms))       
        headMap += new RefWrapper(n) -> head
        
        head
        
      }
    }
  }    
  recHead(t.root)

  def apply(n : NonTerminalNode) : PreTerminalNode = {
    n match {
      case pt : PreTerminalNode => pt
      case it : InternalNode => headMap(new RefWrapper(it))
    }
  }

  /**
   * choose the Rmost 1 (Lmost if lhs=PP,
   * then the Lmost 2, then the Rmost 3-7)
   *
   */ 
  def trimSym(s : String) = {
    if(s.indexOf("-") == 0)  //for -LRB- -RRB-
      s
    else {
      try {
        "^[^-^]+".r.findFirstIn(s).get
      } catch {
        case _ => {
            println(s)
          throw new Exception()
        }
      }
    }
  }

  def pickHead(lhs1 : String, rhs1 : List[String]) : Int = {

    val lhs = trimSym(lhs1)
    val rhs = rhs1.map(x => trimSym(x))

    val res = rhs.map(r => {
      var ret = 8
      if(HeadFinder.pref1 contains (lhs + " " + r))
        ret = 1
      else if(lhs == r)
        ret = 2
      else if(HeadFinder.pref2 contains(lhs + " " + r))
        ret = 3
      else if(HeadFinder.pts contains r)
        ret = 4
      else if(r == "PP")
        ret = 6
      else if(HeadFinder.phrasals contains r)
        ret = 5
      else if(HeadFinder.punc contains r)
        ret = 7
      ret
    })

    var best = 9

    //this switches to L most if best=1/lhs=PP or best = 2

    (0 /: res.zipWithIndex)((a,b) => {
      if((best == 1 && lhs == "PP") || best == 2 ) { //choose left
        if(b._1 < best) {
          best = b._1
          b._2
        } else
          a
      } else {
        if(b._1 <= best) {
          best = b._1
          b._2
        } else
          a
      }
    })
  }
}

object HeadFinder {

  val pref1 = Set("ADJP JJ",
                  "ADJP JJR",
                  "ADJP JJS",
                  "ADVP RB",
                  "ADVP RBB",
                  "LST LS",
                  "NAC NNS",
                  "NAC NN",
                  "NAC PRP",
                  "NAC NNPS",
                  "NAC NNP",
                  "NX NNS",
                  "NX NN",
                  "NX PRP",
                  "NX NNPS",
                  "NX NNP",
                  "NP NNS",
                  "NP NN",
                  "NP PRP",
                  "NP NNPS",
                  "NP NNP",
                  "NP $",
                  "NP POS",
                  "PP IN",
                  "PP TO",
                  "PP RP",
                  "PRT RP",
                  "S VP",
                  "S1 S",
                  "SBAR IN",
                  "SBAR WHNP",
                  "SBARQ SQ",
                  "SBARQ VP",
                  "SINV VP",
                  "SQ MD",
                  "SQ AUX",
                  "VP VB",
                  "VP VBZ",
                  "VP VBP",
                  "VP VBG",
                  "VP VBN",
                  "VP VBD",
                  "VP AUX",
                  "VP AUXG",
                  "VP TO",
                  "VP MD",
                  "WHADJP WRB",
                  "WHADVP WRB",
                  "WHNP WP",
                  "WHNP WDT",
                  "WHNP WP$",
                  "WHPP IN",
                  "WHPP TO")

  val pref2 = Set("ADJP VBN",
                  "ADJP RB",
                  "NAC NP",
                  "NAC CD",
                  "NAC FW",
                  "NAC ADJP",
                  "NAC JJ",
                  "NX NP",
                  "NX CD",
                  "NX FW",
                  "NX ADJP",
                  "NX JJ",
                  "NP CD",
                  "NP ADJP",
                  "NP JJ",
                  "NP NX",
                  "QP $",
                  "QP NN",
                  "S SINV",
                  "S SBARQ",
                  "S X",
                  "PRT RB",
                  "PRT IN",
                  "SBAR WHADJP",
                  "SBAR WHADVP",
                  "SBAR WHPP",
                  "SBARQ S",
                  "SBARQ SINV",
                  "SBARQ X",
                  "SINV SBAR",
                  "SQ VP")

  val punc = Set(".",",",":","-RRB-","-LRB-","``","''")
  val pts = Set("AUX",
                "AUXG",
                "CC",
                "CD",
                "DT",
                "EX",
                "FW",
                "IN",
                "JJ",
                "JJR",
                "JJS",
                "LS",
                "MD",
                "NN",
                "NNS",
                "NNP",
                "NNPS",
                "PDT",
                "POS",
                "PRP",
                "PRP$",
                "RB",
                "RBR",
                "RBS",
                "RP",
                "SYM",
                "TO",
                "UH",
                "VB",
                "VBD",
                "VBG",
                "VBN",
                "VBP",
                "VBZ",
                "WDT",
                "WP",
                "WP$",
                "WRB",
                "#",
                "$")

  val phrasals = Set("ADJP",
                     "ADVP",
                     "CONJP",
                     "FRAG",
                     "INTJ",
                     "LST",
                     "NAC",
                     "NP",
                     "NX",
                     "PP",
                     "PRN",
                     "PRT",
                     "QP",
                     "RRC",
                     "S",
                     "S1",
                     "SBAR",
                     "SBARQ",
                     "SINV",
                     "SQ",
                     "UCP",
                     "VP",
                     "WHADJP",
                     "WHADVP",
                     "WHNP",
                     "WHPP",
                     "X",
                     "G1",
                     "G2",
                     "G3",
                     "-NONE-")
  

}
