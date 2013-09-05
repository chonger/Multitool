package multitool 

import scala.collection.mutable.{HashSet,HashMap}

object SmallestCover {


  def main(args : Array[String]) = {

    val st = new CFGSymbolTable()
    
    val t1 = st.growTree("(R (S (X <>) (Y <>)) (B <>))")
    //val t1 = st.growTree("(S (X <>) (Y <>))")

    val t2 = st.growTree("(S (X <>) (Y (Z <>)))")
    
    val sc = apply(t1,t2)

    println(sc.pString(st))

  }
  
  def apply(a : ParseTree, b : ParseTree) : ParseTree = {

    def recComb(aa : NonTerminalNode, bb : NonTerminalNode) : NonTerminalNode = {
      //println("RC " + aa.toString(st) + " vs " + bb.toString(st))
      if(aa.symbol == bb.symbol) {
        if(aa.isInstanceOf[UnderspecifiedNode])
          return bb
        else if(bb.isInstanceOf[UnderspecifiedNode])
          return aa
      }

      if(aa.rule == bb.rule) {
        if(aa.isInstanceOf[PreTerminalNode])
          aa
        else {
          val cA = aa.asInstanceOf[ProtoNode].children
          val cB = bb.asInstanceOf[ProtoNode].children
          
          val gg = (cA zip cB).map({
            case (x,y) => {
              val r = recComb(x,y)
              if(r == null)
                return null
              r
            }
          })
                          
          new ProtoNode(aa.symbol,gg)
                
        }
      } else {
        null
      }
    }
    
    def recFill(src : NonTerminalNode, out : NonTerminalNode, in : NonTerminalNode) : NonTerminalNode = {
      if(src eq out)
        in
      else {
        src match {
          case p : ProtoNode => new ProtoNode(p.symbol,p.children.map(x => recFill(x,out,in)))
          case _ => src
        }
      }
    }

    def allComb(x : ParseTree, y: ParseTree) : List[ParseTree] = { 
      y.nonterminals.filter(z => z.rule == x.root.rule).flatMap(z => {
        //println("COMB " + (x.fString(st)))
        //println((new ParseTree(z).fString(st)) + " in " + y.fString(st))
        val r = recComb(z,x.root)
        if(r != null) {
          //println("found " + (new ParseTree(r).fString(st)))
          val rr = recFill(y.root,z,r)
          //println("made " + (new ParseTree(rr).fString(st)))
          List(new ParseTree(rr))
        } else
          Nil
      })
    }

    val ac = allComb(a,b) ::: allComb(b,a)
    val sort = ac.sortWith(_.nodes.length > _.nodes.length)
    if(sort.length == 0)
      null
    else
      sort(0)
  }



}
