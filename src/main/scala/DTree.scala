package multitool

class DNode(val index : Int, val token : Int, 
            var nphead : Int, var npRel : String,
            var phead : Int, var pRel : String) {

  override def toString() : String = {
    index + " from " + phead
  }

}

class DTree(val nodes : Array[DNode]) {

  def sent(g : DGrammar) = {
    nodes.map(n => g.indToTok(n.token).word).mkString(" ")
  }

  var projective_? : Boolean = true

  val root : DNode = {

    var rootnodes = if(projective_?) 
      nodes.filter(_.phead == -1)
    else
      nodes.filter(_.nphead == -1)

    //if we want a projective tree, but no root was found, but there's a single nonprojective head
    //then create a projective tree by raising arcs
    if(rootnodes.length == 0 && nodes.filter(_.nphead == -1).length == 1 && projective_?) {
      val deps = findDependants()

      //check if it's already projective

      var isProj_? = true
      
      (deps zipWithIndex).foreach(_ match {
        case (depList,index) => {
          //check the arc to this node's head
          //println("!!!! " + index + "," + nodes(index).nphead)
          if(nodes(index).nphead != -1) {
            val hInd = nodes(index).nphead
            
            val (big,small) = if(hInd > index) (hInd,index) else (index,hInd)

              big.until(small).foreach(x => {
                if(! (depList contains x)) {
                  //println("BIG " + big + " SMALL " + small + " deplist : " + depList.toArray.mkString(","))
                  isProj_? = false
                }
              })
          }
        }
      })


      if(isProj_?) {
        nodes.foreach(n => {
          n.phead = n.nphead
          n.pRel = n.npRel
        })
        rootnodes = nodes.filter(_.phead == -1)
      } else
        throw new Exception("Not projective...dunno what to do")
    } else if (rootnodes.length != 1) {
      throw new Exception("Error : " + rootnodes.length + " root nodes found")
    }

    rootnodes(0)
  }

  def walk(wfunc : (DNode) => Unit) : Unit = {

    def recWalk(n : DNode) : Unit = {
      wfunc(n)

      val kids = nodes.filter(x => {
        val hInd = if(projective_?) {
          x.phead
        } else {
          x.nphead
        }
        hInd == n.index
      })

      kids.map(k => recWalk(k))
    }
    
    recWalk(root)
    
  }

  def findDependants() = {
    
    var deps : Array[List[Int]] = Array.tabulate(nodes.length)(x => Nil)

    var npRoot = nodes.filter(_.nphead == -1)(0)

    def recGetDeps(n : DNode) : List[Int] = {
      
      val kids = nodes.filter(x => x.nphead == n.index)

      val allDeps = (List[Int]() /: kids)((a,b) => {
        a ::: recGetDeps(b)
      }).toList

      val ret = allDeps ::: kids.map(_.index).toList

      deps(n.index) = ret

      ret
    }

    recGetDeps(npRoot)

    deps

  }

  
  def toConstit(dg : DGrammar, st : CFGSymbolTable) = {

    val deps = nodes.groupBy(_.phead)

    def makeNode(d : DNode) : NonTerminalNode = {

      val tok = dg(d.token)
      val mySym = tok.pos
      val myW = tok.word

      def makeL(k : List[NonTerminalNode]) : ProtoNode = {
        if(k.length == 1) 
          new ProtoNode(st.syms.add(mySym + "-L"),k)
        else
          new ProtoNode(st.syms.add(mySym + "-L"),List(makeL(k.slice(0,k.length-1)),k(k.length-1)))
      }

      def makeR(k : List[NonTerminalNode]) : ProtoNode = {
        if(k.length == 1) 
          new ProtoNode(st.syms.add(mySym + "-R"),k)
        else
          new ProtoNode(st.syms.add(mySym + "-R"),List(k(0),makeR(k.drop(1))))
      }


      val lefts = deps.getOrElse(d.index,Array[DNode]()).filter(x => x.index < d.index).map(x => makeNode(x)).toList
      val rights = deps.getOrElse(d.index,Array[DNode]()).filter(x => x.index > d.index).map(x => makeNode(x)).toList

      var kids = List[NonTerminalNode]()

      if(rights.length > 0) {
        kids ::= makeR(rights)
      }

      kids ::= new PreTerminalNode(st.syms.add(mySym),new TerminalNode(st.terms.add(myW)))

      if(lefts.length > 0) {
        kids ::= makeL(lefts)
      }

      new ProtoNode(st.syms.add(d.pRel),kids)
    }


    new ParseTree(new ProtoNode(st.syms.add("ROOT"),List(makeNode(root))))
    
  }


}
