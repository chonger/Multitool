package multitool

class Token(val word : String,val stem : String,
            val pos : String,val cPos : String,
            val feats : String) {
  
}

class DGrammar() {
  import scala.collection.mutable.HashMap

  /**
   * this map stores the Token to index mapping and in Scala also provides the function for its use
   */
  var tokToInd = new HashMap[Token,Int]() 
  /**
   * this map stores the index to Token mapping and its function for use is also provided in Scala
   */
  var indToTok : List[Token] = Nil

  def apply(i : Int) = {
    indToTok(i)
  }

  def transformTokens(mapFile : String) = {
    val mapLines = io.Source.fromFile(mapFile).getLines.toList.map(_.split("\\s")).map(x => {
      (x(0).replaceAll("\\(","LRB").replaceAll("\\)","RRB"),x(1))
    })
    val mapMap = new HashMap[String,String]() ++ mapLines
    
    val newToks = indToTok.map(x => {
      new Token(x.word,x.stem,
                x.pos,mapMap(x.cPos),
                x.feats)
    })

    tokToInd.clear()
    indToTok = Nil
    newToks.foreach(t => {
      tokToInd += t -> indToTok.length
      indToTok = indToTok ::: List(t)
    })
  }


  /**
   *For when you want the Token to Index mapping and you want DGrammar to amalgamate the token if it's not found
   */ 
  def addTok(t : Token) : Int = {
    if(tokToInd.contains(t))
      tokToInd(t)
    else {
      tokToInd += t -> indToTok.length
      indToTok = indToTok ::: List(t)
      tokToInd(t)

    }
  }

  /**
   * Load trees in CONLL-X format from filE
  */

  def readCONLLTrees(lines : Iterator[String]) : List[DTree] = {

    var nTreez = 0
    var trees : List[DTree] = Nil
    var curNodes : List[DNode] = Nil
    var lastLines : List[String] = Nil

    var error = 0 
    var good = 0

    while(lines.hasNext) {
      var line = lines.next() 
      lastLines ::= line
      var parts = line.replaceAll("\\n","").replaceAll("\\(","LRB").replaceAll("\\)","RRB").split('\t')

      if(parts.length == 1) {
        try {
          nTreez += 1
          trees ::= new DTree(curNodes.toArray.reverse)
          good += 1
        } catch {
          case i : Exception => {
            System.err.println("Error creating tree number " + nTreez)
            System.err.println(i)
            lastLines.reverse.foreach(l => {
              System.err.println(l)
            })
            error += 1
          }
        }
        lastLines = Nil
        curNodes = Nil
      } else {
        parts match {
          case Array(ind,wrd,stm,pos,cpos,feat,nph,nphrel,ph,phrel) => {
            val tindex = addTok(new Token(wrd,stm,pos,cpos,feat))
            var phi = -2
            if(!ph.equals("_"))
              phi = ph.toInt-1
            curNodes ::= new DNode(ind.toInt-1,tindex,nph.toInt-1,nphrel,phi,phrel)
          }
          case _ => {
            System.err.println("This is not a good line!")
            System.err.println(line)
          }
        }
      }
    }

    System.err.println("Read " + trees.length + " trees, with " + error + " errors")
    trees.reverse
  }

  def cfgSimple(tree : DTree) : String = {
    "(S " + tree.nodes.map(n => {
      val tok = indToTok(n.token)
      "(" + tok.cPos + " " + tok.word + ")"
    }).toArray.mkString(" ") + ")"
  }

  /**
   * Return the simple string representation of a tree
  * 
  */ 
  def simpleString(tree : DTree) : String = {

    var s = ""

    def recS(n : DNode) : Unit = {
      val tok = indToTok(n.token)
      s += "(" + tok.cPos + "/" + tok.word
      val kids = tree.nodes.filter(x => {
        val hInd = if(tree.projective_?) {
          x.phead
        } else {
          x.nphead
        }
        hInd == n.index
      })

      kids.map(k => {
        s += " "
        recS(k)
      })

      s += ")"
      
    }

    recS(tree.root)

    s

  }

}
