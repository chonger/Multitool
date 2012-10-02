import multitool._


object PennTB {

  def mainr(args : Array[String]) = {

    val lines = io.Source.fromFile("/home/chonger/data/")
    

  }

}

object ICLE_Prep {

  def main(args : Array[String]) = {

    val st = new CFGSymbolTable()

    val dox = XMLDoc.read("/home/chonger/data/ICLE/icle_all.xml",st)

    val dox2 = XMLDoc.evenSubsample(dox,"goldLabel",200)

    val b = new Binarizer(st)
    val dox3 = dox2.map(d => {
      d.map(s => {
        b.revert(s)
      })
    })

    XMLDoc.write("/home/chonger/data/ICLE/icle_unbinarized.xml",dox3,st)

  }

}

object ComparePOS {

  def main(args : Array[String]) = {

    import java.io._

    val st = new CFGSymbolTable()
    val dox = XMLDoc.read("/home/chonger/data/ICLE/icle_unbinarized.xml",st)

    val bw = new BufferedWriter(new FileWriter("/home/chonger/dump/comparePOS.txt"))

    dox.foreach(d => {
      d.text.foreach(s => {
        val t = s.sentence(st)
        val words = s.terminals.map(x => st.terms(x.terminal)).toArray
        val otags = OpenNLP.postag(t)
        val btags = s.preterminals.map(x => st.syms(x.symbol)).toArray

        val ndiffs = (otags zip btags).filter(x => x._1 != x._2).length

        if(ndiffs == 1) { 
          val str = "\n" + t + "\nMAXENT : " + otags.mkString(" ") + "\nBPARSE : " + btags.mkString(" ")
          println(str)
          bw.write(str + "\n")
        } else {
          println(".")
        }

      })
    })

    bw.close()
  }

}


object BNP {

  def main(args : Array[String]) = {
    val st = new CFGSymbolTable()

    def collapse(n : NonTerminalNode) : NonTerminalNode = {
	  n match {
	    case ptn : PreTerminalNode => new PreTerminalNode(ptn.symbol,new TerminalNode(ptn.kid.terminal))
	    case pn : ProtoNode => { 
          var last = -1
          val kids = (List[NonTerminalNode]() /: pn.children)((a,b) => {
            val str = st.syms(b.symbol)
            val r = if(b.symbol == last && (str == "NNP"))
              a
            else
              b :: a 
            last = b.symbol
            r
          }).reverse

          new ProtoNode(pn.symbol,kids.map(x => collapse(x)))
          
        }
	  }
    }

    val dox = Unker.swunk(XMLDoc.read("/home/chonger/data/ICLE/icle_unbinarized.xml",st),st)

    import scala.collection.mutable.HashMap

    val yields = new HashMap[String,(String,Int)]()

    val nouns = List("NN","NNS","NNP","NNPS")

    dox.foreach(d => d.text.foreach(t => {
      
      t.getBNPs(st).filter(b => {
        true
        //(true /: new ParseTree(b).nonterminals.map(x => st.syms(x.symbol)))((a,b)=> a && !(nouns contains b))
      }).foreach(b => {
        //val b = collapse(b2)

        val s = b.toString(st)
        if(!(yields.keySet contains s))
          yields += s -> (new ParseTree(b).sentence(st),1)
        else {
          val e = yields(s)
          yields += s -> (e._1,e._2+1)
        }
      })

    }))

    import java.io._

    val bw = new BufferedWriter(new FileWriter("/home/chonger/dump/icle_bnps2.txt"))

    println(yields.size + " distinct BNPs")
    var c = 0
    yields.iterator.toArray.sortWith(_._2._2 > _._2._2).foreach(y => {
      c += 1
      println(c + ": " + y._1 + " -- " + y._2)
      bw.write(y._1 + " -- " + y._2 + "\n")
    })

    bw.close()

  }

}
