package multitool

import scala.collection.mutable.HashMap

object PTSG {

  def main(args : Array[String]) : Unit = {

    val (st,ptsg) = read("/home/chonger/data/PTB/ptbPTSG.txt")
    //val (st,ptsg) = read("/home/chonger/data/PTB/unnormPCFG.txt")
    
    //

  //  val ptsg2 = new PTSG(st,ptsg.rules.map(_.filter(x => isPCFG(x._1))))

//    ptsg2.write("/home/chonger/data/PTB/unnormPCFG.txt")                         

    val train = st.read("/home/chonger/data/PTB/train.debug.txt")

    println("ready to train")

    ptsg.em(train,20,None)
    
    ptsg.write("/home/chonger/data/PTB/ptbPTSG-EM.txt")

  }

  def read(filE : String) : (CFGSymbolTable,PTSG) = {
    val st = new CFGSymbolTable()
    (st,read(filE,st))
  }

  def read(filE : String, st : CFGSymbolTable) : PTSG = {
    
    println("Reading a PTSG from " + filE)

    var lines = io.Source.fromFile(filE).getLines.toArray

    val nSym = lines(0).toInt
    lines = lines.drop(1)
    
    lines.slice(0,nSym).map(s => {
      st.syms.add(s.trim)
    })
    lines = lines.drop(nSym)

    val nTSG = lines(0).toInt
    lines = lines.drop(1)

    val rules = Array.tabulate(st.syms.size)(x => new HashMap[ParseTree,Double]())

    0.until(nTSG).foreach(x => {

      val d = lines(1).toDouble

      val tree = st.growTree(lines(0).trim)
      rules(tree.root.symbol) += tree -> d

      lines = lines.drop(2)

    })

    println("PTSG created with " + (0 /: rules)(_ + _.size) + " rules")

    new PTSG(st,rules)
  }

  def mlPCFG(st : CFGSymbolTable, data : List[ParseTree], grammar : List[ParseTree]) = {

    val rules = new HashMap[ParseTree,Double]()
    val norm = new HashMap[Int,Double]()
    val smooth = 1.0
    
    grammar.foreach(g => {
      rules+= g -> smooth
      norm(g.root.symbol) = norm.getOrElse(g.root.symbol,0.0) + smooth
    })


    data.foreach(_.nonterminals.foreach(n =>{
      val nn =  new ParseTree(n.rule.node())
      rules(nn) = rules.getOrElse(nn,0.0) + 1.0
      norm(n.symbol) = norm.getOrElse(n.symbol,0.0) + 1.0
    }))

    rules.iterator.foreach({
      case (t,c) => {
        val d = c / norm(t.root.symbol)
        rules += t -> d
      }
    })

    new PTSG(st,rules)
  }

  def emPTSG(st : CFGSymbolTable, ptsg : PTSG, train : List[ParseTree], nIter : Int) = {
    val ret = new PTSG(ptsg)
    ret.em(train,nIter,Some(.0001))
    ret
  }
  
}

class PTSG(val st : CFGSymbolTable, val rules : Array[HashMap[ParseTree,Double]]) {

  val PT_BOOST = 100

  def this(ptsg : PTSG) = {
    this(ptsg.st,ptsg.rules.map(r => {
      val rr = new HashMap[ParseTree,Double]()
      rr ++= r
      rr
    }))
  }

  def this(st : CFGSymbolTable, rules : HashMap[ParseTree,Double]) = {
    this(st,{
      val g = rules.groupBy(_._1.root.symbol)
      Array.tabulate(st.syms.size)(x => g.getOrElse(x,new HashMap[ParseTree,Double]()))
    })
  }

  0.until(rules.length).foreach(ind => {
    val norm = (0.0 /: rules(ind).iterator)(_ + _._2)
    rules(ind).foreach({
      case (t,d) => rules(ind) += t -> d/norm
    })
  })

  lazy val cod = new Compacter(rules.flatMap(_.iterator.map(_._1)).toList)

  def write(filE : String) = {

    println("Writing a PTSG to " + filE)
    
    import java.io._

    val bw = new BufferedWriter(new FileWriter(filE))

    bw.write(st.syms.size + "\n")
    bw.write(st.syms.strings.mkString("\n") + "\n")

    val tt = (0 /: rules)(_ + _.size)
    bw.write(tt + "\n")

    rules.foreach(_.iterator.toArray.sortWith(_._2 > _._2).foreach({
      case (t,d) => {
        bw.write(t.fString(st) + "\n" + d + "\n")
      }
    }))

    bw.close()
  }



  def getInsides(tree : ParseTree,
                 overlays : HashMap[RefWrapper,List[(ParseTree,List[NonTerminalNode])]]) = {
    val insideMap = new HashMap[RefWrapper,Double]()

    def inside(n : NonTerminalNode) : Double = {
      val rw = new RefWrapper(n)
      insideMap.getOrElseUpdate(rw,{
        (0.0 /: overlays(rw))({
          case (a,(t,l)) => {
            val p = rules(t.root.symbol)(t) * math.pow(PT_BOOST,t.preterminals.length)
            a + (p /: l)((x,y) => x * inside(y))
          }
        })
      })
    }

    inside(tree.root)

    insideMap
  }

  def getOutsides(tree : ParseTree, 
                  insideMap : HashMap[RefWrapper,Double],
                  overlays : HashMap[RefWrapper,List[(ParseTree,List[NonTerminalNode])]]) = {
    val outsideMap = new HashMap[RefWrapper,Double]()

    outsideMap += new RefWrapper(tree.root) -> 1.0
    
    tree.nonterminals.foreach(n => {
      val rw = new RefWrapper(n)
      val out = outsideMap(rw)
      overlays(rw).foreach({
        case (t,l) => {
          val lRW = l.map(ll => new RefWrapper(ll)).toArray //refwrappers
          val insides = lRW.map(ll => insideMap(ll)) // inside probs for ntLeafs
          val outP = out * rules(t.root.symbol)(t) * math.pow(PT_BOOST,t.preterminals.length) //common rule/outside/terminal prob
          0.until(insides.length).foreach(lI => { //add in the outside for each ntLeaf
            val myRW = lRW(lI)
            val newO = (outP /: 0.until(insides.length))((a,i) => {
              if(i == lI)
                a
              else
                a * insides(i)
            })
            outsideMap(myRW) = outsideMap.getOrElse(myRW,0.0) + newO
          })
        }
      })
    })
    
    outsideMap
  }

  def score(tree : ParseTree) = {
    getInsides(tree,getOverlays(tree))(new RefWrapper(tree.root)) 
  }

  def score(tree : ParseTree, ole : OLEMap) = {
    getInsides(tree,ole)(new RefWrapper(tree.root)) 
  }

  def logL(data : List[ParseTree], overlayMap : HashMap[RefTree,OLEMap]) : Double = {
    (0.0 /: data)((a,tree) => {
      val p = score(tree,overlayMap(new RefTree(tree)))
      if(p == 0) {
        println(tree.pString(st))

        println("NO OVERLAYS AT -- ")
        overlayMap(new RefTree(tree)).filter(_._2.length == 0).foreach(x => println(x._1.n.toString(st)))
        
        tree.nonterminals.foreach(n => {
          if(! (rules contains new ParseTree(n.rule.node())))
            println("PCFG UNDEF AT " + n.toString(st))
        })

        throw new Exception()
      } else {
        a + math.log(p) - math.log(PT_BOOST)*tree.preterminals.length
      }
    })
  }

  def em(data : List[ParseTree], iters : Int, conv : Option[Double]) : Unit = {

    val oleMap = new HashMap[RefTree,OLEMap]() 
    oleMap ++= data.map(x => (new RefTree(x),getOverlays(x)))

    var ll = logL(data,oleMap)
    println("0: " + ll)
    1.to(iters).foreach(i => {
      emIter(data,oleMap)
      val oLL = ll
      ll = logL(data,oleMap)
      println(i + ": " + ll)
      conv match {
        case Some(d) => {
          if(ll - oLL < d) {
            println("Difference of " + d + " is below ll threshold")
            return
          }
        }
        case None => {/**do nothing*/}
      }
    })
  }

  def getOverlays(tree : ParseTree) = {
    val overlays = new OLEMap()
    overlays ++= tree.nonterminals.map(n => {
      (new RefWrapper(n),cod.findOverlays(n))
    })
    overlays
  }

  type OLEMap = HashMap[RefWrapper,List[(ParseTree,List[NonTerminalNode])]]

  def getEx(tree : ParseTree, overlays : OLEMap) : (List[(ParseTree,Double)],Double) = {

    val insideMap = getInsides(tree,overlays)
    val norm = insideMap(new RefWrapper(tree.root))
    val outsideMap = getOutsides(tree,insideMap,overlays)
    
    val es = tree.nonterminals.flatMap(n => {
      
      val rw = new RefWrapper(n)
      val out = outsideMap(rw)
      
      overlays(rw).map({
        case (t,l) => {
          val lRW = l.map(ll => new RefWrapper(ll)).toArray
          val insides = lRW.map(ll => insideMap(ll))
          val outP = out * rules(t.root.symbol)(t) * math.pow(PT_BOOST,t.preterminals.length) //common rule/outside/terminal prob
          val xxx = (outP /: insides)(_ * _)
          (t,xxx/norm)
        }
      }).toList
    }).toList
    
    (es,norm)
  }

  def emIter(data : List[ParseTree], overlayMap : HashMap[RefTree,OLEMap]) {

    val expects = Array.tabulate(rules.length)(x => new HashMap[ParseTree,Double]())
    
    var fails = 0

    data.par.foreach(tree => {
      val (eee,norm) = getEx(tree,overlayMap(new RefTree(tree)))
      synchronized {
        eee.foreach({
          case(t,p) => {
            val m = expects(t.root.symbol)
            m(t) = m.getOrElse(t,0.0) + p
          }
        })
      }
    })

    val smooth = .0001

    0.until(rules.length).foreach(ind => {
      val norm = ((smooth*rules(ind).size) /: expects(ind).iterator)(_ + _._2)
      rules(ind).foreach({
        case (t,d) => {
          val ex = expects(ind).getOrElse(t,0.0) + smooth
          rules(ind)(t) = ex/norm
        }
      })
    })

  }

}
