package multitool

import scala.collection.mutable.HashMap

object PTSG {

  def main(args : Array[String]) : Unit = {

    val (st,ptsg) = read("/home/chonger/data/PTB/ptbPTSG.txt")
    
    val train = XMLDoc.read("/home/chonger/data/PTB/train.unk.xml",st).flatMap(_.text).toList

    println("ready to train")

    ptsg.em(train,20,None)
    
    ptsg.write("/home/chonger/data/PTB/ptbPTSG-EM.txt")

  }

  def read(filE : String) : (CFGSymbolTable,PTSG) = {
    
    println("Reading a PTSG from " + filE)

    val st = new CFGSymbolTable()

    var lines = io.Source.fromFile(filE).getLines.toArray

    val nSym = lines(0).toInt
    lines = lines.drop(1)
    
    lines.slice(0,nSym).map(s => {
      st.syms.add(s.trim)
    })
    lines = lines.drop(nSym)

    val nTSG = lines(0).toInt
    lines = lines.drop(1)

    val rules = Array.tabulate(nSym)(x => new HashMap[ParseTree,Double]())

    0.until(nTSG).foreach(x => {

      val tree = st.growTree(lines(0).trim)
      val d = lines(1).toDouble
      rules(tree.root.symbol) += tree -> d
      lines = lines.drop(2)

    })

    (st,new PTSG(st,rules))
  }
  
}

class PTSG(val st : CFGSymbolTable, val rules : Array[HashMap[ParseTree,Double]]) {

  lazy val cod = new Compacter(rules.flatMap(_.iterator.map(_._1)).toList)

  def write(filE : String) = {

    println("Writing a PTSG to " + filE)
    
    import java.io._

    val bw = new BufferedWriter(new FileWriter(filE))

    bw.write(st.syms.size + "\n")
    bw.write(st.syms.strings.mkString("\n") + "\n")

    val tt = (0 /: rules)(_ + _.size)
    bw.write(tt + "\n")

    rules.foreach(_.iterator.foreach({
      case (t,d) => {
        bw.write(t.fString(st) + "\n" + d + "\n")
      }
    }))

    bw.close()
  }


  def getInsides(tree : ParseTree) = {
    val insideMap = new HashMap[RefWrapper,Double]()

    def inside(n : NonTerminalNode) : Double = {
      insideMap.getOrElseUpdate(new RefWrapper(n),{
        (0.0 /: cod.findOverlays(n))({
          case (a,(t,l)) => {
            a + (rules(t.root.symbol)(t) /: l)((x,y) => x * inside(y))
          }
        })
      })
    }

    inside(tree.root)

    insideMap
  }

  def getOutsides(tree : ParseTree, insideMap : HashMap[RefWrapper,Double]) = {
    val outsideMap = new HashMap[RefWrapper,Double]()

    outsideMap += new RefWrapper(tree.root) -> 1.0
    
    tree.nonterminals.foreach(n => {
      val rw = new RefWrapper(n)
      val out = outsideMap(rw)
      cod.findOverlays(n).foreach({
        case (t,l) => {
          val lRW = l.map(ll => new RefWrapper(ll)).toArray
          val insides = lRW.map(ll => insideMap(ll))
          val outP = out * rules(t.root.symbol)(t)
          0.until(lRW.length).foreach(lI => {
            val myRW = lRW(lI)
            val newO = (outP /: insides)((a,i) => {
              if(i == lI)
                a
              else
                a * i
            })
            outsideMap(myRW) = outsideMap.getOrElse(myRW,0.0) + newO
          })
        }
      })
    })

    outsideMap
  }

  def score(tree : ParseTree) = {
    getInsides(tree)(new RefWrapper(tree.root))
  }

  def logL(data : List[ParseTree]) : Double = {
    (0.0 /: data)((a,tree) => {
      val p = score(tree)
      if(p == 0) {
        a - 300
      } else {
        a + math.log(score(tree))
      }
    })
  }

  def em(data : List[ParseTree], iters : Int, conv : Option[Double]) : Unit = {
    var ll = logL(data)
    println("0: " + ll)
    1.to(iters).foreach(i => {
      emIter(data)
      val oLL = ll
      ll = logL(data)
      println(i + ":" + ll)
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

  def emIter(data : List[ParseTree]) {
    val expects = Array.tabulate(rules.length)(x => new HashMap[ParseTree,Double]())
    
    var fails = 0

    data.foreach(tree => {
      val insideMap = getInsides(tree)

      val norm = insideMap(new RefWrapper(tree.root))

      if(norm > 0) {

        val outsideMap = getOutsides(tree,insideMap)

        tree.nonterminals.foreach(n => {
          val rw = new RefWrapper(n)
          val out = outsideMap(rw)
          cod.findOverlays(n).foreach({
            case (t,l) => {
              val lRW = l.map(ll => new RefWrapper(ll)).toArray
              val insides = lRW.map(ll => insideMap(ll))
              val xxx = ((out * rules(t.root.symbol)(t)) /: insides)(_ * _)
              val m = expects(t.root.symbol)
              m(t) = m.getOrElse(t,0.0) + (xxx / norm)
            }
          })
        })
      } else {
        fails += 1
      }
    })

    0.until(rules.length).foreach(ind => {
      val norm = (0.0 /: expects(ind).iterator)(_ + _._2)
      expects(ind).foreach({
        case (t,d) => rules(ind)(t) = d/norm
      })
    })
    if(fails > 0)
    println("FAILs : " + fails) 
  }

}
