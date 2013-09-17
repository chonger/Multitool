package multitool

import scala.collection.mutable.{HashMap,HashSet}

class RefTree(val tree : ParseTree) {
  override def equals(any : Any) : Boolean = {
	any match {
	  case t : RefTree => {
		return tree eq t.tree
	  }
	  case _ => false
	}	
  }
  override def hashCode() : Int = tree.hashCode()
}


class Compacter(inTrees : List[ParseTree]) {

  val store = HashMap[Int,CompactTree]() 

  inTrees.foreach(t => addTree(t))

  def findOverlays(n : NonTerminalNode) : List[(ParseTree,List[NonTerminalNode])] = {
    //println("finding overlays in " + n.symbol.toInt)
    if(store contains n.symbol) {
      store(n.symbol).findCompatible(n).iterator.map(_ match {
        case (rtree,leaves) => {
          (rtree.tree,leaves.map(_.n))
        }
      }).toList
    } else {
      Nil
    }
  }

  def addTree(t : ParseTree) = {
    val r = t.root.symbol
    val ct = store.getOrElseUpdate(r,new CompactTree())
    ct.add(t.root,t)
  }

  def remTree(t : ParseTree) = {
    store(t.root.symbol).remove(t.root,t)
  }

}

class CompactTree() {

  var endsHere = new HashSet[RefTree]()

  import scala.collection.mutable.ArrayBuffer

  var children = new ArrayBuffer[HashMap[Tuple2[Int,Int],CompactTree]]()
  var terminalKids = new HashMap[Int,CompactTree]()

  def isEmpty() = {
    endsHere.size == 0 &&
    children.length == 0 &&
    terminalKids.size == 0
  }

  def recPrint(st : CFGSymbolTable) : Unit = {
    println("Compact Tree Node")
    
    println("These segments end here")
    endsHere.foreach(e => {
      println(e.tree.fString(st))
    })
    
    println("Here are my children")
    var ind = 0
    children.iterator.foreach(c => {
      println("Kids at " + ind)
      ind += 1
      c.iterator.foreach(_ match {
        case (key,ctree) => {
          println("KEY - " + st.syms(key._1) + " --- Arity : " + key._2)
          ctree.recPrint(st)
        }
      })
    })

    println("Terminal kids")
    terminalKids.iterator.foreach(_ match {
      case (key,ctree) => {
        println("T KEY - " + st.terms(key))
        ctree.recPrint(st)
      }
    })

    
    println("DONE printin")
  }

  def add(node : TreeNode, tree : ParseTree) : Unit = {
    node match {
      case un : UnderspecifiedNode => {
        endsHere += new RefTree(tree)
      }
      case tn : TerminalNode => {
        endsHere += new RefTree(tree)
      } 
      case pn : NonTerminalNode => {
        var kidDex = 0
        val arity = pn.children.length

        pn.children.foreach(c => {
          var cMap = if(children.length <= kidDex) {
            var nMap = new HashMap[Tuple2[Int,Int],CompactTree]()
            children append nMap
            nMap
          } else {
            children(kidDex)
          }

          c match {
            case in : NonTerminalNode => {
              val entry : CompactTree = cMap.getOrElseUpdate((in.symbol,arity),new CompactTree())
              entry.add(in,tree)
            }
            case tn : TerminalNode => {
              val entry = terminalKids.getOrElseUpdate(tn.terminal,new CompactTree())
              entry.add(tn,tree)
            }
          }

          kidDex += 1
        })
      }
    }
  }

  def remove(node : TreeNode, tree : ParseTree) : Unit = {
    node match {
      case un : UnderspecifiedNode => {
        endsHere.iterator.foreach(el => {
          if(el.tree == tree) 
            endsHere -= el
        })
      }
      case tn : TerminalNode => {
        endsHere.iterator.foreach(el => {
          if(el.tree == tree) {
            endsHere -= el
          }
        })
      } 
      case pn : NonTerminalNode => {
        var kidDex = 0
        val arity = pn.children.length

        pn.children.foreach(c => {
          val cMap = children(kidDex)

          c match {
            case in : NonTerminalNode => {
              val k = (in.symbol,arity)
              val entry : CompactTree = cMap(k)
              entry.remove(in,tree)
              if(entry.isEmpty())
                cMap -= k
            }
            case trn : TerminalNode => {
              val entry = terminalKids(trn.terminal)
              entry.remove(trn,tree)
              if(entry.isEmpty())
                terminalKids -= trn.terminal
            }
          }

          kidDex += 1
        })
      }
    }
  }

  def findCompatible(targNode : NonTerminalNode) : HashMap[RefTree,List[RefWrapper]] = {

    var ret = new HashMap[RefTree,List[RefWrapper]]()
    endsHere.foreach(e => {
      ret += e -> List(new RefWrapper(targNode))
    })

    targNode match {
      case un : UnderspecifiedNode => {

      }
      case ptn : PreTerminalNode => {
        val term = ptn.kid.terminal
        val entry : CompactTree = terminalKids.getOrElse(term,null)
        if(entry != null)
          entry.endsHere.foreach(e => ret += e -> Nil)
      }
      case nn : ProtoNode => {
        val myArity = nn.children.length
        if(myArity > children.length)
          return ret

        var index = 0
        val compatMaps = nn.children.map(c => {
          val bin = children(index)
          index += 1

          val entry = bin.getOrElse((c.symbol,myArity),null)
          if(entry != null) {
            entry.findCompatible(c)
          } else {
            //println("No segments found, aborting at " + Compacter.pcfg.symbolStrings(c.symbol) + "/" + myArity)
            return ret
          }
        })

        var intersect = compatMaps(0)
        compatMaps.drop(1).foreach(m => {
          val keys = intersect.keySet
          keys.foreach(k => {
            if(! m.keySet.contains(k)) {
              intersect -= k
            } else {
              val en = intersect(k)
              intersect += k -> (en ::: m(k))
            }
          })
        })
        intersect.iterator.foreach(e => ret += e)
      }
    }
    //end of case

    ret


  }


}
