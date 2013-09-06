package multitool

/**
 * 
 *  Input Types
 *    - CONNL
 *    - TreeBank
 *
 *  Operations
 *    - convert format
 *    - un/binarize (TB)
 *    - unker (Both)
 *    - annotate (TB)
 *
 *
 */ 


object Main {

  def main(args : Array[String]) = {

    val dg = new DGrammar()
    val st = new CFGSymbolTable()

    val dtrees = dg.readCONLLTrees(io.Source.stdin.getLines())

    dtrees.map(_.toConstit(dg,st)).foreach(t => {
      println(t.fString(st))
    })

  }

}
