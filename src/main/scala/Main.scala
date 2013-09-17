package multitool

/**
object MainSBT {

  def main(args : Array[String]) = {

    val dg = new DGrammar()
    val st = new CFGSymbolTable()

    val dtrees = dg.readCONLLTrees(io.Source.fromFile(args(0)).getLines())

    st.write(args(1),dtrees.map(_.toConstit(dg,st)))

  }

}
*/

object Convert {

  def usage() = {
    System.err.println("Please specify the input")
    System.err.println("Usage : Convert [conll|treeb]")
    System.exit(-1)
  }

  def main(args : Array[String]) = {

    val dg = new DGrammar()
    val st = new CFGSymbolTable()

    if(args.length != 2) 
      usage()
    
    args(1) match {
      case "conll" => {
        val dtrees = dg.readCONLLTrees(io.Source.stdin.getLines())
        dtrees.map(_.toConstit(dg,st)).foreach(t => {
          println(t.fString(st))
        })
      }
      case "treeb" => {
        System.err.println("Not implemented yet...")
      }
      case _ => usage()
    }

  }

}



