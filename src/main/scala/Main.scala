package multitool



object Main {

  def main(args : Array[String]) = {

    val inFile = args(0)
    val outFile = args(1)

    val dg = new DGrammar()
    val st = new CFGSymbolTable()

    val dtrees = dg.readCONLLTrees(inFile)
   
    val ctrees = dtrees.map(d => {
      d.toConstit(dg,st)
    })

    st.write(outFile,ctrees)

  }

}
