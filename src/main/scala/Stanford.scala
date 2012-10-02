package multitool

import edu.stanford.nlp.parser.lexparser.LexicalizedParser
import edu.stanford.nlp.ling.Sentence
import edu.stanford.nlp.trees.{PennTreebankLanguagePack,TreePrint}

import java.io._

object Stanford {

  lazy val tlp = new PennTreebankLanguagePack()
  lazy val lp = LexicalizedParser.loadModel("edu/stanford/nlp/models/lexparser/englishPCFG.ser.gz")

  lazy val st = new CFGSymbolTable()

  def conv[A](x: Array[A]) : java.util.List[A] = java.util.Arrays.asList(x : _*)

/**
  def main(args : Array[String]) : Unit = {

    val tags = Array(null,"IN",null,null,null,null)
    val toks = Array("I","like","big","butts",".")

    val tree = parse(toks)

    println(tree.fString(st))
  }
*/
  def parse(toks : Array[String]) : ParseTree = {
    parse(toks,toks.map(x => {null}))
  }

  def parse(toks : Array[String], tags : Array[String]) : ParseTree = {
    val parse = lp.apply(Sentence.toTaggedList(conv(toks),conv(tags)))
    val printer = new TreePrint("typedDependencies,wordsAndTags","includePunctuationDependencies,basicDependencies",new PennTreebankLanguagePack())
    val sw = new StringWriter()
    val pw = new PrintWriter(sw)
    printer.printTree(parse,pw)
    
    //println(sw.toString)

    val lines = sw.toString.split("\n")

    val wtags = lines(0)

    val otags = wtags.split("\\s").map(x => {
      val WT = "^.*/([^/]+)$".r
      val WT(t) = x
      t
    })

    //println(otags.mkString(" "))

    val rels = lines.drop(2).map(x => {

      val Rel = "^([^\\(]+)\\([^\\s]+-([\\d']*), [^\\s]+-([\\d']*)\\)".r
      val Rel(relType,from,to) = x

      (relType,from,to)

    })

    //println(rels.mkString("\n"))

    val deps = rels.groupBy(_._2)

//    new ParseTree(deps)

    val GetS = "^(\\d+)'*$".r

    def toN(s : String) = {
      val GetS(r) = s
      r.toInt
    }

    def makeNode(rT : String, f : String, t : String) : NonTerminalNode = {
      
      //println(rT + " " + f + " " + t)

      val tInd = toN(t)

      val mySym = otags(tInd-1)
      val myW = toks(tInd-1)

      def makeR(k : List[NonTerminalNode]) : ProtoNode = {
        if(k.length == 1) 
          new ProtoNode(st.syms.add(mySym + "-R"),k)
        else
          new ProtoNode(st.syms.add(mySym + "-R"),List(makeR(k.slice(0,k.length-1)),k(k.length-1)))
      }

      def makeL(k : List[NonTerminalNode]) : ProtoNode = {
        if(k.length == 1) 
          new ProtoNode(st.syms.add(mySym + "-L"),k)
        else
          new ProtoNode(st.syms.add(mySym + "-L"),List(k(0),makeR(k.drop(1))))
      }

      val outD : List[(String,String,String)] = deps.getOrElse(t,Array()).toList

      val lefts = outD.filter(x => toN(x._3) < tInd).map(x => makeNode(x._1,x._2,x._3)).toList
      val rights = outD.filter(x => toN(x._3) >= tInd).map(x => makeNode(x._1,x._2,x._3)).toList

      var kids = List[NonTerminalNode]()

      if(rights.length > 0) {
        //kids ::= new ProtoNode(st.syms.add(mySym + "-R"),rights)
        kids ::= makeR(rights)
      }

      kids ::= new PreTerminalNode(st.syms.add(mySym),new TerminalNode(st.terms.add(myW)))

      if(lefts.length > 0) {
        //kids ::= new ProtoNode(st.syms.add(mySym + "-L"),lefts)
        kids ::= makeL(lefts)
      }
      val rStr = if(t.indexOf("'") > 0) {rT + "_prime"} else rT

      new ProtoNode(st.syms.add(rStr),kids)

    }

    val rdep = deps("0")(0)

    new ParseTree(new ProtoNode(st.syms.add("R"),List((makeNode _).tupled(rdep))))
  }

}
