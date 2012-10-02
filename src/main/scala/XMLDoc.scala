package multitool

class XMLDoc[A](val text : List[A], val meta : Array[(String,String)]) {
  
  def getMeta(s : String) : String = {
    val x = meta.filter(_._1 == s)
    if(x.length == 1) x(0)._2 else null
  }

  def getAttributes() = {
    def rGA(a : Array[(String,String)]) : scala.xml.MetaData = {
      if(a.length == 0)
        scala.xml.Null
      else {
        val (k,v) = a(0)
        scala.xml.Attribute("",k,v,rGA(a.drop(1)))
      }
    }
    rGA(meta)
  }

  def map[B](func : (A) => B) = {
    new XMLDoc[B](text.map(func),meta)
  }

}

object XMLDoc {

  import scala.xml._

  def scan(filE : String, func : (String) => Unit) : Unit = {
    val xml = XML.loadFile(filE)
    (xml \ "doc").foreach(doc => {
      (doc \ "s").foreach(sent => {
        func(sent.text)
      })
    })
  }

  def read[A](filE : String, func : (String) => Option[A]) : Array[XMLDoc[A]] = {
    (XML.loadFile(filE) \ "doc").map(doc => {
      val attrs = doc.attributes.iterator.map(x => (x.key,x.value.text)).toList.toArray

      val text : List[A] = (doc \ "s").flatMap(sent => {
        val res = func(sent.text)
        if(res.isDefined)
          List(res.get)
        else
          Nil
      }).toList

      new XMLDoc[A](text,attrs)

    }).toArray
  }

  def read(filE : String) : Array[XMLDoc[String]] = read(filE,x => Some(x))
  def read(filE : String, st : CFGSymbolTable) : Array[XMLDoc[ParseTree]] = read(filE,x => {
    Some(st.growTree(x))
  })

  def write[A](filE : String, docs : Array[XMLDoc[A]], func : (A) => String) : Unit = {
    import java.io._
    val bw = new BufferedWriter(new FileWriter(new File(filE)))
    
    val xml = <documents>{
      docs.map(doc => {
        val attrs = doc.getAttributes()
        <doc>{
          doc.text.map(sent => {
            <s>
            {func(sent)}
            </s>
          })
        }
        </doc> % attrs})
    }</documents>
    
    bw.write(xml.toString + "\n")

    bw.close()
  }

  def write(filE : String, docs : Array[XMLDoc[String]]) : Unit = {
    def f(x :String) = x
    write(filE,docs,f _)
  }

  def write(filE : String, docs : Array[XMLDoc[ParseTree]], st : CFGSymbolTable) : Unit = {
    def f(x : ParseTree) = x.fString(st)
    write(filE,docs,f _)
  }

  def evenSubsample[A](dox : Array[XMLDoc[A]],label : String, number : Int) = {
    val groups = dox.groupBy(_.getMeta(label))
    
    println("GROUPS")
    
    groups.foreach({
      case (l,s) => {
        println(l + " - " + s.size)
      }
    })

    groups.flatMap({
      case (l,s) => {
        if(s.size >= number) {
          Shuffle(s)
          s.slice(0,number).toList
        } else {
          Nil
        }
      }
    }).toArray
  }

}



