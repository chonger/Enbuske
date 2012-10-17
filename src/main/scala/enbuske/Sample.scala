package enbuske

import multitool._

object SampleTreebank {

 def main(args : Array[String]) = {

   val singleT = Array(Array(100.0)) //the value doesnt matter
   val singleA = Array(100.0)
   val singleG = 1000000.0
   val typeA = Array("pennTB")

   val filE = "/home/chonger/data/PTB/train.txt.unk"
   //val filE = "/home/chonger/data/PTB/train.debug.txt"

   val st = new CFGSymbolTable()
   val treez = st.read(filE)
   val pcfg = new PCFG(st,treez)
   val esampler = new ESampler(Array(new XMLDoc(treez,Array(("goldLabel","pennTB")))),st,pcfg,typeA,singleA,singleG,singleT)
   
   esampler.doSampling(1000,"/home/chonger/data/PTB/train-pp.txt")

   esampler.saveSampled("/home/chonger/data/PTB/train-sampled.txt")

   val grammar = esampler.getGrammar()

   import java.io._

   val bw = new BufferedWriter(new FileWriter("/home/chonger/data/PTB/train-grammar.txt"))
   
   grammar.foreach({
     case (t,v) => {
       bw.write(t.fString(st) + "\n")
       bw.write(v + "\n")
     }
   })

   bw.close()
 }

}

object SampleICLE {

  def main(args : Array[String]) : Unit = {

    val singleT = Array(Array(100.0)) //the value doesnt matter
    val singleA = Array(100.0)
    val singleG = 1000000.0
    val typeA = Array("pennTB")
    
    
    val unk_bnp = "/home/chonger/data/ICLE/icle_bnp_unk.xml" //unked with collapsed bnps
    val unk_normal = "/home/chonger/data/ICLE/icle_unk.xml" //unked
    
    val st = new CFGSymbolTable()
    val dox = XMLDoc.read(unk_bnp,st).map(x => new XMLDoc[ParseTree](x.text,Array(("goldLabel","pennTB"))))
    val pcfg = new PCFG(st,dox)
    val esampler = new ESampler(dox,st,pcfg,typeA,singleA,singleG,singleT)
    
    esampler.doSampling(10,"/home/chonger/data/ICLE/icle-pp.txt")
    
    esampler.saveSampled("/home/chonger/data/ICLE-sampled.txt")
    
    val grammar = esampler.getGrammar()
    
    import java.io._
    
    val bw = new BufferedWriter(new FileWriter("/home/chonger/data/ICLE/bnp-tsg.txt"))
    
    grammar.foreach({
      case (t,v) => {
        bw.write(t.fString(st) + "\n")
      }
    })
    
    bw.close()
  }

}

object ContinueSample {

  def main(args : Array[String]) = {

    val toSample = args(0)
    val nIter = args(1).toInt
    val ppFile = args(2)
    val outFile = args(3)
    val sampleFile = args(4)

    val sampler = ESampler.continue(toSample,sampleFile)

    sampler.doSampling(nIter,ppFile)

    sampler.saveSampled(outFile)
    
  }

}


/**
object CollectSamples {

  /**
   *
  * args
  * 0 - samplin data
  * 1 - number of iterations for collection
  * 2 - posteriorFile (not used)
  * 3 - outFile
  * 5 - nTopics
  * 7 - initialization file
  * 8 - number of collections
  * 9 - testFile (to build the analyzers)
  *
  */ 
  
  def main(args : Array[String]) = {

    val iterN = args(1).toInt
    val posteriorFile = args(2)
    val outFile = args(3)
    val ssSplit = args(4).toDouble
    val nTopics = args(5).toInt
    val modelType = args(6)
    val collectN = args(8).toInt
    val testF = args(9)

    val sampler = ESampler.getFromXML(args(0),ssSplit,nTopics,modelType,args(7))

    var pcfg : PCFG = null
    var typeXgrammar : Array[Array[Double]] = null
    var treeMap : Array[HashMap[ParseTree,Array[Double]]] = null
    var tset : Array[String] = null

    0.until(collectN).foreach(x => {
      sampler.doSampling(iterN,null)
      sampler.saveSampled(outFile + "__tmp")
      val anal = Analyzer.fromSampled(args(0),outFile + "__tmp",testF)
      if(pcfg == null) {
        pcfg = anal.pcfg
        tset = anal.typeset
        typeXgrammar = anal.typeXgrammar.map(_.map(x => 0.0).toArray)
        treeMap = anal.treeMap.map(x => new HashMap[ParseTree,Array[Double]]())
      }
                                 
      var i = 0

      typeXgrammar = anal.typeXgrammar.map(y => {
        var j = 0
        val r = y.map(x => {
          println("i = " + i)
          println("j = " + j)
          val r2 = typeXgrammar(i)(j) + anal.typeXgrammar(i)(j) / collectN.toDouble
          j += 1
          r2
        })
        i += 1
        r
      })
      0.until(anal.nSyms).foreach(s => {
        anal.treeMap(s).iterator.foreach(_ match {case (et,probs) => {
          val en = treeMap(s).getOrElse(et,Array.tabulate(anal.nGrammar)(x => 0.0))
          val newScores = (en zip probs).map(x => x._1 + x._2 / collectN.toDouble).toArray
          treeMap(s) += et -> newScores
        }})
      })
    })

    val analOut = new Analyzer(pcfg,typeXgrammar,treeMap,tset)

    analOut.save(outFile)

    1
  }
  
}
*/
