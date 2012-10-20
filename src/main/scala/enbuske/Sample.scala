package enbuske

import multitool._

abstract class SampleProg {

  def init(typeA : Array[String]) : (Array[Array[Double]],Array[Double],Double) 

  def apply(fBase : String,inF : String,nIters : Int,ppF : String,sF : String) : Unit = {

    val st = new CFGSymbolTable()
    val dox = XMLDoc.read(fBase + inF,st)

    val typeA = (new scala.collection.mutable.HashSet[String]() ++ dox.map(_.getMeta("goldLabel"))).toArray
    val nT = typeA.length

    val (theta,alphas,gamma) = init(typeA)

    val pcfg = new PCFG(st,dox)
    val esampler = new ESampler(dox,st,pcfg,typeA,alphas,gamma,theta)
    
    esampler.doSampling(nIters,fBase + ppF)
    
    esampler.saveSampled(fBase + sF)
  }

}

class SampleSingle extends SampleProg {
  override def init(typeA : Array[String]) = {
    val nT = typeA.length
    val theta = Array.tabulate(nT)(x => Array(100.0))
    val alphas = Array(100.0)
    val gamma = 1000000.0 
    (theta,alphas,gamma)
  }
}

class SampleLDA(val n : Int) extends SampleProg {
  override def init(typeA : Array[String]) = {
    val nT = typeA.length
    val theta = Array.tabulate(nT)(x => Array.tabulate(n)(y => 1.0))
    val alphas = Array.tabulate(n)(x => 100.0)
    val gamma = 1000.0 
    (theta,alphas,gamma)
  }
}

class SampleDiagonal(val gamma : Double) extends SampleProg {
  override def init(typeA : Array[String]) = {
    val nT = typeA.length
    val theta = Array.tabulate(nT)(x => Array.tabulate(nT)(y => {
      if(x == y)
        1000000
      else
        0.00001
    }))
    val alphas = Array.tabulate(nT)(x => 100.0)
    (theta,alphas,gamma)
  }
}

object ContinueSample {

  def apply(fBase : String, toSample : String, sampleFile : String, nIter : Int, ppFile : String, outFile : String) {
    
    val sampler = ESampler.continue(fBase + toSample,fBase + sampleFile)

    sampler.doSampling(nIter,fBase + ppFile)

    sampler.saveSampled(fBase + outFile)
    
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
