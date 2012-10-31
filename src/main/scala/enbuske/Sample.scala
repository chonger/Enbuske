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
    val gamma = 1000000.0 //bypasses smoothing grammar
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

/**
 *
 *    Gamma is inversely related to the amount of smoothing
 * 
 */ 
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

object CollectSamples {
  
  def apply(fBase : String, toSample : String, sampleFile : String, betweenSamples : Int, nSamples : Int) : Array[PTSG] = {

    val sampler = ESampler.continue(fBase + toSample,fBase + sampleFile)

    val st = sampler.st

    val ptsgs = sampler.getPTSGs()

    0.to(nSamples).foreach(i => {

      println("COLLECTION " + i)

      sampler.doSampling(betweenSamples,null)
      
      val newPTSGs = sampler.getPTSGs()

      (ptsgs zip newPTSGs).foreach({
        case (p1,p2) => {
          p2.rules.foreach(_.iterator.foreach(r => {
            val rr = r._1.root.symbol
            p1.rules(rr)(r._1) = p1.rules(rr).getOrElse(r._1,0.0) + r._2
          }))
        }
      })

    })

    ptsgs.foreach(p => {
      p.rules.foreach(rm => {
        rm.iterator.foreach({
          case (r,d) => rm(r) = d/(nSamples).toDouble
        })
      })
    })

    ptsgs

  }
  
}

