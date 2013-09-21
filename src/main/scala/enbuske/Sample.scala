package enbuske

import multitool._

abstract class SampleProg {

  /**
   *  From an array of type names, produce thetas, alphas, and gamma
   */ 
  def init(typeA : Array[String]) : (Array[Array[Double]],Array[Double],Double) 

  /**
   *  Semi-supervised sampling
   */ 
  def apply(inF : String, semiF : String, nIters : Int,ppF : String,sF : String) : Unit = {
    val st = new CFGSymbolTable()
    val dox = XMLDoc.read(inF,st)
    val sdox = XMLDoc.read(semiF,st)

    val typeA = (new scala.collection.mutable.HashSet[String]() ++ dox.map(_.getMeta("goldLabel"))).toArray
    val nT = typeA.length

    val (theta,alphas,gamma) = init(typeA)
    
    val allDox = (dox.toList ::: sdox.toList).toArray
    val pcfg = PTSG.mlPCFG(st,allDox.flatMap(_.text).toList)

    val esampler = new SemiSampler(allDox,st,pcfg,typeA,alphas,gamma,theta,dox.length)
    esampler.doSampling(nIters,ppF)
    esampler.saveSampled(sF)
  }

  /**
   *  Supervised Sampling
   */ 
  def apply(inF : String,nIters : Int,ppF : String,sF : String) : Unit = {

    val st = new CFGSymbolTable()
    val dox = XMLDoc.read(inF,st)

    val typeA = (new scala.collection.mutable.HashSet[String]() ++ dox.map(_.getMeta("goldLabel"))).toArray
    val nT = typeA.length

    val (theta,alphas,gamma) = init(typeA)

    val pcfg = PTSG.mlPCFG(st,dox.flatMap(_.text).toList)
    val esampler = new ESampler(dox,st,pcfg,typeA,alphas,gamma,theta)
    
    esampler.doSampling(nIters,ppF)    
    esampler.saveSampled(sF)
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

class SampleMix(val n : Int) extends SampleProg {
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

  /**
   *   The parameters are stored in the sample file
   */ 

  def apply(toSample : String, sampleFile : String, nIter : Int, ppFile : String, outFile : String) {
    val sampler = ESampler.continue(toSample,sampleFile)
    sampler.doSampling(nIter,ppFile)
    sampler.saveSampled(outFile)
  }
}

/**
 *
 *   Monte Carlo integral over derivations
 * 
 */ 
object CollectSamples {
  
  def apply(toSample : String, sampleFile : String, betweenSamples : Int, nSamples : Int) : Array[PTSG] = {

    val sampler = ESampler.continue(toSample,sampleFile)

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

