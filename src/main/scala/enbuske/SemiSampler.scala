package enbuske

import multitool._
import scala.collection.mutable.{HashMap,HashSet}
import java.io.{BufferedWriter,FileWriter,File}
import scala.collection.mutable.ArrayBuffer

class SemiSampler(originalDox : Array[XMLDoc[ParseTree]], 
               st : CFGSymbolTable,
               pcfg : PTSG,
               typeArr : Array[String],
               alphas : Array[Double],
               gamma : Double,
               theta : Array[Array[Double]],
               val nLabeled : Int) extends ESampler(originalDox,st,pcfg,typeArr,alphas,gamma,theta,false) {

  0.until(treez.length).foreach(x => {
    if(x < nLabeled) {
      docTyp(x) = goldIndex(x)
    } else {
      docTyp(x) = rando.nextInt(numTypes)
    }
  })

  autoLoad()

  var semiC = 0.0
  var semiT = 0.0

  override def iterInfo() : Unit = { 
    println("SEMI ACC : " + semiC/semiT)
    semiC = 0.0
    semiT = 0.0
  }

  override def resampleDoc(doc : List[CParseTree],absInd : Int) : Unit = {

    if(absInd < nLabeled)
      super.resampleDoc(doc,absInd)
    else {
      val oldTyp = docTyp(absInd)
    
      doc.foreach(tree => {
        removeSegmentsFromMap(tree,oldTyp)
      })

      val treeIProbs = doc.map(tree => {
        var overlays : Array[List[(CSegment,List[Int])]] = (for{i <- 0 until tree.nodez.length} yield {
          model.cod.findOverlays(tree,i)
        }).toArray
        0.until(numTypes).map(ty => {
          getInsideProbs(tree,ty,overlays)
        })
      })

      val margP = (Array.tabulate(numTypes)(x => 1.0) /: treeIProbs)((x,tz) => {
        0.until(numTypes).map(i => {
          val probs = tz(i)(0)._1
          val sc = (0.0 /: 0.until(numTopics))((a,b) => {
            a + model.topicProb(b,i) * probs(b)
          })
          x(i) * sc
        }).toArray
      })

      var sampInd = sampleUnNormalized(margP)
      if(sampInd == -1)
        sampInd = rando.nextInt(numTypes) 
      else {
        var best = (-1,0.0)
        0.until(numTypes).foreach(i => {
          if(margP(i) > best._2)
            best = (i,margP(i))
        })
        semiT += 1.0
        if(best._1 == goldIndex(absInd))
          semiC += 1.0
      }

      (doc zip treeIProbs).foreach({
        case (tree,iProbs) => {
          resampleDerivation(tree,iProbs(sampInd),sampInd)  
          addSegmentsToMap(tree,sampInd)
        }
      })

      docTyp(absInd) = sampInd
    }
    
  }

}
