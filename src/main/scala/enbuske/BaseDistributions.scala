package enbuske

import multitool._

abstract class BaseDistribution {
  def score(t : CSegment) : Double
  var betas : Array[Double] 
  def resampleBetas(evidence : Array[XMLDoc[CParseTree]]) : Unit
}

class CohnBase(val lowmem : LowMem, val nSyms : Int, val cutPrior : Double, val uncutPrior : Double) extends BaseDistribution {

  override var betas = (for(i <- 0 until nSyms) yield {0.5}).toArray

  def this(lm : LowMem, ns : Int, cP : Double, uP : Double, b : Array[Double]) = {
    this(lm,ns,cP,uP)
    betas = b
  }

  override def resampleBetas(evidence : Array[XMLDoc[CParseTree]]) : Unit = {
    val cCounts = Array.tabulate(nSyms)(x=>cutPrior)
    val uCounts = Array.tabulate(nSyms)(x=>uncutPrior)
    
    evidence.foreach(_.text.foreach(t => {
      t.nodez.foreach(n => {
        val mark = n.mark
        val lhs = lowmem.lhsOfRule(n.rule) 
        if(mark)
          cCounts(lhs) += 1
        else
          uCounts(lhs) += 1
      })    
    }))

    val rando = new cc.mallet.util.Randoms()

    0.until(nSyms).foreach(x => {
      betas(x) = rando.nextBeta(cCounts(x),uCounts(x))
    })
  }

  override def score(t : CSegment) : Double = {
    val firstProb = lowmem.pcfgProbs(t.tree.nodez(t.root).rule)
    (firstProb /: t.getNodes().drop(1))((a,n) => {
      val node = t.tree.nodez(n._1)
      val stub = n._2
      val lhs = lowmem.lhsOfRule(node.rule) 
      if(stub) {
        a * betas(lhs)
      } else {
        a * (1 - betas(lhs)) * lowmem.pcfgProbs(node.rule)
      }
    })
  }
}
