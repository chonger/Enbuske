package enbuske

import multitool._
import scala.collection.mutable.{HashMap}

/**
 * 
 *
 */ 

class MParseTree(val nodez : Array[CNode], val clusters : Array[Int]) extends CParseTree(nodez) {

}

class MSegment(val tree : CParseTree, val root : Int, 
               val marks : Array[Boolean], val clusters : Array[Int]
             ) extends CSegment(tree,root,marks) {

  override def nodesEq(t : CSegment) = {
    if(myHash != t.myHash)
      return false
	val myNodes = getNodes()
    val oNodes = t.getNodes()
    if(myNodes.length != oNodes.length)
      return false
    (myNodes zip oNodes).foreach(_ match {
      case ((mI,mS),(oI,oS)) => {
        if(mS != oS)
          return false
        val mN = tree.nodez(mI)
        val oN = t.tree.nodez(oI)
        if(!mS) { //if they're stubs then they automatically match
          if(mN.rule != oN.rule)
            return false
        }
      }
    })
    true
  }

  override def nodesHash() = {
    val nodez = getNodes()
    var shift = 0
    (0 /: nodez)((a,n) => {
      val nod = tree.nodez(n._1)
      var rule = if(n._2) {
        0
      } else {
        if(nod.isTerm)
          clusters(n._1)
        else
          nod.rule
      }
      val ret = a ^ (rule << shift)
      shift += 2
      if(shift > 28)
        shift = 0
      ret
    })
  }

}


class MTopicTypeTSG(val nSyms : Int, 
                    val numTopics : Int,
                    val numTypes : Int,
                    var theta0 : Array[Array[Double]], //fake grammar counts for each topic
                    val base : BaseDistribution, 
                    val tsgAlpha : Array[Double],
                    val tsgGamma : Double,
                    val lowmem : LowMem) extends TopicTypeTSG(nSyms,numTopics,numTypes,theta0,base,tsgAlpha,tsgGamma,lowmem) {

  override def pcfgScore(n : CNode) = {
    if(n.isTerm) {
      val p = lowmem.pcfgProbs(n.rule) //start with the pcfg prob
      //multiply in sum_Cluster{p(Cluster|POS)p(W|Cluster)} - for efficiency we need the nonzero P(W|C) for a W
      p
    } else {
      lowmem.pcfgProbs(n.rule)      
    }
  }

  override def tableScores(t : CSegment) : Array[Double] = {

    null
    
  }

}

class MSampler(originalDox : Array[XMLDoc[ParseTree]], 
               st : CFGSymbolTable,
               pcfg : PTSG,
               typeArr : Array[String],
               alphas : Array[Double],
               gamma : Double,
               theta : Array[Array[Double]],
               wordClusters : Array[HashMap[String,HashMap[String,Double]]]
             ) extends ESampler(originalDox,st,pcfg,typeArr,alphas,gamma,theta,true) {

  

  /**
   * 
   getInsideProbs  doInside - assigning primeprob was just an array of pcfg prob from each - now what?
   *    its 
   *
  *    this fixes prime - we can choose which one it was when sampling down.
  * 

* 

*/


  


}
