package enbuske

import multitool._
import scala.collection.mutable.{HashMap,HashSet}
import scala.collection.mutable.ArrayBuffer

class TopicTypeTSG(val nSyms : Int, 
                   val numTopics : Int,
                   val numTypes : Int,
                   var theta0 : Array[Array[Double]], //fake grammar counts for each topic
                   val base : BaseDistribution, 
                   val tsgAlpha : Array[Double],
                   val tsgGamma : Double,
                   val lowmem : LowMem) {

  //change name to topicXtypeCount
  var topicCount : Array[Array[Int]] = null 
  var topicTotal : Array[Int] = null 

  val rando = new cc.mallet.util.Randoms()

  var headCount : ArrayBuffer[Array[Double]] = null
  var baseCount : Array[Double] = null
  
  //stores the base count and count in each topic
  //what about having layers of topics, like a NN, will hierarchy emerge?
  type ETMapType = HashMap[CSegment,(Int,Array[Int])]
  
  var treemap : Array[ETMapType] = null  //store a seperate map for each symbol

  var getCOD : () => COD = getNoCod //this gets changed to a real COD after the first iteration
  var cod : COD = getCOD()

  clear() //sets up an empty model

  //END CONSTRUCTOR

  def getNoCod() = {
    new NoCodCompacter(lowmem,nSyms)
  }

  def getCODCOD() = {
    new LMCompacter(lowmem,nSyms)
  }

  def clear() = {
    topicCount = Array.tabulate(numTopics,numTypes)((x,y) => 0)
    topicTotal = Array.tabulate(numTypes)(x => 0)

    def newCounts() = Array.tabulate(nSyms)(x => 0.0)
    headCount = new ArrayBuffer[Array[Double]]()
    for(j <- 0 until numTopics) {headCount += newCounts()}
    baseCount = newCounts()

    treemap = (for(i <- 0 until nSyms) yield {new ETMapType()}).toArray
    cod = getCOD()
  }

  def getEmptyEntry(e : CSegment, add : Boolean) : (Int,Array[Int]) = {
    val eArr = Array.tabulate(numTopics)(x => 0)
    val lhs = lowmem.rootLHS(e)
    val ret = (0,eArr)
    if(add) {
      treemap(lhs) += e -> ret
      cod.addTree(e)
    }
    ret
  }

  def topicProb(index : Int, typ : Int) : Double = {
    val t0 = theta0(typ)
    val tSum = (0.0 /: t0)(_ + _)
    (topicCount(index)(typ) + t0(index)) / (topicTotal(typ) + tSum)
  }

  def printTreemap(st : CFGSymbolTable) = {
    println("TREEMAP")
    for(i <- 0 until nSyms) {
      treemap(i).foreach(e => {
        println(lowmem.revert(e._1).pString(st))
        println("B : " + e._2._1 + " SC : " + e._2._2.mkString(","))
      })
    }
  }

  //this should be the prob mass that comes from sitting at a real table -
  //this means either we sit at a table in this restaurant or in the base
  def tableScores(t : CSegment) : Array[Double] = {
    val lhs = lowmem.rootLHS(t)
    val (backC,counts) : (Int,Array[Int]) = treemap(lhs).getOrElse(t,getEmptyEntry(t,false))
    (for(i <- 0 until numTopics) yield {
      val h = headCount(i)(lhs)
      val r = (counts(i).toDouble + (tsgAlpha(i) * backC / (baseCount(lhs) + tsgGamma))) / (h + tsgAlpha(i)) 

      r
    }).toArray
  }

  //this is the probability of bypassing both this and the base (for each restaurant)
  def alphaScores(sym : Int) = {
    (for(i <- 0 until numTopics) yield {
      tsgAlpha(i) * tsgGamma / (baseCount(sym) + tsgGamma) / (headCount(i)(sym) + tsgAlpha(i))
    }).toArray
  }

  def scoreET(t : CSegment) : Array[Double] = {
    val lhs = lowmem.rootLHS(t)
    val (backC,counts) : (Int,Array[Int]) = treemap(lhs).getOrElse(t,getEmptyEntry(t,false))
    
    val baseScore = base.score(t)

    val ret = (for(i <- 0 until numTopics) yield {
      val h = headCount(i)(lhs)
      val b = baseCount(lhs)
      val c = counts(i)
      (c + tsgAlpha(i) * (backC + tsgGamma * baseScore) / (b + tsgGamma)) / (h + tsgAlpha(i))
    }).toArray   
    
    ret
  }

  def remET(e : CSegment, ind : Int) = {
    val lhs = lowmem.rootLHS(e)
    headCount(ind)(lhs) -= 1
    var (beta,counts) = treemap(lhs)(e) //throws if e is not in teh mapzorz
    counts(ind) -= 1

    /**
    var (beta2,counts2) = treemap(lhs)(e) //throws if e is not in teh mapzorz
    if(counts(ind) != counts2(ind))
      throw new Exception()
*/
    val tot = (0 /: counts)(_ + _)
    if(tot == 0) {
      cod.remTree(e)
      treemap(lhs) -= e
    }
  }

  def addET(e : CSegment, ind : Int) = {
    val lhs = lowmem.rootLHS(e)
    var (beta,counts) = treemap(lhs).getOrElseUpdate(e,getEmptyEntry(e,true))
    headCount(ind)(lhs) += 1
    counts(ind) += 1    
  }

  def resampleTSGBase() = {
    //println("RESAMPLING PRIOR FOR TSG OVER " + numStates + " GRAMMARS")
    //STEP 1 ---- sample the number of tables for each elementary tree in each state

    //defines a fixed ordering of elements
    val segs = 0.until(nSyms).map(sym => {
      treemap(sym).iterator.toArray
    })

    //sym X seg X state
    val tables : Array[List[Int]] = segs.map(symsegs => {
      val r : List[Int] = symsegs.map({
        case (seg,(beta,counts)) => {
          var grammarInd = 0
          val stateXtable = counts.map(count => {
            var tabC = 0
            for(i <- 0 until count) {

              //TODO : make sure we shouldnt use tabC instead of i here

              //sample against the ratio count / alpha
              if(rando.nextDouble() > (i / (tsgAlpha(grammarInd)+i)))
                tabC += 1
            }
            grammarInd += 1
            tabC
          }).toList
          //aggregate over the states to get table counts
          val rt = (0 /: stateXtable)(_ + _)
          //println(rt)
          rt
        }
      }).toList
      r
    }).toArray
    
    for(i <- 0 until nSyms) {
      val theseSegs = segs(i).toList      
      var bTot = 0
      (theseSegs zip tables(i)).foreach(_ match {
        case ((seg,(bC,counts)),newBC) => {
          //println("BETA - " + newBeta)
          bTot += newBC
          treemap(i) += (seg -> (newBC,counts))
        }
      })
      baseCount(i) = bTot
    }
  }
}
