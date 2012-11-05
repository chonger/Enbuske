package enbuske

import multitool._
import scala.collection.mutable.{HashMap,HashSet}
import java.io.{BufferedWriter,FileWriter,File}
import scala.collection.mutable.ArrayBuffer

object ESampler {

  def create(filE : String,typeArr : Array[String], gamma : Double, alphas : Array[Double], theta : Array[Array[Double]]) = {
    val st = new CFGSymbolTable()
    val dox = XMLDoc.read(filE,st)
    val pcfg = new PCFG(st,dox)
    new ESampler(dox,st,pcfg,typeArr,alphas,gamma,theta)
  }

  def continue(filE : String, sampFile : String) = {    
    val st = new CFGSymbolTable()
    val dox = XMLDoc.read(filE,st)
    val pcfg = new PCFG(st,dox)

    println("Initializing the sampler with a previously sampled TSG")

    var lines = io.Source.fromFile(sampFile).getLines.toArray

    val gamma = lines(0).toDouble
    val numTopics = lines(1).toInt
    val nTypes = lines(2).toInt
    lines = lines.drop(3)

    val alphas = lines.slice(0,numTopics).map(_.toDouble)
    lines = lines.drop(numTopics)
        
    val typeArr = lines.slice(0,nTypes).map(_.trim).toArray
    lines = lines.drop(nTypes)

    val theta = 0.until(nTypes).map(x => {
      0.until(numTopics).map(y => {
        lines(x * numTopics + y).toDouble
      }).toArray
    }).toArray
    lines = lines.drop(nTypes*numTopics)

    val sampler = new ESampler(dox,st,pcfg,typeArr,alphas,gamma,theta,false)

    sampler.fileLoad(lines)

    sampler
  }


}

class ESampler(originalDox : Array[XMLDoc[ParseTree]], 
               val st : CFGSymbolTable,
               val pcfg : PCFG,
               val typeArr : Array[String],
               alphas : Array[Double],
               gamma : Double,
               theta : Array[Array[Double]],
               autoload : Boolean) {
  
  def this(originalDox : Array[XMLDoc[ParseTree]], 
           st : CFGSymbolTable,
           pcfg : PCFG,
           typeArr : Array[String],
           alphas : Array[Double],
           gamma : Double,
           theta : Array[Array[Double]]) {
    this(originalDox,st,pcfg,typeArr,alphas,gamma,theta,true)
  } 

  val numTypes = typeArr.length
  val numTopics = alphas.length

  assert(numTypes == theta.length, {throw new Exception("Type number error")})
  theta.foreach(t => {
    assert(t.length == numTopics, {throw new Exception("Topic number error")})
  })

  /**
   *
   *  For visualization
   *
   */
  var totalTrees = 0
  var processedTrees = 0
  var totalIters = 0
  var doneIters = 0
  var totalChanged = 0

  var times : List[Float] = Nil
  var llikes : List[Float] = Nil
  var cacheSzs : List[Float] = Nil
  var percChanged : List[Float] = Nil

  var fails = 0

  /**
   *                    CONSTRUCTOR STUFF
   *
   *    Main fields
   */ 

  var model : TopicTypeTSG = null
  val rando = new cc.mallet.util.Randoms()
  var lowmem = new LowMem(pcfg,st)
  val treez = lowmem.compactCorpus(originalDox)
  val nDocs = treez.length
  
  def goldIndex(i : Int) = {
      val nlang = treez(i).getMeta("goldLabel")
      val gT = typeArr.indexOf(nlang)
      if(gT == -1)
        throw new Exception("No type " + nlang + " in - " + typeArr.mkString(" "))
      gT
  }  

  println("Enbuske Sampler Online")
  println()
  println("Using these types : " + typeArr.mkString(" "))
  println("   This is " + (0 /: originalDox)(_ + _.text.length) + " treez")
  println(" There are " + numTopics + " topics and " + numTypes + " types")
  println()

  
  /**
   *   docTyp is an array of type indices, one for each document
   */ 
  val docTyp = (treez zipWithIndex).map(x => {
    goldIndex(x._2)
  }).toArray

  val nSyms = st.syms.size

  //WARNING MAGIC NUMBERS IN BASE CONSTRUCTOR
  model = new TopicTypeTSG(nSyms,numTopics,numTypes,theta,new CohnBase(lowmem,nSyms,100,100),alphas,gamma,lowmem)  

  if(autoload) {
    autoLoad()
  }

  def autoLoad() {
    //STEP 1 - insert all the information in caches
    println("Autoloading caches")
    
    //randomly set the aspect (topic) for each node in the data
    (treez zip docTyp).foreach(_ match {
      case (doc,typ) => {
        doc.text.foreach(s => {
          s.nodez.foreach(n => {
            n.aspect = rando.nextInt(numTopics).toChar            
          })
        })
      }
    })
    
    (0.until(treez.length) zip docTyp).foreach(_ match {
      case (docI,typ) => {
        treez(docI).text.foreach(tree => {
          var overlays : Array[List[(CSegment,List[Int])]] = (for{i <- 0 until tree.nodez.length} yield {
            model.cod.findOverlays(tree,i)
          }).toArray
          val iProbs = getInsideProbs(tree,typ,overlays)
          resampleDerivation(tree,iProbs,typ)  
          addSegmentsToMap(tree,typ)
        })             
      }
    })
    //STEP 2 - instantiate the TSG base
    model.resampleTSGBase()
    model.base.resampleBetas(treez)
  }

  def fileLoad(lines : Array[String]) = {
    var tInd = 0
    (treez zip docTyp).foreach(_ match {
      case (doc,typ) => {
        doc.text.foreach(tree => {
          val as = lines(tInd).split(",").toList
          val ms = lines(tInd+1).split("").toList.drop(1)
          tInd += 2
          if(ms.length != tree.nodez.length)
            throw new Exception()
          if(as.length != tree.nodez.length) {
            throw new Exception()
          }        
          (as zip tree.nodez).foreach(x => {
            x._2.aspect = x._1.toInt.toChar
          })
          (ms zip tree.nodez).foreach(x => {
            if(x._1 == "1") {
              x._2.mark = true
            } else 
              x._2.mark = false
          })
          addSegmentsToMap(tree,typ)
        })
      }
    })

    model.resampleTSGBase()
    model.base.resampleBetas(treez)
  }
   
  /**
   *
  *          END CONSTRUCTOR
  *
  *
  */

  def sampleUnNormalized(a : Array[Double]) : Int = {
    val sum = (0.0 /: a)(_ + _)
    if(sum == 0)
      return -1
    val r = rando.nextDouble() * sum
    var acc = 0.0
    0.until(a.length).foreach(aI => {
      acc += a(aI)
      if(acc > r)
        return aI
    })
    -1
  }

  def getPTSGs() : Array[PTSG] = {
    0.until(numTypes).map(i => {
      val rM = getGrammar(i).groupBy(_._1.root.symbol)
      val rules : Array[HashMap[ParseTree,Double]] = Array.tabulate(st.syms.size)(x => {
        new HashMap[ParseTree,Double]() ++ rM.getOrElse(x,Array[(ParseTree,Double)]())
      })
      new PTSG(st,rules)
    }).toArray
  }

  def getGrammar(typeInd : Int) : Array[(ParseTree,Double)] = {
    val rawCounts = new HashMap[ParseTree,Double]()

    val tz = new HashSet[CSegment]() ++ model.treemap.flatMap(_.map(_._1))

    tz ++= TreeTools.cfgSet(tz.map(x => lowmem.revert(x)).toList).map(x => {
      lowmem.convert(new ParseTree(x.root) with Markers)
    }).map(x => {
      new CSegment(x,0,x.nodez.map(y => true))
    }).toList

    tz.foreach(seg => {

      val scores = model.scoreET(seg)

      val p = (0.0 /: 0.until(numTopics))((a,i) => {
        model.topicProb(i,typeInd) * scores(i)
        })
      
      rawCounts += lowmem.revert(seg) -> p
      
    })
    
    val norm = new HashMap[Int,Double]() 

    rawCounts.foreach({
      case (t,c) => {
        norm(t.root.symbol) = norm.getOrElse(t.root.symbol,0.0) + c.toDouble
      }
    })

    rawCounts.iterator.toArray.map({
      case (t,c) => {
        (t,c.toDouble/norm(t.root.symbol))
      }
    })

  }


  def saveSampled(filE : String) = {
    
    val bw = new BufferedWriter(new FileWriter(new File(filE)))
    
    bw.write(model.tsgGamma.toString + "\n")
    bw.write(model.numTopics.toString + "\n")
    bw.write(model.numTypes.toString + "\n")
    
    bw.write(model.tsgAlpha.mkString("\n") + "\n")
    bw.write(typeArr.mkString("\n") + "\n")

    model.theta0.foreach(_.foreach(x => bw.write(x + "\n")))

    println("writing " + treez.flatMap(_.text).length + " treez")
    treez.flatMap(_.text).map(tree => {
      bw.write(tree.nodez.map(x => {
        x.aspect.toInt.toString
      }).mkString(",") + "\n")
      bw.write(tree.nodez.map(x => {
        if(x.mark)
          "1"
        else
          "0"
      }).mkString("") + "\n")
    })
    
    bw.close()
  }
  
  //add all the elementary trres specified by a tree's markers to the maps
  def addSegmentsToMap(t : CParseTree, label : Int) = {
    t.getSegments.foreach(e => {
      model.topicCount(e.rNode().aspect)(label) += 1
      model.topicTotal(label) += 1
      model.addET(e,e.rNode().aspect)
    })
  }

  //remove all the ET's for a tree from the maps
  def removeSegmentsFromMap(t : CParseTree, label : Int) = {
    t.getSegments.foreach(e => {
      model.topicCount(e.rNode().aspect)(label) -= 1
      model.topicTotal(label) -= 1
      model.remET(e,e.rNode().aspect)
    })
  }
  
  //calculate log likelihood
  def calcLL() : Double = {

    model.clear()

    val ll = (0.0 /: (treez zipWithIndex))((x,t) => {
      t match {
        case (doc,dI) => {
          val dTyp = docTyp(dI)
          var tsgProb = 0.0
          var topProb = 0.0
          doc.text.foreach(tree => {
            tree.getSegments.foreach(et => {
              var modelScore = model.scoreET(et)(et.rNode().aspect)
              val topicScore = model.topicProb(et.rNode().aspect,dTyp)
              if(modelScore == 0) {
                modelScore = math.pow(10,-300)
                println("NEG model")
              }
              if(topicScore == 0) {
                println("NEG topic")
              }
              
              tsgProb += math.log(modelScore)
              topProb += math.log(topicScore)
            })
            addSegmentsToMap(tree,dTyp)
          })
          x + tsgProb + topProb
        }
      }
    })
    ll
  }

  def doSampling(iters : Int, filE : String) = {

    times = Nil
    llikes = Nil
    cacheSzs = Nil
    percChanged = Nil
    totalIters = iters
    totalTrees = treez.length
    processedTrees = 0
    doneIters = 0
    totalChanged = 0

    println("Block Sampling for " + iters + " iterations")
    println("I = iteration number")
    println("T = iteration elapsed time")
    println("L = log likelihood")
    println("Z = number of cached trees")
//    println("C = percent of nodes that were changed")
    println("ACC = accuracy on unsupervised portion")
    println("FAIL = number of underflows")
    println()
    var bw : BufferedWriter = null
    if(filE != null)
      bw = new BufferedWriter(new FileWriter(new File(filE)))
    
    var doLL = true
    var doMem = true
    
    var totalNodes = 0
    treez.foreach(doc => doc.text.foreach(t => totalNodes += (t.nodez.length - 1)))

    val rang = 0.until(treez.length).toArray

    var time = System.currentTimeMillis()
    for(i <- 1 to iters) {
      fails = 0
      Shuffle(rang)
      rang.foreach(r => {
        processedTrees += 1

        val t = treez(r)

        resampleDoc(t.text,r)

      })

      model.getCOD = model.getCODCOD //after the first iter, use a real COD

      model.resampleTSGBase()

      model.base match {
        case cb : CohnBase => {
          cb.resampleBetas((treez zipWithIndex).map(_._1))
        }
      }

      var lind = 0
      (typeArr zipWithIndex).foreach(l => {
        val topics = model.topicCount.map(_(l._2))
        println(l._1 + " -   " + topics.mkString("\t"))
      })

      //DISPLAY
      
      val dispString = i

      val cTime = System.currentTimeMillis()
      val eTime = (cTime - time).toDouble / 1000.0
      times ::= eTime.toFloat
      val timeString = eTime.toString
      time = cTime
      
      val llString = if(doLL && i % 1 == 0) {
        val ll = calcLL()
        llikes ::= ll.toFloat
        if(filE != null) {
          bw.write(ll + "\n")
          bw.flush()
        }
        ll.toString
      } else {
        "?"
      }

      val cSz = (0 /: model.treemap)(_ + _.size)
      val sizeStr = cSz.toString
      cacheSzs ::= cSz

      val chg = totalChanged.toDouble / totalNodes.toDouble
      percChanged ::= chg.toFloat
      val chgStr = (((chg * 10000).toInt).toFloat / 100).toString + "(" + totalChanged + ")"

      var classTot = Array.tabulate(model.numTypes)(x => 0)
      var semiTot = Array.tabulate(model.numTypes)(x => 0)
        
      println("I-(" + dispString + ")\tT>{" + timeString + "}\tL_[" + llString + "]\tZ<" + sizeStr + 
              //">\tC!" + chgStr + 
              "\tFAIL=" + fails)
  
      iterInfo()
      
      processedTrees = 0
      totalChanged = 0
      doneIters += 1
    }

    if(filE != null)
      bw.close()
  }

  def iterInfo() : Unit = {
    //do nothing
  }

  //Short for Substitution Move.  
  //arg1 -  a specific et substitution, or a transition to the prime grammar (null)
  //arg2 -  the indexes of the leaves of this substitution (Nil if GOTO')
  //arg3 -  the probability of this move in each of the grammars.
  type SubMove = (CSegment,List[Int],Array[Double])
    
  //Short for InsideProbability, stored for each node n
  //these are the cell contents in the dynamic program
    //
  //the first two args are stored for each topic
    //
  //arg1 - P(full subtree of n | n was a subst. site)
  //arg2 - P(full subtree of n | n was expanded in ')
  //arg3 - All possible moves to start an et from n
  type IProb = (Array[Double],Array[Double],List[SubMove])

    /**
     *  MAIN LOOP!
     *
     *  remove and add segments are keyed by the type.
     *  if #grammars=1 then this is irrelevant,
     *  its simply adding or removing et's from the only
     *  grammar that there is.
     *
     *  really all that matters is getInsideProbs and
     *  resampleDerivation.
     *  
     */
  def resampleDoc(doc : List[CParseTree],absInd : Int) = {
    val oldTyp = docTyp(absInd)
    
    doc.foreach(tree => {
      removeSegmentsFromMap(tree,oldTyp)
      //pray to COD for all the ET's that could have been substituted here!
      var overlays : Array[List[(CSegment,List[Int])]] = (for{i <- 0 until tree.nodez.length} yield {
        model.cod.findOverlays(tree,i)
      }).toArray
      val iProbs = getInsideProbs(tree,oldTyp,overlays)
      resampleDerivation(tree,iProbs,oldTyp)  
      addSegmentsToMap(tree,oldTyp)
    })
    
  }

  /**
   * Dynamic program to get the IProb cells for each node (arrayed by DFS index)
  *
  *  the label, by which I mean type, doesnt matter for single grammar systems
  * 
  */ 
  def getInsideProbs(tree : CParseTree, label : Int, overlays : Array[List[(CSegment,List[Int])]]) : Array[IProb] = {
    
    //why not name this for fun?
    val nNodes = tree.nodez.length
    
    //setup the DynProg table to fill!
    var insideProbs : Array[IProb] = 
      Array.tabulate(nNodes)(x => {
        val x : IProb = null
        x
      })

    //get the distribution over grammars - (delta dist if #grammars = 1)
    val thetaN = 0.until(model.numTopics).map(x => model.topicProb(x,label))
    
    /**
     * Fills the index'th cell with its inside probs.
     *
     * just after this function definition is the loop that uses it.
     *
     */ 
    def doinside(index : Int) : Unit = {
      
      val n = tree.nodez(index) //name ze node

      //the probability of leaving this node primed and deriving the subtree below n
      //there's one value for each grammar
      val primeProbs : Array[Double] = if(n.isTerm) {
        //for a terminal theres just the rule, the pcfg prob is all there is to it
        (for(i <- 0 until model.numTopics) yield {
          lowmem.pcfgProbs(n.rule) 
        }).toArray
      } else {
        //now weve got to compute the prob of leaving this node primed

        //this requires making one pcfg move and then for each child
        //either continuting in prime or leaving prime and starting a 
        //new move.

        val pcfgProb = lowmem.pcfgProbs(n.rule) //start with the pcfg prob        
        
        //get the prob of arriving at each child in prime
        //this equals the probability of leaving it prime +
        // the prob of ending prime there and starting a new subst.
        
        val kProbs = tree.getChildren(index).map(c => {
          val cNode = tree.nodez(c) //child node
          val beta = model.base.betas(lowmem.lhsOfRule(cNode.rule)) //the beta for this child node
          val (iS,iP,opts) = insideProbs(c) //get the ynProg cell for the child

          /**
           * iS is an array across grammars of the inside prob of being in grammar i,
           * starting a substitution at this child node and deriving the subtree
           *
           * iP is an array across grammars of bring in grammar i, being primed, and
           * deriving the subtree
           *
           */ 

          //calculate prob of starting a new substitution, mixed across all the grammars,
          //and multiplied by the beta (prob of leaving prime at the child node)
          val prodL = (iS zip thetaN).map(x => x._1 * x._2) 
          val totalSProbWBeta = ((0.0 /: prodL)(_ + _)) * beta

          val onemBeta = 1-beta //naming things is fun!
          (for(i <- 0 until model.numTopics) yield {

            totalSProbWBeta + iP(i) * (onemBeta) //if we stay primed, then we must stay in the same topic

          }).toArray
        }).toArray

        //now multiply the kid probs altogether, once for each grammar
        (for(i <- 0 until model.numTopics) yield {
          (pcfgProb /: kProbs)((x,y) => x * y(i))
        }).toArray
      }

      /**
       *  
       *
       *
       */
      
      //for each segment, the prob of leaving a node with that ET and each topic
      val etInsides : List[(CSegment,List[Int],Array[Double])] = overlays(index).map(_ match {
        case (tree,leaves) => {
          //table scores for each state, and the new state
          val tableScores : Array[Double] = model.tableScores(tree) 
          val sProbs = (tableScores zipWithIndex).map(ts => {
            (ts._1 /: leaves)((a,l) => {

              val prodL = (insideProbs(l)._1 zip thetaN).map(x => x._1 * x._2)
              val tInside = (0.0 /: prodL)(_ + _)
              //sum together the inside probs at the leaf over all topics
              a * tInside 

            })
          })
          (tree,leaves,sProbs)
        }
      })

      //probability of starting a new primed ET here
      val primeAl : Array[Double] = (model.alphaScores(lowmem.lhsOfRule(n.rule)) zip primeProbs).map(a => {
        a._1 * a._2
      })

      //add it all up for each topic - starting a new ET or starting a primed ET
      val startProbs : Array[Double] = (primeAl zipWithIndex).map(_ match {
        case (prob,ind) => {
          (prob /: etInsides)(_ + _._3(ind))
        }
      })

/**
      println("IPROB RETURN AT " + index)
      println(startProbs.length + " -- " + startProbs.mkString(","))
      println(primeProbs.length + " -- " + primeProbs.mkString(","))
      throw new Exception()
*/

      insideProbs(index) = (startProbs,primeProbs,(null,Nil,primeAl) :: etInsides)
    }

    var ind = tree.nodez.length - 1
    while(ind >= 0) {
      doinside(ind)
      ind -= 1
    }

    insideProbs
  }

  def resampleDerivation(tree : CParseTree, insideProbs : Array[IProb], label : Int) = {

    //val origMarks = tree.nodez.map(_.mark) //DIAGNOSTIC 
    
    tree.clearMarks()

    def recSample(index : Int, primed : Boolean, gInd : Int) : Unit = {
      val cNode = tree.nodez(index)
      if(primed) { 
        if(!cNode.isTerm) { //if we're primed and at a terminal node we must stop, no changes to be made
          tree.getChildren(index).foreach(c => {
            val childNode = tree.nodez(c)
            val beta = model.base.betas(lowmem.lhsOfRule(childNode.rule))
            val iP : IProb = insideProbs(c)
            val startProb = (0.0 /: iP._1)(_ + _) //in non ET-TOPIC mode, this will only have a nonzero entry in one place
            //NOTE : don't need to put in the PCFG prob part
            val sP = beta * startProb //unprime (beta) and start something new
            val pP = (1.0-beta) * iP._2(gInd) //P(stay primed), and in the current grammar
            val sampleR = pP / (sP + pP)
            if(rando.nextDouble() > sampleR)
              recSample(c,false,-1)
            else
              recSample(c,true,gInd)
          })
        } 
      } else {
        cNode.mark = true

        //have to choose not only which move to use but which grammar to draw it from

        //so we store the grammar index (Int) along with 
        //the move instructions (SubMove) 
        //and the prob (Double)

        //unnormalized probability, grammar index, substitution move
        var sampOpts : Array[(Double,Int,SubMove)] = insideProbs(index)._3.flatMap(x => { //go over the sampling options
            (x._3 zipWithIndex).map(_ match { //multiply in the topic choice probability to get the joint P(z,s)
              case(grammarProb,grammarIndex) => {
                val prob = grammarProb * model.topicProb(grammarIndex,label)
                (prob,grammarIndex,x)
              }
            })
          }).toArray


        //println("SAMPLING OPTIONS")
        //sampOpts.foreach(x => println(x))
          
        var sampInd = sampleUnNormalized(sampOpts.map(_._1))
        if(sampInd == -1) {
          fails += 1
          sampInd = rando.nextInt(sampOpts.length)
        }

        cNode.aspect = sampOpts(sampInd)._2.toChar //set the grammar index 
        val (seg,lvs,probs) = sampOpts(sampInd)._3 //get the sampled move instructions out
        
        val samp = (seg,lvs) //the prob's in each grammar for this action dont matter any more
          samp match {
            case (null,Nil) => { //go into the prime grammar
              if(!cNode.isTerm)
                recSample(index,true,cNode.aspect)
            } 
            case (tree,leevs) => {
              leevs.foreach(l => {
                recSample(l,false,-1)
              })
            }
          }   
      }
    }
    
    recSample(0,false,-1)
/**
    //DIAGNOSTIC!!!
    val newMarks = tree.nodez.map(_.mark)  
    (origMarks zip newMarks).foreach(x => {
      if(x._1 != x._2)
        totalChanged += 1
    })
*/
  }

}

