package enbuske.tsg
/**
import enbuske.util._
import scala.collection.mutable.{HashMap,HashSet}
import enbuske.util.Util.BigDouble
import java.io.{BufferedWriter,FileWriter,File}
import cc.mallet.pipe._
import cc.mallet.pipe.iterator._
import cc.mallet.types._
import cc.mallet.classify.evaluate._

object Evaluator {

  def fromSampled(treeF : String, sampF : String, testF : String) : Evaluator = {
    fromSampled(treeF,sampF,testF,false)
  }

  def fromSampled(treeF : String, sampF : String, testF : String, degenerate : Boolean) : Evaluator = {

    val (pcfg0,train) = DoctorX.readDoc(treeF,true) //smoothed!
    val (pcfg,test) = DoctorX.readDoc(testF,pcfg0)
    //have to read the test file too, to get the full pcfg set
    pcfg.estimateFromData(train.flatMap(_.text.flatMap(a => a)).toList)

    var lines = Util.getLines(sampF)

    println("READING")

    val sampledNSyms = lines(0).toInt
    val nSyms = pcfg.nextSymID

    if(nSyms != sampledNSyms) {
      println("NOTE : Sampling had " + sampledNSyms + " symbols but we have " + nSyms + " with test files")
    }
    
    lines = lines.drop(1)
    val numTopics = lines(0).toInt
    lines = lines.drop(1)
    val tsgAlpha = lines.slice(0,numTopics).map(_.toDouble).toArray
    lines = lines.drop(numTopics)
    val tsgGamma = lines(0).toDouble
    lines = lines.drop(1)

    val docTypes = lines(0).split(",").map(_.toInt).toArray
    lines = lines.drop(1) //kill doc types

    val nSemi = lines(0).toInt
    lines = lines.drop(1) //kill doc types

 //burn semiInds
    val semiSet = new HashSet[Int]() ++ lines.slice(0,nSemi).map(_.toInt)
    lines = lines.drop(nSemi)

    val nUsed = lines(0).toInt
    lines = lines.drop(1) 
    println("Used " + nUsed + " docs")
    val useSet = new HashSet[Int]() ++ lines.slice(0,nUsed).map(_.toInt)
    lines = lines.drop(nUsed)

    val numTypes = lines(0).toInt
    lines = lines.drop(1)
    val typeset = lines.slice(0,numTypes).map(_.replaceAll("\\s","")).toArray
    lines = lines.drop(numTypes)
    println("TYPES - "  + typeset.mkString(","))

    val theta0 = 0.until(numTypes).map(x => {
      val r = 0.until(numTopics).map(y => {
        lines(x * numTopics + y).toDouble
      }).toArray
//      println(r.mkString(","))
      r
    }).toArray
    lines = lines.drop(numTopics * numTypes)

/**    
    val theta0 = 0.until(numTypes).map(x => {
      val r = 0.until(numTopics).map(y => {
        .1
      }).toArray
      r
    }).toArray
    lines = lines.drop(1)
*/
    val p0betas = (for(i <- 0 until nSyms) yield {
      if(i < sampledNSyms)
        lines(i).toDouble
      else
        .5
    }).toArray
    println("BETAS! " + p0betas.mkString("\n"))
    lines = lines.drop(sampledNSyms)

    val tDist = new CohnGoldwater(pcfg,p0betas)

    val typeXtopic = Array.tabulate(numTypes,numTopics)((a,b) => 0)
    val topicC = Array.tabulate(numTopics)(x => 0)

    val nTrain = train.flatMap(_.text.flatMap(x => x)).length
    println(nTrain + " training treez")

//    val semiSet = new HashSet[Int]() ++ 0.until(nTrain)
  //  val useSet = new HashSet[Int]() ++ 0.until(nTrain)
    //val docTypes = 0.until(nTrain).map(x => 0.toInt).toArray

    var tInd = 0
    val tsgCounts = Array.tabulate(nSyms)(x => new HashMap[ParseTree,Array[Int]]())
    def blankArr() = Array.tabulate(numTopics)(x => 0)
    var dInd = 0
    train.foreach(doc => {
      val typ = docTypes(dInd)
      
  //    println("\n\n\n\nNEW DOC -type- " + typ + "\n\n\n\n")
      if(useSet contains dInd) {

        doc.sents().foreach(tree => {
          val as = lines(tInd).split(",").toList
          val ms = lines(tInd+1).split("").toList.drop(1)
          tInd += 2
          //    println(tree.sentence(pcfg))
          //    println(as.toArray.mkString(" "))
          //    println(ms.toArray.mkString(" "))
          //    println((ms.length,as.length,tree.nonterminals.length))
          if(ms.length != tree.nonterminals.length)
            throw new Exception()
          if(as.length != tree.nonterminals.length) {
            throw new Exception()
          }
          
          (as zip tree.nonterminals).foreach(x => {
            tree.aspect += new RefWrapper(x._2) -> x._1.toInt
          })
          (ms zip tree.nonterminals).foreach(x => {
            if(x._1 == "1") {
              tree.mark(x._2)
            }
          })
          tree.nonterminals.filter(n => tree.isMarked(n)).foreach(n => {
            val seg = new ParseTree(tree.getSegmentFrom(n))
            val lhs = seg.root.symbol
            //println(tree.aspect.iterator.toArray.mkString(","))
            val grammarIndex = tree.aspect(new RefWrapper(n))
            topicC(grammarIndex) += 1
            typeXtopic(typ)(grammarIndex) += 1
            val ent = tsgCounts(lhs).getOrElse(seg,blankArr())
            ent(grammarIndex) += 1
            if(!degenerate)
              tsgCounts(lhs) += seg -> ent
          })
        })
        dInd += 1 
      } else {dInd += 1 ; tInd += (doc.sents().length * 2)}
    })
                      
    val topicTotal = (0 /: topicC)(_ + _)

    val pcfgSet = new HashSet[ParseTree]()
    (train.zipWithIndex.filter(semiSet contains _._2).map(_._1).toList ::: 
     test.toList).flatMap(_.text.flatMap(x => x)).foreach(tree => {
      val nnodes = tree.nonterminals.filter(!_.isInstanceOf[UnderspecifiedNode])
      nnodes.foreach(n => {
        n match {
          case pn : PreTerminalNode => {
            if(degenerate) {
              val term = pn.kid.terminal
              if(pcfg.terminalStrings(term) != "UNK")
                pcfgSet += new ParseTree(pn)
            } else {
              pcfgSet += new ParseTree(pn)
            }
          }
          case in : InternalNode => {
            val newkids = in.children.map(n2 => new UnderspecifiedNode(n2.symbol,null))
            pcfgSet += new ParseTree(new ProtoNode(in.symbol,newkids))
          }
        }
      })
    })    

    var added = 0
    pcfgSet.foreach(pt => {
      val m = tsgCounts(pt.root.symbol)
      if(!m.isDefinedAt(pt)) {
        added += 1
        m += (pt -> blankArr())
      }
    })
    println("ADDED " + added + " PCFG RULES")

    new Evaluator(nSyms,numTopics,pcfg,tsgAlpha,tsgGamma,tDist,theta0,typeXtopic,
                      topicC,topicTotal,tsgCounts,typeset,test) 

  }

}

class Evaluator(val nSyms : Int, val numTopics : Int, val pcfg : PCFG,
                tsgAlpha : Array[Double], tsgGamma : Double,
                base : TreeDistribution,
                theta0 : Array[Array[Double]],
                typeXtopicC : Array[Array[Int]],
                topicCounts : Array[Int],
                topicTotal : Int,
                treeCounts : Array[HashMap[ParseTree,Array[Int]]],
                val typeset : Array[String],
                dox : Array[EnbuDoc]) {
  
  val numTypes = typeXtopicC.length
  val typeXtopicProb = Array.tabulate(numTypes,numTopics)((a,b) => 0.0)

  val topicProb = Array.tabulate(numTopics)(x => 0.0)
  val etreeProb = Array.tabulate(nSyms)(x => new HashMap[ParseTree,Array[Double]]())
  val cod = new Compacter(nSyms)
  val rando = Util.malletRandom
  

  {
    0.until(numTypes).foreach(t => {
      val tot = (0.0 /: typeXtopicC(t))(_ + _) + (0.0 /: theta0(t))(_ + _)
      0.until(numTopics).foreach(s => {
        typeXtopicProb(t)(s) = (typeXtopicC(t)(s) + theta0(t)(s)) / tot
      })
    })
  }


  println(numTypes + " types")
  println(numTopics + " topics")

/**
  {
    0.until(numTypes).foreach(t => {
      val tot = (0.0 /: typeXtopicC(t).slice(0,15))(_ + _) + (0.0 /: theta0(t).slice(0,15))(_ + _)
      0.until(numTopics-1).foreach(s => {
        typeXtopicProb(t)(s) = (typeXtopicC(t)(s) + theta0(t)(s)) / tot
      }) 
       typeXtopicProb(t)(numTopics -1) = 0.0
    })
  }
*/

  {
    0.until(numTypes).foreach(t => {
      println(typeXtopicProb(t).mkString(" "))
    })
  }

  //println("TYPE X TOPIC")
  //typeXtopicProb.foreach(x => println(x.mkString("\t")))
  //throw new Exception()

 //  {
//     0.until(numTopics).foreach(i => {
//       topicProb(i) = (topicCounts(i) + theta0) / (topicTotal + numTopics * theta0)
//     })
//   }

 
  
  {
    //now fill the eTreeProb. we need the backoff weights for each tree
    for(i <- 0 until nSyms) {
      val segs = treeCounts(i).iterator.toList
      var backoff = segs.map(_ match {
        case (seg,counts) => {
          var tC = 0.0 
          var cI = 0
          counts.foreach(c => {
            for(i <- 0 until c) {
              if(rando.nextDouble() > (i / (i + tsgAlpha(cI))))
                tC += 1
            }
            cI += 1
          })
          val r = tC + tsgGamma * base.score(seg)
          if(r < 0) {
            println(tC)
            println(tsgGamma)
            println(base.score(seg))

            throw new Exception()
          }
          r
        }
      }).toArray
      //normalize base
      var bTot = (0.0 /: backoff)(_ + _)
      backoff = backoff.map(_ / bTot)

      val scores = (segs zip backoff.toList).map(_ match {
        case ((seg,counts),beta) => {
          var cI = 0
          counts.map(c => {
            val r = c + tsgAlpha(cI) * beta
            cI += 1
            r
          })
        }
      })

      var norm = Array.tabulate(numTopics)(x => 0.0)
      scores.foreach(sc => {
        for(i <- 0 until numTopics) {
          norm(i) += sc(i)
        }
      })

      (segs zip scores).foreach(_ match {
        case ((seg,counts),unScore) => {
          val nScores = 0.until(numTopics).map(x => {
            unScore(x) / norm(x)
          }).toArray
          nScores.foreach(x => {
            if(x < 0)
              throw new Exception()
            if(x.isNaN)
              throw new Exception()
            if(x == Double.PositiveInfinity)
              throw new Exception()
            if(x == Double.NegativeInfinity)
              throw new Exception()
          })
          etreeProb(i) += seg -> nScores
          cod.addTree(seg)
        }
      })

    }
  }
    //END CONSTRUCTOR


  def eval() = {
    
    val confuse = Array.tabulate(numTypes,numTypes)((x,y) => 0)
    
    var acc = 0.0
    var twoacc = 0.0
    var tot = 0.0
    var ind = 0
    var accumErr = 0.0

    dox.filter(typeset contains _.getMeta("goldLabel")).foreach(doc => {
      val treeScores = doc.text.flatMap(x => x).map(tree => {
        val r = scoreTree(tree)
        //println(r.mkString("\t"))
        //throw new Exception()
        r
      })
      val typeScores = 0.until(numTypes).map(t => {
        val topicW = typeXtopicProb(t)
        
        (0.0 /: treeScores)((a,ts) => {
          //total tree score is sum over topics
          //println("TREE SCORE - " + ts.mkString("\t"))
          def gS(i : Int) = {
            new BigDouble(1.0,math.log(topicW(i))) times ts(i)
          }

          val r = (gS(0) /: 1.until(numTopics))((a,b) => {
            a plus gS(b)
          })

          a + r.log()
        })
      })

      //println(typeScores.mkString("\t"))

      val trueS = typeset.indexOf(doc.getMeta("goldLabel"))
      if(trueS < 0)
        throw new Exception()

      val trueClassScore = typeScores(trueS)
      var moreThanTrueS = 0

      var m = typeScores(0)
      var mI = 0
      1.until(numTypes).foreach(t => {
        if(typeScores(t) > m) {
          mI = t
          m = typeScores(t)
        }
        if(typeScores(t) > trueClassScore)
          moreThanTrueS += 1
      })
      accumErr += moreThanTrueS
      if(moreThanTrueS <= 1)
        twoacc += 1
      
      confuse(trueS)(mI) += 1

      if(trueS == mI)
        acc += 1
      tot += 1

      //println(acc + "/" + tot)

      ind += 1
    })        

    println("Accuracy = " + (acc/tot))
    println("2Accuracy = " + (twoacc/tot))
    
    println("Avg Rank of Correct Label (0 based) = " + (accumErr / tot)) 
    
    println("Confusion Matrix")
    println("(X,Y) in applet coordinates is # of X classified as Y")
    println(("" :: typeset.toList).toArray.mkString("\t"))
    (confuse zip typeset).foreach(_ match {
      case (errCounts,label) => {
        print((label :: errCounts.toList).toArray.mkString("\t"))
        println("  -- T:" + (0 /: errCounts)(_ + _))
      }
    })

    (acc/tot)
  }
    
    def printMe(filE : String) = {
    import java.io.{BufferedWriter,FileWriter,File}
    var bw : BufferedWriter = null
    //show the tree distributions for each state

    for(i <- 0 until numTopics) {
      bw = new BufferedWriter(new FileWriter(new File(filE + "." + i)))
      for(j <- 0 until nSyms) {
        etreeProb(j).iterator.toList.sortWith((a,b) => {a._2(i) > b._2(i)}).slice(0,20).foreach(e => {
          bw.write(PCFGPrinter.treeToString(pcfg,e._1) + "\n")
          bw.write(e._2(i) + "\n")
        })
        bw.write("--------------------------------------------------\n")
      }
      bw.close()
    }
  }

  def printStrs(xmlF : String, filE : String) = {
    
    import java.io.{BufferedWriter,FileWriter,File}
    var bw : BufferedWriter = null
    //show the tree distributions for each state

    var bests = Array.tabulate(numTopics)(x => {
      val r : List[String] = Nil
      r
    })

    val (pcfgX,docs) = DoctorX.readDoc(xmlF,pcfg)

    docs.flatMap(_.text.flatMap(x => x)).foreach(tree => {
      val scores = scoreTree(tree)
      var mI = 0
      var m = scores(0)
      1.until(scores.length).foreach(x => {
        if(scores(x) gt m) {
          m = scores(x)
          mI = x
        }
      })
      bests(mI) ::= tree.sentence(pcfg)
    })

    for(i <- 0 until numTopics) {
      bw = new BufferedWriter(new FileWriter(new File(filE + "." + i)))
      bests(i).foreach(s => {
        bw.write(s + "\n")
      })
      bw.close()
    }
  }

  def scoreTree(tree : ParseTree) : Array[BigDouble] = {

    var overlays : Array[List[(ParseTree,List[NonTerminalNode])]] = tree.nonterminals.map(n => {
      cod.findOverlays(n)
    }).toArray

    var insideProbs = new HashMap[RefWrapper,Array[BigDouble]]()
    //println(PCFGPrinter.treeToString(pcfg,tree))
    def doinside(index : Int) : Unit = {
      val n = tree.nonterminals(index)

      val iProbs : Array[BigDouble] = Array.tabulate(numTopics)(x => new BigDouble(0.0,0.0))
      //println("TREE")
      //println("NODE - " + PCFGPrinter.nodeString(pcfg,tree.nonterminals(index))) 
      //println("NO - " + overlays(index).length)
      overlays(index).map(_ match {
        case (tree,leaves) => {
          val scores = etreeProb(n.symbol)(tree).map(x => new BigDouble(1.0,math.log(x)))
          //println("MY SCORE - " + scores.mkString(" "))
          //println("Num leaves = " + leaves.length)
          val lProbs = leaves.map(x => insideProbs(new RefWrapper(x)))
          0.until(numTopics).foreach(sI => {
            val sc = (scores(sI) /: lProbs)((a,l) => {
              val r = a times l(sI)
              //println("R = " + r)
              //println("A = " + a)
              //println("L = " + l(sI))
              r
            })
      //      println("SCC - " + scores(sI))
        //    println("SCORE - " + sc)
          //  throw new Exception()
            if(iProbs(sI).arg == 0.0) {
              iProbs(sI) = sc
            } else {
              val sum = iProbs(sI) plus sc
              iProbs(sI) = sum
            }
          })
        }
      })
      insideProbs += (new RefWrapper(n) -> iProbs)
    }

    var nodez = tree.nonterminals.zipWithIndex
    var ind = nodez.length - 1
    while(ind >= 0) {
      doinside(ind)
      ind -= 1
    }

    insideProbs(new RefWrapper(tree.root))
  }
/**
  def perplexity(documents : Array[Array[Array[ParseTree]]]) = {
    //var numSamples = (0.0 /: documents)(_ + _.length)
    var numSamples = (0.0 /: documents)((a,b) => a + (0.0 /: b)(_ + _.length))

    println("NUM SAMPLES = " + numSamples)

    var pplex = 0.0
    
    documents.foreach(sample => {
      sample.foreach(text => {
        text.foreach(sent => {
          pplex += -calcLL(Array(sent)) / numSamples
        })
      })
    })
    
    println(pplex)

    Math.pow(2.0,pplex)
  }
*/
/**
  def calcLL(text : Array[ParseTree]) = {

    val treeS = text.map(tree => {
      val scores = scoreTree(tree)
      //total tree score is sum over topics
      def gS(i : Int) = {
        new BigDouble(1.0,Math.log(topicProb(i))) times scores(i)
      }

      val r = (gS(0) /: 1.until(numTopics))((a,b) => {
        a plus gS(b)
      })
      r
    })
    
    (0.0 /: treeS)(_ + _.log() / Math.log(2))
  }
*/
  
}


object Analyzer {

  def main(args : Array[String]) = {
/**    
    val ev = Evaluator.fromSampled(args(0),args(1),args(2))
    ev.eval()
  }

  def pooo(args : Array[String]) = {
*/

/**
    val (pcfg,train) = DoctorX.readDoc(args(0))
    val t = train(0).text(0)(0)
    val nts = t.nonterminals
    val spns = t.getSpans()
    println(PCFGPrinter.treeToString(pcfg,t))
    nts zip spns foreach(x => {
      println("------")
      println(PCFGPrinter.nodeString(pcfg,x._1))
      println(x._2)
      println("------")
    })
  }
  def poo(args : Array[String]) = {
*/

    println("START")
    var lastTime = System.currentTimeMillis()
    val anal = Analyzer.fromSampled(args(0),args(1),args(2))
    println("LOADED " + ((System.currentTimeMillis() - lastTime).toDouble / 1000.0))
    lastTime = System.currentTimeMillis()

    val bw = new BufferedWriter(new FileWriter("/home/chonger/BucketParser/data/TreebankTSG.txt"))
    bw.write(anal.pcfg.nextSymID + "\n")
    anal.pcfg.symbolStrings.foreach(s => {
      bw.write(s +"\n0.0\n")
    })
    val treez = anal.treeMap.flatMap(_.iterator.toList)
    bw.write(treez.length + "\n")
    treez.foreach({
      case (t,ps) => {
        bw.write(PCFGPrinter.treeToString(anal.pcfg,t).replaceAll("\\s+"," ").trim + "\n")
        bw.write(ps(0) + "\n")
      }
    })
    bw.close()

/**
    anal.save(args(3))
    println("SAVED " + ((System.currentTimeMillis() - lastTime).toDouble / 1000.0))
    lastTime = System.currentTimeMillis()
    val anal2 = Analyzer.fromSaved(args(3))
    println("RELOADED " + ((System.currentTimeMillis() - lastTime).toDouble / 1000.0))
   */ 
  }
  def poo(args : Array[String]) = {

    var lastTime = System.currentTimeMillis()
    val anal = Analyzer.fromSaved(args(3))
    //val anal = Analyzer.fromSampled(args(0),args(1),args(2))
    
    println("Loaded in " + ((System.currentTimeMillis() - lastTime).toDouble / 1000.0) + " seconds")

    val (pcfg0,train) = DoctorX.readDoc(args(0),anal.pcfg)
    val (pcfg1,test) = DoctorX.readDoc(args(2),anal.pcfg)
    
    val tr_insts = anal.extractInstances(train)
    val ts_insts = anal.extractInstances(test)

    println("Training - " + tr_insts.size()  + " instances")
    println("Test     - " + ts_insts.size()  + " instances")

    import cc.mallet.classify._

    val regularizers : List[Double] = List(.1,1,5,10,20,50,100,1000)

    val accuracies = regularizers.map(x => {
      val C = x
      val trainer = new MaxEntTrainer(C)
      try {
        trainer.train(tr_insts)
      } catch {
        case e : Exception => {
          println(e)
        }
      }
    
      val classifier = trainer.getClassifier()
      val trial = new Trial(classifier, ts_insts)
    
      println("Accuracy: " + trial.getAccuracy())
      println(new ConfusionMatrix(trial))
      //classifier.print()
      (C,trial.getAccuracy())
    }).toArray

    println(accuracies.mkString("\n"))
    println(accuracies.map(_._2).mkString("\n"))
  }

  def fromSampled(treeF : String, sampF : String, testF : String) = {

    val evaluator = Evaluator.fromSampled(treeF,sampF,testF,false) //this last boolean is degenerate indicator

    new Analyzer(evaluator.pcfg,evaluator.typeXtopicProb,evaluator.etreeProb, evaluator.typeset)

  }

  def fromSaved(filE : String) = {
    var lines = Util.getLines(filE)
    
    val nTypes = lines(0).toInt
    lines = lines.drop(1)
    val tset = lines.slice(0,nTypes).map(_.replaceAll("\\s","")).toArray
    lines = lines.drop(nTypes)
    val nGrammars = lines(0).toInt
    lines = lines.drop(1)
    val tXt = 0.until(nTypes).map(x => {
      val ps = lines.slice(0,nGrammars)
      lines = lines.drop(nGrammars)
      ps.map(_.toDouble).toArray
    }).toArray

    val nSyms = lines(0).toInt
    lines = lines.drop(1)
    
    val treeMap = Array.tabulate(nSyms)(x => new HashMap[ParseTree,Array[Double]]())

    val (pcfg,train) = DoctorX.readDoc("/home/chonger/data/CONNLX/connl-prepped/6lang.xml",true) //smoothed!

    0.until(nSyms).foreach(x => {
      val nTreez = lines(0).toInt
      lines = lines.drop(1)
      0.until(nTreez).foreach(t => {
        val tree = pcfg.growTree(lines(0))
        lines = lines.drop(1)
        val scores = lines.slice(0,nGrammars).map(_.toDouble).toArray
        lines = lines.drop(nGrammars)
/**
        if(x == 5) {
          println("!!!@@@" + PCFGPrinter.treeToString(pcfg,tree))
        }
*/
        //if(x != tree.root.symbol)
        //  throw new Exception()
        treeMap(tree.root.symbol) += tree -> scores
      })
    })

    //this pcfg gets its symbol order from the saved trees
    
    new Analyzer(pcfg,tXt,treeMap,tset)
  }

}

class Analyzer(var pcfg : PCFG, val typeXgrammar : Array[Array[Double]], val treeMap : Array[HashMap[ParseTree,Array[Double]]], val typeset : Array[String]) {

  val nType = typeXgrammar.length
  val nGrammar = typeXgrammar(0).length
  val nSyms = treeMap.length
  val treeSizes = treeMap.map(x => x.size).toArray
  val cod = new Compacter(nSyms)
  treeMap.foreach(m => {
    m.iterator.foreach(x => {
      try {
        cod.addTree(x._1)
      } catch {
        case _ => {
          println("Number of symbols - " + nSyms)
          println(x._1.getTree().root.symbol)
          println(PCFGPrinter.treeToString(pcfg,x._1))
          throw new Exception()
        }
      }
    })
  })

  var etI = 0
  val etIndexes = new HashMap[ParseTree,Int]()
  treeMap.foreach(x => {
    x.iterator.foreach(et => {
      etIndexes += et._1 -> etI
      etI += 1
    })
  })
/**
  etIndexes.foreach(_ match {
    case (et,i) => {
      println("--" + i + "--")
      println(PCFGPrinter.treeToString(pcfg,et))
    }
  })
*/
  val nTrees = etI

  println("Grammer Analyzer Online")
  println("Experiment Information")
  println("  "  + nType + " types")
  println("  "  + nGrammar + " tree substitution grammars")
  println("  "  + nSyms + " distributions in each grammar")
  println("       with sizes as follows:")
  0.until(pcfg.nextSymID).foreach(s => {
    println(pcfg.symbolStrings(s) + " - " + treeMap(s).size)
  })
  println("Analyzation commencing immediately")

  println("Computing the ETs that each grammar owns maximum probability over")
  val mpg = maxProbGrammar()

  var filterF : ((ParseTree,Double,Double))=>Boolean = (x : (ParseTree,Double,Double)) => {
    x._3 > .001 && x._2 > 6
  }

  var sortF = (x : (ParseTree,Double,Double), y : (ParseTree,Double,Double)) => {
    x._2 > y._2
  }
  
  println("Computing the ETs that each type owns maximum probability over")
  val mpt = maxProbType()

  /**
  0.until(mpt.length).foreach(t => {
      println("TYPE - " + t)
      //println(t._1.mkString(","))
      0.until(mpt(0).length).foreach(x => {
        val goodones : List[(ParseTree,Double,Double)] = mpt(t)(x).toList
        val goodones1 = goodones.filter(filterF).toArray
        if(goodones1.length > 0)
          println("SYM " + x + " - " + goodones1.length)
      })
  })
  * 
*/


  def extractInstances(dox : Array[EnbuDoc]) = {
    import cc.mallet.pipe._
    import cc.mallet.pipe.iterator._
    import cc.mallet.types._
    //import java.util._

    val pipe : Pipe = {
      val pipeList = new java.util.ArrayList[Pipe]()
      //pipeList.add(new Csv2Array())
      pipeList.add(new Target2Label())
      pipeList.add(new Array2FeatureVector())
      pipeList.add(new PrintInputAndTarget())
      new SerialPipes(pipeList)
    }

    val instances = new InstanceList(pipe)
    
    dox.foreach(doc => {
      /**
       *  Features
       *
       *  LL - total, conditioned on native language, conditioned on grammar
       *  ET counts - (lets the maxent model decide for itself.)
       *  
       *
       *
       * */

      val treeScores = doc.text.flatMap(x => x).map(tree => {scoreTree(tree)})

      var overlays : List[ParseTree] = doc.text.flatMap(x => x).flatMap(tree => {
        val ret = tree.nonterminals.flatMap(n => {
          val oles = cod.findOverlays(n).map(_._1)
          oles
        })
/**
        println("Tree\n" + PCFGPrinter.treeToString(pcfg,tree))
        ret.foreach(et => {
          println(PCFGPrinter.treeToString(pcfg,et))
        })
        * */
        ret
      }).toList

      val countMap = new HashMap[ParseTree,Double]() 

      overlays.foreach(et => {
        //println("found ET")
  //      println(PCFGPrinter.treeToString(pcfg,et))
        val count = countMap.getOrElse(et,0.0)
        countMap += et -> (count + 1)
      })
/**
      println("PCFG COUNTS")
      countMap.foreach(_ match {
        case (et,c) => {
          println(PCFGPrinter.treeToString(pcfg,et))
          println(c)
        }
      })
*/
      val etFeats = Array.tabulate(nTrees)(x => 0.0)
      countMap.iterator.foreach(x => {
        val ind = etIndexes(x._1)
        etFeats(ind) = x._2.toInt
      })
/**
      println("ETFEATS = " + etFeats.mkString(","))
  etIndexes.foreach(_ match {
    case (et,i) => {
      println("--" + i + "--")
      println(PCFGPrinter.treeToString(pcfg,et))
    }
  })
  * */
      //add across rows to get the document score in each grammar

      val emptyA = Array.tabulate(nGrammar)(x =>0.0)

      val grammarScores = (emptyA /: treeScores)((a,ts) => {
        0.until(nGrammar).map(x => {
          a(x) + ts(x).log()
        }).toArray
      }).toList

      val typeScores = 0.until(nType).map(t => {
        val topicW = typeXgrammar(t)
        
        (0.0 /: treeScores)((a,ts) => {
          //total tree score is sum over topics
          //println("TREE SCORE - " + ts.mkString("\t"))
          def gS(i : Int) = {
            new BigDouble(1.0,math.log(topicW(i))) times ts(i)
          }

          val r = (gS(0) /: 1.until(nGrammar))((a,b) => {
            a plus gS(b)
          })

          a + r.log()
        })
      }).toList

      //val nTS = (0.0 /: typeScores)(_ + _)
      

      //println("FEATS : " + (grammarScores ++ typeScores).toArray.mkString(","))

      //var feats = java.util.Arrays.asList((grammarScores ++ typeScores).toArray: _*).toArray      

      //val feats : Array[Array[Array[Double]]] = ((("1,0,1,1,1,1","1,1,1,1,1,1","1,1,1,1,1,1","1,1,1,1,1,1"), ("data_bmp")))

      //val feats = (grammarScores ++ typeScores ++ etFeats).toArray
      //val feats = (grammarScores ++ typeScores ++ etFeats).toArray
      //val feats = (grammarScores ++ typeScores).toArray
      //val feats = (typeScores).toArray
      //val feats = (typeScores.map(_ * -1)).toArray
      val feats = (etFeats.map(x => if(x > 0) 1.0 else 0.0)).toArray
      //val feats = (etFeats).toArray
//      val foundrules = 
  //    println(foundrules.toArray.mkString(","))
      //println(feats.toList.min)
      //throw new Exception()
      instances.addThruPipe(new Instance(feats,doc.getMeta("goldLabel"),"NAME!","SOURCE"))




    })
    instances
  }

  def printMaxProbGrammar(gInd : Int,s : ParseTypes.Symbol, n : Int) = {
    mpg(gInd)(s).filter(filterF).toList.sortWith(sortF).slice(0,n).foreach(x => {
      println("-!-!-!-!-!-!-!-!-!-!-")
      println(PCFGPrinter.treeToString(pcfg,x._1))
      println(x._2 + " ---- " + x._3)
    })
  }

  def printMaxProbType(tInd : Int,s : ParseTypes.Symbol, n : Int) = {
    mpt(tInd)(s).filter(filterF).toList.sortWith(sortF).slice(0,n).foreach(x => {
      println("-!-!-!-!-!-!-!-!-!-!-")
      println(PCFGPrinter.treeToString(pcfg,x._1))
      println(x._2 + " ---- " + x._3)
    })
  }

  def listMPT(tInd : Int) = {
    0.until(pcfg.nextSymID).foreach(s => {
      mpt(tInd)(s).foreach(x => {
        println(PCFGPrinter.treeToString(pcfg,x._1))
      })
      //println(pcfg.symbolStrings(s) + " - " + mpt(tInd)(s).size)
    })
  }

  var testData : Array[EnbuDoc] = null
  var testDataU : Array[EnbuDoc] = null

  def loadTestData() : Unit = {
    loadTestData("/home/chonger/data/ICLE/jojo_cjtest.xml","/home/chonger/data/ICLE/jojo_cjtest.xml")
  }

  def loadTestData(SWfilE : String, UfilE : String) : Unit = {
    val (pcfg2,testS) = DoctorX.readDoc(SWfilE,pcfg)
/**
    pcfg.symbolStrings zip pcfg2.symbolStrings foreach (x => {
      println(x._1 + " " + x._2)
    })
*/
    pcfg = pcfg2
    testData = testS
    val (pcfg3,testU) = DoctorX.readDoc(UfilE,pcfg)
    pcfg = pcfg3
    testDataU = testU
  }

  def testInfo() = {
    if(testData == null)
      println("No test data loaded")
    else {
      println("Test data consists of " + testData.length + " documents")
    }
  }

  def showErrors(ind : Int) = {


    0.until(nType).foreach(x => {
      println("\n\n!!!!!!!\n\nTYPE " + x + "\n\n")
      val codd = new Compacter(nSyms)
      mpt(x).foreach(t => {
        t.foreach(y => {
            codd.addTree(y._1)
        })
      })
      0.until(testData(ind).sents().length).foreach(ii => {
        val tree = testData(ind).sents()(ii)
        val utree = testDataU(ind).sents()(ii)
        var overlays : Array[List[(ParseTree,List[NonTerminalNode])]] = tree.nonterminals.map(n => {
          codd.findOverlays(n)
        }).toArray
        val count = (0 /: overlays)(_ + _.length)
        if(count > 0) {
          (utree.nonterminals zip overlays).foreach(_ match {
            case (nt,oles) => {
              if(oles.length > 0) {
                println("EVIDENCE")
                val ntree = new ParseTree(nt)
                oles.foreach(ole => {
                  println(PCFGPrinter.treeToString(pcfg,ole._1))
                })
                println("SPAN")
                println(new ParseTree(nt).sentence(pcfg))
              }
            }
          })
          println("FULL SENTENCE")
          println(utree.sentence(pcfg))
          println()
        }
      })
    })
  }
  
  def save(filE : String) = {
    val bw = new BufferedWriter(new FileWriter(new File(filE)))
    
    bw.write(typeXgrammar.length.toString + "\n")
    typeset.foreach(x => bw.write(x + "\n"))
    bw.write(typeXgrammar(0).length.toString + "\n")
    typeXgrammar.foreach(ty => {
      ty.foreach(x => {
        bw.write(x + "\n")
      })
    })

    bw.write(treeMap.length + "\n")
    
    treeMap.foreach(mmm => {
      bw.write(mmm.size + "\n")
      mmm.iterator.foreach(_ match {
        case (tree,scrs) => {
          bw.write(PCFGPrinter.treeToString(pcfg,tree).replaceAll("\\s+"," ") + "\n")
          scrs.foreach(s => bw.write(s + "\n"))
        }
      })
    })
    
    bw.close()
  }

  /**
   *  GOAL -
   *
  *   a picture of which rules come from each grammar 
  *   a picture of which rules come from each native language
  *
  *   these are easy, but have 2 options
  *     1 - assign each ptree to the grammar that has maximum prob (1 to 1), sort by P/Avg(P)
  * 
  *     2 - assign each ptree to any grammar where P/Avg(P) is over a certain threshold.
  *
  *  
  *    maybe exclude rules that have a particularly small a priori prob
  *   
  * ----------------------
  *
  *   GOAL 2 - 
  *   
  *   given a sentence, find the sections that have high prob mass under each grammar...
  *
  *   how?
  *
  *   get inside probs like in sampling - the first member of IProb gives us P(start a substitution here with each topic) 
  *
  *
  */

  //return max members for grammarXsymXtree,give tree,ratio,prob|LHS
  def maxProbGrammar() : Array[Array[List[(ParseTree,Double,Double)]]] = {
    
    val res = Array.tabulate(nGrammar,nSyms)((x,y) => List[(ParseTree,Double,Double)]())
    
    (treeMap zipWithIndex).foreach(_ match {
      case (mmm,sym) => {
        mmm.iterator.foreach(_ match {
          case (tree,scores) => {
            val tot = (0.0 /: scores)(_ + _)
            val ratios = scores.map(_ / tot).toArray
            var mInd = 0
            var mScore = ratios(0)
            (ratios zipWithIndex).drop(1).foreach(_ match {
              case (s,i) => {
                if(s > mScore) {
                  mScore = s
                  mInd = i
                }
              }
            })
            res(mInd)(sym) ::= (tree,mScore,scores(mInd))
          }
        })
      }
    })

    res
  }

  //return max members for typeXsymXtree,give tree,ratio,prob|LHS
  def maxProbType() : Array[Array[List[(ParseTree,Double,Double)]]] = {

    val res = Array.tabulate(nType,nSyms)((x,y) => List[(ParseTree,Double,Double)]())
    
    (treeMap zipWithIndex).foreach(_ match {
      case (mmm,sym) => {
        mmm.iterator.foreach(_ match {
          case (tree,gscores) => { 
            val scores = 0.until(nType).map(ty => {
              val sss = gscores zip typeXgrammar(ty) map (x => x._1 * x._2)
              (0.0 /: sss)(_ + _)
            })
            //println(scores)
            val tot = (0.0 /: scores)(_ + _) / nType.toDouble
            val ratios = scores.map(_ / tot).toArray
            var mInd = 0
            var mScore = ratios(0)
            (ratios zipWithIndex).drop(1).foreach(_ match {
              case (s,i) => {
                if(s > mScore) {
                  mScore = s
                  mInd = i
                }
              }
            })
            res(mInd)(sym) ::= (tree,mScore,scores(mInd))
          }
        })
      }
    })
    //res
    res.map(_.map(_.filter(filterF)))
  }


  def scoreTree(tree : ParseTree) : Array[BigDouble] = {

    var overlays : Array[List[(ParseTree,List[NonTerminalNode])]] = tree.nonterminals.map(n => {
      cod.findOverlays(n)
    }).toArray

    var insideProbs = new HashMap[RefWrapper,Array[BigDouble]]()
    //println(PCFGPrinter.treeToString(pcfg,tree))
    def doinside(index : Int) : Unit = {
      val n = tree.nonterminals(index)

      val iProbs : Array[BigDouble] = Array.tabulate(nGrammar)(x => new BigDouble(0.0,0.0))
      //println("TREE")
      //println("NODE - " + PCFGPrinter.nodeString(pcfg,tree.nonterminals(index))) 
      //println("NO - " + overlays(index).length)
      //println("CYM - " + pcfg.symbolStrings(n.symbol))
      //println(n.symbol)
      //overlays(index).foreach(x => println("!!! - " + PCFGPrinter.treeToString(pcfg,x._1)))
      overlays(index).map(_ match {
        case (seg,leaves) => {
          //println("LOOKUP - " + PCFGPrinter.treeToString(pcfg,seg)) 
          //treeMap(n.symbol).iterator.foreach(x => println("MAPPED - " + PCFGPrinter.treeToString(pcfg,x._1)))
          
          val scores = try {
            treeMap(n.symbol)(seg).map(x => new BigDouble(1.0,math.log(x)))
          } catch {
            case _ => {
              println("Looking for")
              println(PCFGPrinter.treeToString(pcfg,seg))
              println("But we only have")
              treeMap(n.symbol).keySet.foreach(x => {
                println(PCFGPrinter.treeToString(pcfg,x))
              })
              throw new Exception()
            }
          }

          //println("MY SCORE - " + scores.mkString(" "))
          //println("Num leaves = " + leaves.length)
          val lProbs = leaves.map(x => insideProbs(new RefWrapper(x)))
          0.until(nGrammar).foreach(sI => {
            val sc = (scores(sI) /: lProbs)((a,l) => {
              val r = a times l(sI)
              //println("R = " + r)
              //println("A = " + a)
              //println("L = " + l(sI))
              r
            })
      //      println("SCC - " + scores(sI))
        //    println("SCORE - " + sc)
          //  throw new Exception()
            if(iProbs(sI).arg == 0.0) {
              iProbs(sI) = sc
            } else {
              val sum = iProbs(sI) plus sc
              iProbs(sI) = sum
            }
          })
        }
      })
      insideProbs += (new RefWrapper(n) -> iProbs)
    }

    var nodez = tree.nonterminals.zipWithIndex
    var ind = nodez.length - 1
    while(ind >= 0) {
      doinside(ind)
      ind -= 1
    }

    insideProbs(new RefWrapper(tree.root))
  }

  def getTypeS(tree : ParseTree) = {
    val ts = scoreTree(tree)

    0.until(nType).map(t => {
      val topicW = typeXgrammar(t)  

      //total tree score is sum over topics
      //println("TREE SCORE - " + ts.mkString("\t"))
      def gS(i : Int) = {
        new BigDouble(1.0,math.log(topicW(i))) times ts(i)
      }

      val r = (gS(0) /: 1.until(nGrammar))((a,b) => {
        a plus gS(b)
      })

      r
    })
  }

  def getTypeScores(trees : Array[ParseTree]) = {
  //  println("NTREEZ = " + trees.length)
    val treeScores = trees.map(tree => {
        val r = scoreTree(tree)
//        println(r.mkString("\t"))
        //throw new Exception()
        r
    })
    0.until(nType).map(t => {
      val topicW = typeXgrammar(t)
      
      (0.0 /: treeScores)((a,ts) => {
        //total tree score is sum over topics
        //println("TREE SCORE - " + ts.mkString("\t"))
        def gS(i : Int) = {
          new BigDouble(1.0,math.log(topicW(i))) times ts(i)
        }

        val r = (gS(0) /: 1.until(nGrammar))((a,b) => {
          a plus gS(b)
        })

        if(r.get() != 0)
          a + r.log()
        else
          a
      })
    })
  }

  def eval() = {
    
    val confuse = Array.tabulate(nType,nType)((x,y) => 0)
    
    var acc = 0.0
    var twoacc = 0.0
    var tot = 0.0
    var ind = 0
    var accumErr = 0.0


    val testF = ""
    val (pcfg0,testData) = DoctorX.readDoc(testF,pcfg)

    testData.foreach(doc => {
      val treeScores = doc.text.flatMap(x => x).map(tree => {
        val r = scoreTree(tree)
        //println(r.mkString("\t"))
        //throw new Exception()
        r
      })
      val typeScores = 0.until(nType).map(t => {
        val topicW = typeXgrammar(t)
        
        (0.0 /: treeScores)((a,ts) => {
          //total tree score is sum over topics
          //println("TREE SCORE - " + ts.mkString("\t"))
          def gS(i : Int) = {
            new BigDouble(1.0,math.log(topicW(i))) times ts(i)
          }

          val r = (gS(0) /: 1.until(nGrammar))((a,b) => {
            a plus gS(b)
          })

          a + r.log()
        })
      })

      //println(typeScores.mkString("\t"))

      val trueS = typeset.indexOf(doc.getMeta("goldLabel"))
      if(trueS < 0) {
        println("Gold label not found")
        println("TYPESET - " + typeset.toArray.mkString(","))
        println("this label is " + doc.getMeta("goldLabel"))

        throw new Exception()
      }

      val trueClassScore = typeScores(trueS)
      var moreThanTrueS = 0

      var m = typeScores(0)
      var mI = 0
      1.until(nType).foreach(t => {
        if(typeScores(t) > m) {
          mI = t
          m = typeScores(t)
        }
        if(typeScores(t) > trueClassScore)
          moreThanTrueS += 1
      })
      accumErr += moreThanTrueS
      if(moreThanTrueS <= 1)
        twoacc += 1
      
      confuse(trueS)(mI) += 1

      if(trueS == mI)
        acc += 1
      tot += 1

      //println(acc + "/" + tot)

      ind += 1
    })        

    println("Accuracy = " + (acc/tot))
    println("2Accuracy = " + (twoacc/tot))
    
    println("Avg Rank of Correct Label (0 based) = " + (accumErr / tot)) 
    
    println("Confusion Matrix")
    println("(X,Y) in applet coordinates is # of X classified as Y")
    println(("" :: typeset.toList).toArray.mkString("\t"))
    (confuse zip typeset).foreach(_ match {
      case (errCounts,label) => {
        print((label :: errCounts.toList).toArray.mkString("\t"))
        println("  -- T:" + (0 /: errCounts)(_ + _))
      }
    })
  }


}
*/
