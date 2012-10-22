package enbuske

import multitool._
import scala.collection.mutable.{HashMap,HashSet}

/**
object FoldedCounter {

  import java.io._

  def main(args : Array[String]) = {

    val fold = args(0).toInt
    val nFolds = args(1).toInt

    val st = new CFGSymbolTable()

    val file = "/home/chonger/data/TAG/trainTAG.txt"

    val (st,pcfg,lowmem,treez) = {
      val tz = pcfg.read(file).filter(_.terminals.length <= 40)
      println("Using " + tz.length + " trees for counting")
      val lowmem = new LowMem(pcfg)
      tz.map(x => lowmem.convert(new ParseTree(x.root) with Markers))
    }

    val getFeet : (CParseTree => Array[List[Int]]) = (tree => {
      var feet = tree.nodez.zipWithIndex.map(n => {
        val sym = lowmem.lhsOfRule(n._1.rule)
        val kids = tree.getChildren(n._2)
        kids.filter(k => {
          val kSym = lowmem.lhsOfRule(tree.nodez(k).rule)
          kSym == sym
        }).toList
      })
      0.until(tree.nodez.length).foreach(i => {
        val ind = tree.nodez.length - 1 - i
        val newFeet = (List[Int]() /: feet(ind))((a,j) => {
          feet(j) ::: a
        })
        feet(ind) :::= newFeet
      })
      feet
    })

    val nSyms = pcfg.nextSymID
    val counts = new HashMap[CSegment,Int]()
    var cod : TAGCOD = null

    val oFile = file + ".count-F" + fold + "of" + nFolds
    val bw = new BufferedWriter(new FileWriter(oFile))

    var nT = 0
    pcfg.lazyGrowth(file,(x : ParseTree) => {  
      nT += 1
    })

    var fInd = 0
    val foldS = (nT.toDouble / nFolds.toDouble).toInt + 1
    println("fold size = " + foldS)
    var fragz : Array[CSegment] = Array.tabulate(foldS)(x => null)

    var nF = 0
    
    def procFragz() = {
      println("processing fragments")
      
      cod = new TAGCOD(pcfg,lowmem,nSyms,getFeet)
      0.until(fInd).foreach(r => {
        cod.addTree(fragz(r))
      })      
      println("cod constructed")

      var c = 0
      treez.par.foreach(tree => {


        
//        println("SEEK")
        val oles = 0.until(tree.nodez.length).flatMap(i => {
          cod.findOverlays(tree,i).map(_._1)
        })
  //      println("OLES FOUND")

        synchronized {
          c += 1
          if(c % 1000 == 0)
            println(nF + " " + c)
          oles.foreach(et => {
            val count = counts.getOrElse(et,0) + 1
            counts += et -> count
          })
        }

      })

      println("writing segments")
      counts.iterator.foreach({
        case (seg,c) => {
          bw.write(c + "\t" + "\\s+".r.replaceAllIn(PCFGPrinter.treeToString(pcfg,lowmem.revert(seg))," ") + "\n")
        }
      })

      counts.clear()
      fInd = 0
      println("done")
    }

    var skip = 0
    var done = false
    
    pcfg.lazyGrowth(file,(x : ParseTree) => {  
      if(skip < fold * foldS)
        skip += 1 
      else {
        if(!done) {
          val r = lowmem.convert(new ParseTree(x.root) with Markers)
          fragz(fInd) = new CSegment(r,0,r.nodez.map(_.mark))
          fInd += 1
//      if(fInd % 1000 == 0)
  //      println(fInd)
          if(fInd >= foldS) {
            nF += 1
            procFragz()
            done = true
          }
      
        }
      }
    }) //prep the symbol indexing of the pcfg

    if(!done)
      procFragz()

    bw.close()

  }
}


object FragmentSeeker {

  def main(args : Array[String]) = {

    1.to(5).foreach(sInd => {
      val train = "/home/chonger/data/ICLE/jojo_train_swunk_"+sInd+".xml"
      val (pcfg,treez) = DoctorX.readDoc(train)
      val oPath = "/home/chonger/data/ICLE/FRAG/JJ-DoubleDOP"+sInd+".txt"
      /**
       val segLines = io.Source.fromFile(oPath).getLines.toArray
       val nlines = segLines.length
       */
      val segs = pcfg.read(oPath)
      val cod = new Compacter(pcfg.nextSymID)
      segs.foreach(x => cod.addTree(x))

      val max = 100

      val maxPlus = new HashSet[ParseTree]()
      val counts = new HashMap[ParseTree,Int]()
      var iii = 0
      treez.flatMap(_.sents()).par.foreach(tree => {
        
        var overlays : List[ParseTree] = tree.nonterminals.flatMap(n => {
          cod.findOverlays(n).map(_._1)
        }).toList

        var rem = new HashSet[ParseTree]()
        
        overlays.foreach(o => {
          if(rem contains o) {
            //doodoo
          } else {
            val e = counts.getOrElse(o,0) + 1
            if(e >= max) {
              rem += o
              cod.remTree(o)
              maxPlus += o
              counts -= o
            } else {
              counts += o -> e
            }
          }
        })
        iii += 1
        if(iii % 100 == 0)
          println(iii)
      })
      /**
       counts.iterator.toArray.sortWith(_._2 > _._2).slice(0,20).foreach(x => {
       println(PCFGPrinter.treeToString(pcfg,x._1))
       println(x._2)
       })
       */
      val counted = Array.tabulate(max-1)(x => List[ParseTree]())

      counts.iterator.filter(_._2 > 1).foreach(_ match {
        case (t,c) => {
          counted(c-2) ::= t
        }
      })
      counted(max-2) = maxPlus.iterator.toList

      var ind = max + 1
      var rTot = 0
      counted.reverse.foreach(x => {
        ind -= 1

        val fname = oPath + "." + ind
        val trz = counted.reverse.slice(0,max+1-ind).flatMap(x => x)
        pcfg.write(fname,trz.toList)

        rTot += x.length
        println(ind + " - " + x.length + "(" + rTot + ")")
        
      })
      
    })

  }

}

*/
/**
class FragmentSeeker(val st : CFGSymbolTable) {

  class SeekCell(val i : Int, val j : Int, val star : Boolean, val fill : Boolean) {
    var top = if(fill) true else false
    val indSet = new HashSet[Int]()
  }

  def seekNSave(dox : Array[XMLDoc] , filE : String) : Unit = {
    seekNSave(dox,filE,0,1)
  }

  def seekNSave(dox : Array[XMLDoc] , filE : String,fold : Int, nFolds : Int) : Unit = {
    var lowmem = new LowMem(st)
    val ctreez = dox.flatMap(_.text.map(x => lowmem.convert(new ParseTree(x.root) with Markers)))
    val grammarSet = seekC(ctreez,lowmem,fold,nFolds)    
    pcfg.write(filE,grammarSet.iterator.map(x => lowmem.revert(x)).toList)
  }


  def showSeek(treez : Array[ParseTree], filE : String) = {
    
    var lowmem = new LowMem(pcfg)
    val ctreez = treez.map(x => lowmem.convert(new ParseTree(x.root) with Markers))
    val segs = seekC(ctreez,lowmem,0,1)
    
    val cod = new LMCompacter(lowmem,pcfg.symbolStrings.length)
    segs.foreach(x => cod.addTree(x))
    
    val max = 20

    val maxPlus = new HashSet[CSegment]()
    val counts = new HashMap[CSegment,Int]()

    ctreez.foreach(tree => {
      var overlays : List[CSegment] = 0.until(tree.nodez.length).flatMap(n => {
        cod.findOverlays(tree,n).map(_._1)
      }).toList

      var rem = new HashSet[CSegment]()
      
      overlays.foreach(o => {
        if(rem contains o) {
          //doodoo
        } else {
          val e = counts.getOrElse(o,0) + 1
          if(e >= max) {
            rem += o
            cod.remTree(o)
            maxPlus += o
            counts -= o
          } else {
            counts += o -> e
          }
        }
      })
    })
    /**
     counts.iterator.toArray.sortWith(_._2 > _._2).slice(0,20).foreach(x => {
     println(PCFGPrinter.treeToString(pcfg,x._1))
     println(x._2)
     })
     */
    val counted = Array.tabulate(max-1)(x => List[CSegment]())

    counts.iterator.filter(_._2 > 1).foreach(_ match {
      case (t,c) => {
        counted(c-2) ::= t
      }
    })
    counted(max-2) = maxPlus.iterator.toList

    var ind = max + 1
    var rTot = 0
    counted.reverse.foreach(x => {
      ind -= 1

      val fname = filE + "." + ind
      val trz = counted.reverse.slice(0,max+1-ind).flatMap(x => x)
      pcfg.write(fname,trz.map(x => lowmem.revert(x)).toList)

      rTot += x.length
      println(ind + " - " + x.length + "(" + rTot + ")")
      
    })

  }

  def seekNSave(treez : Array[ParseTree], filE : String) = {
    import java.io.{FileWriter,BufferedWriter,File}
    val segs = seek(treez)
    pcfg.write(filE,segs.toList)
  }

  def seek(treez : Array[ParseTree]) : Array[ParseTree] = {
    seek(treez,false)
  }

  def seek(treez : Array[ParseTree],addPCFG : Boolean) : Array[ParseTree] = {
    var lowmem = new LowMem(pcfg)
    val ctreez = treez.map(x => lowmem.convert(new ParseTree(x.root) with Markers))

    val grammarSet = seekC(ctreez,lowmem,0,1)
    
    val realHS = new HashSet[ParseTree]() ++ grammarSet.map(x => lowmem.revert(x))
    if(addPCFG)
      realHS ++= enbuske.util.TreeTools.cfgSet(treez.toList).iterator
    realHS.iterator.toArray
  }

  def seekC(ctreez : Array[CParseTree], lowmem : LowMem, fold : Int, nFolds : Int) : Array[CSegment] = {
    //lowmem.trim()

    val totalComparisons = ctreez.length * (ctreez.length - 1) / 2
    val foldSize = ((ctreez.length.toDouble / nFolds.toDouble) * (ctreez.length - 1.0) / 2.0).toInt + 1 //round up to make sure we make all comparisons
    //val nSkip = foldSize * fold //use zero based indixes for folds

    println("Total comparisons = " + totalComparisons)
    println("Fold Size = " + foldSize)
    //println("Skipping " + nSkip)

    //compare all pairs of treez

    //the first tree's index is iii
    //the second tree's index is jjj

    val grammarSet = new HashSet[CSegment]()

    val nTreez = ctreez.length

    var time : Double = System.currentTimeMillis().toDouble
    var done = 0
    var foldBurn = 0
    var burn = 0

    0.until(nTreez).foreach(iii => {
      if(done < foldSize) {
        val nComps = nTreez - iii - 1
        if(foldBurn < fold)
          burn += nComps
        if(burn > foldSize) {
          burn -= foldSize
          foldBurn += 1
        }
        if(foldBurn < fold) {
          //print(".")
          //println(foldBurn + " " + burn)
          //skip
        } else {
          val myGSet = new HashSet[CSegment]()
          val tree1 = ctreez(iii)
          val dynamicA = Array.tabulate(tree1.nodez.length)(x => Array[SeekCell]())

          (iii+1).until(nTreez).foreach(jjj => {
            if(done < foldSize) {
              //main program meat starts here
              if(burn > 0) {
                burn -= 1
              } else {
                val tree2 = ctreez(jjj)

                def symsMatch(i : Int, j : Int) : Boolean = {
                  lowmem.lhsOfRule(tree1.nodez(i).rule) == lowmem.lhsOfRule(tree2.nodez(j).rule)
                }

                def kidsMatch(i : Int, j : Int) : Boolean = {
                  tree1.nodez(i).rule == tree2.nodez(j).rule
                }

                0.until(tree1.nodez.length).foreach(x => {
                  dynamicA(x) = Array.tabulate(tree2.nodez.length)(y => {
                    null
                  })
                })

                def getCell(x : Int, y : Int) : SeekCell = {
                  if(dynamicA(x)(y) != null)
                    dynamicA(x)(y)
                  else {
                    lazy val k1 = tree1.getChildren(x)
                    lazy val k2 = tree2.getChildren(y)
                    val myCell = new SeekCell(iii,jjj,symsMatch(x,y),kidsMatch(x,y)) 

                    //if no kids match then the set is empty

                    if(myCell.fill) {
                      myCell.indSet += x //only keep track of the indexes in tree1
                      k1 zip k2 foreach(x => {
                        val kCell = getCell(x._1,x._2)
                        kCell.top = false
                        myCell.indSet ++= kCell.indSet
                      })
                    }
                    dynamicA(x)(y) = myCell
                    myCell
                  }
                }
                

                0.until(tree1.nodez.length).foreach(x => {
                  0.until(tree2.nodez.length).foreach(y => {
                    getCell(x,y)
                  })
                })

                val tops = 0.until(tree1.nodez.length).flatMap(x => {
                  dynamicA(x).filter(_.top).map(a => (a,x))
                })
                
                tops.foreach(_ match {
                  case (topN,index) => {
                    var marks = Array.tabulate(tree1.nodez.length)(x => true)
                    topN.indSet.foreach(i => marks(i) = false)
                    val seg = new CSegment(tree1,index,marks)
                    myGSet += seg
                  }
                })

                done += 1
                if(done % 10000 == 0) {
                  /**
                   val curT = System.currentTimeMillis().toDouble
                   val secs = (curT - time) / 1000.0
                   val hours = secs.toInt / 3600
                   val leftS = secs - hours * 3600
                   val mins = leftS.toInt / 60
                   val leftS2 = leftS - mins * 60
                   val tStr = hours + "h " + mins + "m " + leftS2.toInt + "s"*/
                  println("Finished " + done + " and found " + grammarSet.size + " trees so far")
                }
              }
            }
          })
          grammarSet ++= myGSet
        }
      }
    })
    println("did " + done)
    grammarSet.iterator.toArray
  } 

}

*/
