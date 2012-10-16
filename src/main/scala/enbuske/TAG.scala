package enbuske

/**

class TAGCOD(val pcfg : PCFG, val lowmem : LowMem, inTrees : Array[List[CSegment]], val footF : (CParseTree => Array[List[Int]])) {

  def this(pcfg: PCFG, lowmem: LowMem, nSyms : Int,footF : (CParseTree => Array[List[Int]])) = {
    this(pcfg,lowmem, (for(i <- 0 until nSyms) yield {List[CSegment]()}).toArray,footF)
  }

  val store : Array[PackTree] = inTrees.map(ar => { //for each nonterminal's e-trees
    val t = new PackTree()
    ar.map(tree => t.add(lowmem,tree,tree.root))
    t
  })

  def findOverlays(tree : CParseTree, root : Int) : List[(CSegment,List[Int],List[(Int,Int)])] = {  
    val sym = lowmem.lhsOfRule(tree.nodez(root).rule)
    findTAG(tree,root,store(sym)).iterator.flatMap(_ match {
      case (rtree,insts) => {
        insts.map({
          case (warps,leaves) => (rtree.tree,leaves,warps)
        })
      }
    }).toList
  }

  def addTree(t : CSegment) = {
    val sym = lowmem.rootLHS(t)
    store(sym).add(lowmem,t,t.root)
  }

  def remTree(t : CSegment) = {
    val sym = lowmem.rootLHS(t)
    store(sym).remove(lowmem,t,t.root)
  }                     
  
  def findTAG(ctree : CParseTree, index : Int, packed : PackTree) = {
//    println("lookup! " + index)
    //packed.recPrint(lowmem,pcfg,"")
    val warps = footF(ctree)
//    println(PCFGPrinter.treeToString(pcfg,lowmem.revert(ctree)))
    
    def recFindTAG(ctree : CParseTree, index : Int, packed : PackTree) : HashMap[CRefTree,List[(List[(Int,Int)],List[Int])]] = {

  //    println("rec " + index)

      var ret = new HashMap[CRefTree,List[(List[(Int,Int)],List[Int])]]()
      packed.endsHere.foreach(e => {
        ret += e -> List((List[(Int,Int)](),List(index)))
      })

      val allopts = index :: warps(index)
    //  println(index + " -- ALL OPTIONS - " + allopts.mkString(" "))

      allopts.foreach(ind => {
      //  println("OPT IND " + ind)
        val targNode = ctree.nodez(ind)

        if(targNode.isTerm) { 
        //  println("is term! - " + packed.terminalKids.keySet.mkString(" ") + " ----- " + targNode.rule)
          val entry : PackTree = packed.terminalKids.getOrElse(targNode.rule,null)
          if(entry != null) {
            entry.endsHere.foreach(e => {
              if(ind == index) {
    //            println("!!??! " + ind + " " + index)
                ret += e -> List((List[(Int,Int)](),Nil))
              } else {
      //          println("!!!!!")
                ret += e -> List((List[(Int,Int)]((index,ind)),Nil))
              }
            })
          }
        } else {
          //println("is not term! - " + packed.terminalKids.keySet.mkString(" "))
          //println("Chech agaist packed tree")
          //packed.recPrint(lowmem,pcfg,"")
          val kids = ctree.getChildren(ind)
          val myArity = kids.length

          if(myArity <= packed.children.length) { //dont even bother if the arity is too big
            var cindex = 0
            
            var bail = false

            var compatMaps = kids.map(c => {
              val bin = packed.children(cindex)
              cindex += 1
              val childNode = ctree.nodez(c)
              val lhs = lowmem.lhsOfRule(childNode.rule)

              val entry : PackTree = bin.getOrElse((lhs,myArity),null)
              if(entry != null) {
                recFindTAG(ctree,c,entry)
              } else {
                bail = true
                null //this expansion cant be used, because there are no fragments with a child here
              }
            })


            
            if(!bail) {


//              println(" MERGE " + compatMaps.map(_.size).mkString(" _ "))
              //fragments that use this expansion must be in the maps for all of the children
              
              //we must record all combinatorial ways that a fragment can be used (with all the different warps)

              val goodFragz = (compatMaps(0).keySet /: compatMaps.drop(1))(_ intersect _.keySet)
              
              compatMaps = compatMaps.map(_.filter(goodFragz contains _._1))

              goodFragz.foreach(f => {
                val ents = compatMaps.map(m => m(f))

                val comb = (ents(0) /: ents.drop(1))((a,b) => {
                  a.flatMap({
                    case (wA,lA) => {
                      b.map({
                        case (wB,lB) => {
                          (wA ::: wB,lA ::: lB)
                        }
                      })
                    }
                  })
                })

                comb.foreach({
                  case (w,l) => {
                    val e = (w,l) :: ret.getOrElse(f,Nil)
                    ret += f -> e
                  }
                })
              })
            }
          }
        }
        //println("DONE LOOKING AT " + ind)
      })              

//      println(index + " -- finished")

      ret
    } //end of recursive bit



    val ret = recFindTAG(ctree, index, packed)
  //  println("really done")
    ret
  }
}



object TagFrag {

  def main(args : Array[String]) = {

    println("TAGSEEKDEMO")

    //val (pcfg,dox) = enbuske.util.DoctorX.readDoc("/home/chonger/data/ICLE/jojo_train_swunk_1.xml")
    val pcfg = new PCFG()
    //val treez = pcfg.read("/home/chonger/data/tagtoy2.txt")
    val treez = pcfg.read("/home/chonger/data/10train.txt.tri.unk")
    val seeker = new TagFrag(pcfg)

    //seeker.seekNSave(dox,"/home/chonger/data/ICLE/FRAG/fragtest.txt")
    val (grammar,tags) = seeker.seek(treez.toArray,false)
/**
    println("got " + grammar.length + " substitution rulez")
    grammar.foreach(t => {
      println(PCFGPrinter.treeToString(pcfg,t))
    })
    println("got " + tags.length + " auxiliary rulez")
    tags.foreach(t => {
      println(PCFGPrinter.treeToString(pcfg,t))
    })
   */ 
    val outbase = "/home/chonger/data/TAGgram3"
    
    pcfg.write(outbase + "TSG.txt",grammar.toList)
    pcfg.write(outbase + "TAG.txt",tags.toList)
 
  }

}

import scala.collection.mutable.{HashSet,HashMap}


object FoldedTagFrag {

  def main(args : Array[String]) = {

    val fold = args(0).toInt
    val foldTotal = args(1).toInt

    val pcfg = new PCFG()
    val treez = pcfg.read("/home/chonger/data/10train.txt.tri.unk")
    val oPath = "-F" + fold + "of" + foldTotal + ".txt"
    val base = "/home/chonger/data/"
    val seeker = new TagFrag(pcfg)
    val (grammar,tags) = seeker.seek(treez.toArray,false,fold,foldTotal)

    pcfg.write(base + "TSG" + oPath,grammar.toList)
    pcfg.write(base + "TAG" + oPath,tags.toList)
  }
}

class TagFrag(val pcfg : PCFG) {

  class SegSet(var indSet : List[Int],var warps : List[(Int,Int)], var foot : Int, var top : Boolean) {
    def this() = this(Nil,Nil,-1,true)
    override def toString() : String = {
      "IS: " + indSet.toArray.mkString(",") + " W: " + warps.toArray.mkString(",") + " T:" + top
    }
  }

  class SeekCell(val i : Int, val j : Int, val star : Boolean, val fill : Boolean) {
    var maxSegs : List[SegSet] = Nil
  }


  def seek(treez : Array[ParseTree],addPCFG : Boolean) : (Array[ParseTree],Array[ParseTree]) = {
    seek(treez,addPCFG,0,1)
  }

  def seek(treez : Array[ParseTree],addPCFG : Boolean,fold : Int,nFolds : Int) : (Array[ParseTree],Array[ParseTree]) = {
    var lowmem = new LowMem(pcfg)
    val ctreez = treez.map(x => lowmem.convert(new ParseTree(x.root) with Markers))

    val (grammarSet,tagset) = seekC(ctreez,lowmem,fold,nFolds)
    
    val realHS = new HashSet[ParseTree]() ++ grammarSet.map(x => lowmem.revert(x))
    if(addPCFG)
      realHS ++= enbuske.util.TreeTools.cfgSet(treez.toList).iterator
    
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
/**
    val cod = new TAGCOD(pcfg,lowmem,pcfg.nextSymID,getFeet)
    
    grammarSet.foreach(r => {
      val otr = new ParseTree(lowmem.revert(r).root) with Markers
      //println("REVERTED\n" + PCFGPrinter.treeToString(pcfg,otr))
      val tr = lowmem.convert(otr)
      //println("converted - ")
      //tr.print()
      //println(PCFGPrinter.treeToString(pcfg,lowmem.revert(tr)))
      cod.addTree(new CSegment(tr,0,tr.nodez.map(_.mark)))
    })
    //tagset.foreach(r => cod.addTree(r))

    ctreez.foreach(ct => {
      
      println("\nOVERLAYS for")
      println(PCFGPrinter.treeToString(pcfg,lowmem.revert(ct)))
      println("----o---oo--o-oo---o-oo-----")
      //println(0.until(ct.nodez.length).flatMap(n => {
      println(0.until(1).flatMap(n => {
        val oles = cod.findOverlays(ct,n)
        if(oles.length > 0) {
          oles.map({
            case (seg,lvs,wrps) => {
              PCFGPrinter.treeToString(pcfg,lowmem.revert(seg)) + "\n" + lvs + "\n" + wrps
            }
          })
        } else 
          Nil
      }).mkString("\n-----------------------------\n")) 
      println("\n")
    })
*/
    (realHS.iterator.toArray,tagset.iterator.map(x => lowmem.revert(x)).toArray)
  }

  def seekC(ctreez : Array[CParseTree], lowmem : LowMem,fold : Int, nFolds : Int) : (Array[CSegment],Array[CSegment]) = {

    def getFeet(tree : CParseTree) : Array[List[Int]] = {
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
    }

    val totalComparisons = ctreez.length * (ctreez.length - 1) / 2
    val foldSize = ((ctreez.length.toDouble / nFolds.toDouble) * (ctreez.length - 1.0) / 2.0).toInt + 1 //round up to make sure we make all comparisons

    println("Total comparisons = " + totalComparisons)
    println("Fold Size = " + foldSize)

    //compare all pairs of treez

    //the first tree's index is iii
    //the second tree's index is jjj

    val grammarSet = new HashSet[CSegment]()
    val tagSet = new HashSet[CSegment]()

    val realTAGS = new HashSet[ParseTree]()

    val nTreez = ctreez.length

    var time : Double = -1
    var avg : Double = 0
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
          //skip
        } else {      
          
          val curT = System.currentTimeMillis().toDouble
          time = curT

          val tree1 = ctreez(iii)
          val feet1 = getFeet(tree1)
          val dynamicA = Array.tabulate(tree1.nodez.length)(x => Array[SeekCell]())

          
          (iii+1).until(nTreez).foreach(jjj => {
            if(done < foldSize) {
              //main program meat starts here
              if(burn > 0) {
                burn -= 1
              } else {
                val tree2 = ctreez(jjj)
                val feet2 = getFeet(tree2)

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

                    val sym1 = pcfg.symbolStrings(lowmem.lhsOfRule(tree1.nodez(x).rule))
                    val sym2 = pcfg.symbolStrings(lowmem.lhsOfRule(tree2.nodez(y).rule))

                    lazy val k1 = tree1.getChildren(x)
                    lazy val k2 = tree2.getChildren(y)

                    val myCell = new SeekCell(iii,jjj,symsMatch(x,y),kidsMatch(x,y)) 

                    //if no kids match then the set is empty
                    if(myCell.fill) {
                      //println("CALCULATING CELL FOR " + sym1 + " v " + sym2)
                      val regSeg = new SegSet()
                      regSeg.indSet ::= x //only keep track of the indexes in tree1
                      myCell.maxSegs ::= regSeg
                      //make regular seg
                      k1 zip k2 foreach(_ match {
                        case (c1,c2) => {
                          val l1 = c1 :: feet1(c1)
                          val l2 = c2 :: feet2(c2)

                          //println("tree 1 nodez - " + l1.toArray.mkString(","))
                          //println("tree 2 nodez - " + l2.toArray.mkString(","))

                          val curCells = myCell.maxSegs
                          var newCells : List[SegSet] = Nil 

                          l1.foreach(kid1 => {
                            l2.foreach(kid2 => {
                              val kCell = getCell(kid1,kid2)
                              val kSegs = kCell.maxSegs
                              //for each segment out of the node add it into the list of current segs
                              //println(kid1 + " -- " + kid2)
                              //println(sym1 + "-" + sym2 + " has a child with " + kSegs.length + " max segs")

                              val warps : List[(Int,Int)] = if(kid1 == c1)
                                Nil
                                                            else
                                                              List((c1,kid1))
                              

                              if(kSegs.length > 0) {
                                newCells :::= kSegs.flatMap(seg => {
                                  seg.top = false
                                  curCells.map(pseg => {
                                    //println("!!!" + warps.mkString("-") + "_" + pseg.warps.mkString("-"))
                                    new SegSet(pseg.indSet ::: seg.indSet,warps ::: seg.warps,-1,true)
                                  })
                                })
                              }
                              //println("So far " + newCells.length + " segs")
                            })
                          })
                          if(newCells.length > 0)
                            myCell.maxSegs = newCells
                          //println("UPDATE CELLS FOR " + sym1 + " v " + sym2)
                          //println(myCell.maxSegs.mkString("\n"))
                        }
                      })

                      //println(sym1 + "-" + sym2 + " has " + myCell.maxSegs.length + " max segs")

                    } 
                    dynamicA(x)(y) = myCell
                    myCell
                  }
                }        

                0.until(tree1.nodez.length).foreach(x => {
                  0.until(tree2.nodez.length).foreach(y => {
                    val cell = getCell(x,y)
                    if(cell.fill) {
                      cell.maxSegs.foreach(seg => {
                        //println(x + "/" + y + " " + seg)
                        if(seg.top) {
                          var marks = Array.tabulate(tree1.nodez.length)(x => true)
                          seg.indSet.foreach(i => marks(i) = false)
                          var warps = Array.tabulate(tree1.nodez.length)(x => -1)
                          seg.warps.foreach(x => {
                            warps(x._1) = x._2
                          })
                          val et = new CTagSeg(tree1,x,marks,warps)
                          grammarSet += et
                        }
                      })
                      feet1(x).foreach(fInd => {
//                        println("Checking TAG from " + x + " to " + fInd)
                        cell.maxSegs.foreach(seg => {
                          val covered = new HashSet[Int]()
                          seg.indSet.foreach(i => {
                            covered += i
                            covered ++= tree1.getChildren(x)
                          })

                          if(covered contains fInd) {
  //                          println("Found a seg for the foot " + fInd)
                            var marks = Array.tabulate(tree1.nodez.length)(x => true)
                            seg.indSet.foreach(i => marks(i) = false)

                            marks(fInd) = true
                            var warps = Array.tabulate(tree1.nodez.length)(x => -1)
                            seg.warps.foreach(x => {
                              warps(x._1) = x._2
                            })
                            warps(fInd) = -1
                            val et = new CTagSeg(tree1,x,marks,warps)

                            val revT = lowmem.revert(et)
                            val a = if(realTAGS contains revT) "1" else "0"
                            val b = if(tagSet contains et) "1" else "0"
                            
                            if(revT.nonterminals.length != et.getNodes().length) {
                              throw new Exception()
                            }

                            if(a != b) {
                               val t1 = lowmem.revert(tree1)
                              val t2 = lowmem.revert(tree2)
                              println(PCFGPrinter.treeToString(pcfg,t1))
                              println(PCFGPrinter.treeToString(pcfg,t2))
                              println(PCFGPrinter.treeToString(pcfg,revT))
                              println("In Real " + a)
                              println("In comp " + b)
                              val e = tagSet.findEntry(et).get
                              println(PCFGPrinter.treeToString(pcfg,lowmem.revert(e)))
                              println("FROM")
                              println(PCFGPrinter.treeToString(pcfg,lowmem.revert(e.tree)))
                              et.print()
                              println("-------------")
                              e.print()

                              throw new Exception()
                            }
                            realTAGS += revT
                            tagSet += et
                          }
                        })
                      })
                    }
                  })
                })

                done += 1
                if(done % 10000 == 0) {
                  println("Finished " + done + " and found " + grammarSet.size + "/" + tagSet.size +" trees so far")
                }

              }
            }
          })
        }
      }
    })
    println("performed " + done + " comparisons")
    (grammarSet.iterator.toArray,tagSet.iterator.toArray)

  }

}


*/
