package enbuske

import scala.collection.mutable.{HashMap,HashSet}
import multitool._

class CRefTree(val tree : CSegment) {
  override def equals(any : Any) : Boolean = {
	any match {
	  case t : CRefTree => {
		return tree eq t.tree
	  }
	  case _ => false
	}	
  }
  override def hashCode() : Int = tree.hashCode()
}

abstract class COD {

  def findOverlays(tree : CParseTree, root : Int) : List[(CSegment,List[Int])]
  def addTree(t : CSegment) 
  def remTree(t : CSegment)

}

class NoCodCompacter(val lowmem : LowMem, inTrees : Array[List[CSegment]]) extends COD {

  def this(lowmem: LowMem, nSyms : Int) = {
    this(lowmem, (for(i <- 0 until nSyms) yield {List[CSegment]()}).toArray)
  }

  import scala.collection.mutable.HashSet
  val store : Array[HashSet[CSegment]] = inTrees.map(x => {
    new HashSet[CSegment]()
  })

  override def findOverlays(tree : CParseTree, root : Int) : List[(CSegment,List[Int])] = {
    var ret : List[(CSegment,List[Int])] = Nil

    def walkwith(r : Int, seg : CSegment, sI : Int, tr : CParseTree, tI : Int) : List[Int] = {
      if(sI != r && seg.marks(sI))
        return List(tI)

      val segNode = seg.tree.nodez(sI)
      val trNode = tr.nodez(tI)
      
      if(trNode.rule != segNode.rule)
        return null
      
      val ch1 = seg.tree.getChildren(sI)
      val ch2 = tr.getChildren(tI)

      val chZip = ch1 zip ch2

      val walkZ = chZip.map(_ match {
        case (a,b) => walkwith(r,seg,a,tr,b)
      })

      if(walkZ.filter(_ == null).size > 0)
        return null
      
      return walkZ.flatMap(a => a)
    }

    val rootnode = tree.nodez(root)
    val sym = lowmem.lhsOfRule(rootnode.rule)
    store(sym).iterator.foreach(seg => {
      val find = walkwith(seg.root,seg,seg.root,tree,root)
      if(find != null) {
        ret ::= (seg,find)
      }
    })
    ret
  }

  override def addTree(t : CSegment) = {
    val sym = lowmem.rootLHS(t)
    store(sym) += t
  }

  override def remTree(t : CSegment) = {
    val sym = lowmem.rootLHS(t)
    store(sym) -= t
  }

}

class LMCompacter(val lowmem : LowMem, inTrees : Array[List[CSegment]]) extends COD {

  def this(lowmem: LowMem, nSyms : Int) = {
    this(lowmem, (for(i <- 0 until nSyms) yield {List[CSegment]()}).toArray)
  }

  val store : Array[LMCompactTree] = inTrees.map(ar => { //for each nonterminal's e-trees
    val t = new LMCompactTree()
    ar.map(tree => t.add(lowmem,tree,tree.root))
    t
  })

  override def findOverlays(tree : CParseTree, root : Int) : List[(CSegment,List[Int])] = {  
    val sym = lowmem.lhsOfRule(tree.nodez(root).rule)
    store(sym).findCompatible(lowmem,tree,root).iterator.map(_ match {
      case (rtree,leaves) => {
        (rtree.tree,leaves)
      }
    }).toList
  }

  override def addTree(t : CSegment) = {
    val sym = lowmem.rootLHS(t)
    store(sym).add(lowmem,t,t.root)
  }

  override def remTree(t : CSegment) = {
    val sym = lowmem.rootLHS(t)
    store(sym).remove(lowmem,t,t.root)
  }

}

class LMCompactTree() {

  var endsHere = new HashSet[CRefTree]()

  import scala.collection.mutable.ArrayBuffer

  var children = new ArrayBuffer[HashMap[(Int,Int),LMCompactTree]]()
  var terminalKids = new HashMap[Int,LMCompactTree]()

  def isEmpty() = {
    endsHere.size == 0 &&
    children.length == 0 &&
    terminalKids.size == 0
  }
  
  def recPrint(st : CFGSymbolTable) : Unit = {
    println("Compact Tree Node")
    
    println("These segments end here")
    endsHere.foreach(e => {
      println(e.tree.print())
    })
    
    println("Here are my children")
    var ind = 0
    children.iterator.foreach(c => {
      println("Kids at " + ind)
      ind += 1
      c.iterator.foreach(_ match {
        case (key,ctree) => {
          println("KEY - " + st.syms(key._1) + " --- Arity : " + key._2)
          ctree.recPrint(st)
        }
      })
    })

    println("Terminal kids")
    terminalKids.iterator.foreach(_ match {
      case (key,ctree) => {
        println("T KEY - " + key)
        ctree.recPrint(st)
      }
    })
    
    println("DONE printin")
  }

  def add(lowmem : LowMem, seg : CSegment, index : Int) : Unit = {
    val node = seg.tree.nodez(index)
    if(index != seg.root && seg.marks(index)) {
      endsHere += new CRefTree(seg)
    } else if(node.isTerm) {
      //println("adding to TERM")
      val entry = terminalKids.getOrElse(node.rule,new LMCompactTree()) 
      entry.endsHere += new CRefTree(seg)
      terminalKids += node.rule -> entry
    } else {
      val kids = seg.tree.getChildren(index)
      //println("!!!! " + kids)
      val myArity = kids.length

      var cindex = 0
      kids.foreach(c => {
        var bin = if(children.length <= cindex) {
          var nMap = new HashMap[(Int,Int),LMCompactTree]()
          children append nMap
          nMap
        } else {
          children(cindex)
        }
        cindex += 1
        val childNode = seg.tree.nodez(c)
        val lhs = lowmem.lhsOfRule(childNode.rule)
        val k = (lhs,myArity)
          
        val entry : LMCompactTree = bin.getOrElseUpdate(k,new LMCompactTree())
        entry.add(lowmem,seg,c)
      })
    }
  }


  def remove(lowmem : LowMem, seg : CSegment, index : Int) : Unit = {
    val node = seg.tree.nodez(index)
    if(index != seg.root && seg.marks(index)) {
      endsHere.iterator.foreach(el => {
        if(el.tree == seg) 
          endsHere -= el
      })
    } else if(node.isTerm) {
      val entry = terminalKids(node.rule) //get the tree here
      entry.endsHere.iterator.foreach(el => {
        if(el.tree == seg) 
          entry.endsHere -= el
      })
      if(entry.isEmpty())
        terminalKids -= node.rule
    } else {
      val kids = seg.tree.getChildren(index)
      val myArity = kids.length

      var cindex = 0
      kids.foreach(c => {
        val bin = children(cindex)
        cindex += 1
        val childNode = seg.tree.nodez(c)
        val lhs = lowmem.lhsOfRule(childNode.rule)
        val k = (lhs,myArity)
        val entry : LMCompactTree = bin(k)
        entry.remove(lowmem,seg,c)
        if(entry.isEmpty())
          bin -= k
      })
    }
  }

  def findCompatible(lowmem : LowMem, ctree : CParseTree, index : Int) : HashMap[CRefTree,List[Int]] = {

    var ret = new HashMap[CRefTree,List[Int]]()
    endsHere.foreach(e => {
      ret += e -> List(index)
    })

    val targNode = ctree.nodez(index)

    if(targNode.isTerm) {
      val entry : LMCompactTree = terminalKids.getOrElse(targNode.rule,null)
      if(entry != null)
        entry.endsHere.foreach(e => ret += e -> Nil)
    } else {
      val kids = ctree.getChildren(index)
      val myArity = kids.length
      if(myArity > children.length)
        return ret

      var cindex = 0
      val compatMaps = kids.map(c => {
        val bin = children(cindex)
        cindex += 1
        val childNode = ctree.nodez(c)
        val lhs = lowmem.lhsOfRule(childNode.rule)

        val entry = bin.getOrElse((lhs,myArity),null)
        if(entry != null) {
          entry.findCompatible(lowmem,ctree,c)
        } else {
          //println("No segments found, aborting at " + Compacter.pcfg.symbolStrings(c.symbol) + "/" + myArity)
          return ret
        }
      })

      var intersect = compatMaps(0)
      compatMaps.drop(1).foreach(m => {
        val keys = intersect.keySet
        keys.foreach(k => {
          if(! m.keySet.contains(k)) {
            intersect -= k
          } else {
            val en = intersect(k)
            intersect += k -> (en ::: m(k))
          }
        })
      })
      intersect.iterator.foreach(e => ret += e)
    }
    ret
  }
}
/**
class TAGCOD(val lowmem : LowMem, inTrees : Array[List[CSegment]], val footF : (CParseTree => Array[List[Int]])) {

  def this(lowmem: LowMem, nSyms : Int,footF : (CParseTree => Array[List[Int]])) = {
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
    val warps = footF(ctree)
    
    def recFindTAG(ctree : CParseTree, index : Int, packed : PackTree) : HashMap[CRefTree,List[(List[(Int,Int)],List[Int])]] = {
      var ret = new HashMap[CRefTree,List[(List[(Int,Int)],List[Int])]]()
      packed.endsHere.foreach(e => {
        ret += e -> List((List[(Int,Int)](),List(index)))
      })

      val allopts = index :: warps(index)

      allopts.foreach(ind => {

        val targNode = ctree.nodez(ind)

        if(targNode.isTerm) { 

          val entry : PackTree = packed.terminalKids.getOrElse(targNode.rule,null)
          if(entry != null) {
            entry.endsHere.foreach(e => {
              if(ind == index) {
                ret += e -> List((List[(Int,Int)](),Nil))
              } else {
                ret += e -> List((List[(Int,Int)]((index,ind)),Nil))
              }
            })
          }
        } else {
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
      })              

      ret
    } //end of recursive bit

    val ret = recFindTAG(ctree, index, packed)

    ret
  }
}
*/
