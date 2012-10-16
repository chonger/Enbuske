package enbuske

import multitool._

class LowMem(pcfg : PCFG, st : CFGSymbolTable) {

  import scala.collection.mutable.HashMap

  var lhsOfRule : Array[Int] = null //record the index of a rule's lhs
  var pcfgProbs : Array[Double] = null

  var rulemap = new HashMap[TreeRule,Int]() //record the enumeration of all rules  
  var termmap = new HashMap[Int,Int]()

  def compactCorpus(fatTreez : Array[XMLDoc[ParseTree]]) : Array[XMLDoc[CParseTree]] = {
    fatTreez.map(_.map(x => convert(new ParseTree(x.root) with Markers)))
  }

  genMaps(pcfg)

  def trim() = {
    println("Trimming LowMem mapping by removing rule enumeration")
    rulemap = null
  }
  
  def genMaps(pcfg : PCFG) {

    rulemap = new HashMap[TreeRule,Int]() //record the enumeration of all rules  
    termmap = new HashMap[Int,Int]() //a map of terminals to rules that derive them

    /**
     *  set up the fast array access PCFG
     */
    var index = 0
    var probs : List[Double] = Nil
    var lhsL : List[Int] = Nil
    var termL : List[Int] = Nil

    pcfg.rules.foreach(rule => {
      rulemap += (rule._1 -> index)
      probs ::= rule._2
      lhsL ::= rule._1.lhs
      index += 1
    })

    pcfg.lexiconRules.foreach(rule => {
      rulemap += (rule._1 -> index)
      termmap += index -> rule._1.terminal
      probs ::= rule._2
      lhsL ::= rule._1.lhs
      index += 1
    })
/**  
    println("LOWMEM Rules")
    rulemap.elements.foreach(r => {
      println(PCFGPrinter.ruleString(pcfg,r._1) + " ---> " + r._2)
    })
  */
    pcfgProbs = probs.reverse.toArray
    lhsOfRule = lhsL.reverse.toArray    
  }
  
  def rootLHS(e : CSegment) = {
    lhsOfRule(e.tree.nodez(e.root).rule)
  }

  def convert(tree : ParseTree with Markers) : CParseTree = {

    //utility for counting 
    def countUnder(n : NonTerminalNode) : Int = {
      n match {
	    case tn : PreTerminalNode => {
          1
        }
	    case pn : ProtoNode => {
          (1 /: pn.children)(_ + countUnder(_))          
        }
	    case un : UnderspecifiedNode => {
          1
	      //throw new Exception("Shouldnt find underspecified nodes")
        }                             
	  }
    }

    var nodez : List[CNode] = Nil

    def walktree(n : NonTerminalNode,parentOff : Int,hasSibling : Boolean) : Unit = {
      val marked = tree.isMarked(n)
      var pind = nodez.length
	  n match {
	    case tn : PreTerminalNode => {
          val sib = if(hasSibling) {pind + 1} else 0
          val cn = new CNode(rulemap(tn.rule),
                             parentOff.toChar,
                             sib.toChar,
                             marked,true)
          nodez ::= cn
        }
	    case pn : ProtoNode => {
          val sib = if(hasSibling) {pind + countUnder(pn)} else 0
          val cn = new CNode(rulemap(pn.rule),
                             parentOff.toChar,
                             sib.toChar,
                             marked,false)
          var sibs = pn.children.map((n) => true).toArray
          sibs(sibs.length - 1) = false
          nodez ::= cn
          (pn.children zip sibs.toList).foreach(_ match {case (c,s) => walktree(c,pind,s)})
        }
	    case un : UnderspecifiedNode => {
          val sib = if(hasSibling) {pind + 1} else 0

          //TODO : improve!!!
          //for now just find a rule that has this LHS
          var iii = 0
          while(lhsOfRule(iii) != un.symbol) {
            iii += 1
          }

          val cn = new CNode(iii.toInt,
                             parentOff.toChar,
                             sib.toChar,
                             true,false)
          nodez ::= cn

	      //throw new Exception("Shouldnt find underspecified nodes")
        }                             
	  }
    }


    walktree(tree.root,0,false)

    new CParseTree(nodez.reverse.toArray)

  }

  def revert(ctree : CParseTree) : ParseTree = {
    revert(new CSegment(ctree,0,ctree.nodez.map(_.mark)))
  }
 
  def revert(cseg : CSegment) : ParseTree = {

    def makeTAGNode(s : CTagSeg, i : Int) : NonTerminalNode = {
      var segNode = s.tree.nodez(i)
      var stub = i != s.root && s.marks(i) //&& s.warps(i) < 0
      val lhs = lhsOfRule(segNode.rule)
      var indo = i
//      print("TAGN - " + i)
      if(stub) {
  //      println(" STUB")
        new UnderspecifiedNode(lhs,null)
      } else {
        if(s.warps(i) != -1) {
          indo = s.warps(i)
    //      print (" WARP to " + indo)
          segNode = s.tree.nodez(indo)
          stub = indo != s.root && s.marks(indo) && s.warps(indo) < 0
        }
        if(stub) {
      //    println(" STUB")
          new UnderspecifiedNode(lhs,null)
        } else {
        //  println(" NOTSTUB")
          if(segNode.isTerm) {
            val lhs = lhsOfRule(segNode.rule)
            val term = termmap(segNode.rule)
            new PreTerminalNode(lhs,new TerminalNode(term))
          } else {
            val ch = s.tree.getChildren(indo).map(x => makeTAGNode(s,x))
            if(ch.length == 0) {
              println(i)
              println(s.warps(i))
              throw new Exception()
            }
            new ProtoNode(lhs,ch)
          }
        }
      }
    }
    
    def makeNode(s : CSegment, i : Int) : NonTerminalNode = {

      val segNode = s.tree.nodez(i)
      val stub = (i != s.root && s.marks(i))
        //println("MN " + i + " -  stub - " + stub)
      val lhs = lhsOfRule(segNode.rule)
      if(stub) {
        new UnderspecifiedNode(lhs,null)
      } else {
        if(segNode.isTerm) {
          val lhs = lhsOfRule(segNode.rule)
          val term = termmap(segNode.rule)
          new PreTerminalNode(lhs,new TerminalNode(term))
        } else {
          val ch = s.tree.getChildren(i).map(x => makeNode(s,x))
          new ProtoNode(lhs,ch)
        }
      }
    }

    cseg match {
      case cts : CTagSeg => {
  //      println(cts)
        val r = new ParseTree(makeTAGNode(cts,cts.root))
        //      println(PCFGPrinter.treeToString(pcfg,r))
        r
      } 
      case _ =>  {
//        println("   !          <>               !")
        new ParseTree(makeNode(cseg,cseg.root))
      }
    }
  }
}


