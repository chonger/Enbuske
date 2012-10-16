package enbuske

import multitool._
import collection.mutable.HashMap

class PCFG(val st : CFGSymbolTable) {

  def this(st : CFGSymbolTable,filE : String) = {
    this(st)
    estimateCore(x => {
      st.lazyReader(filE,x)
    })
  }
  def this(st : CFGSymbolTable,dox : Array[XMLDoc[ParseTree]]) = {
    this(st)
    estimateCore(x => {
      dox.foreach(_.text.foreach(t => x(t)))
    })
  }
  def this(st : CFGSymbolTable,trees : List[ParseTree]) = {
    this(st)
     estimateCore(x => {
      trees.foreach(t => x(t))
    })
  }

  var rules = new HashMap[ProtoRule,Double]()
  var lexiconRules = new HashMap[TerminalRule,Double]()

  def lexScore(tr : TerminalRule) = {
    lexiconRules.getOrElse(tr,0.0)
  }

  def protoScore(pr : ProtoRule) = {
    rules.getOrElse(pr,0.0)
  }
    
  def scoreTree(tree : ParseTree) : Double = {
    scoreFromNode(tree.root)
  }
  
  def scoreFromNode(n : NonTerminalNode) : Double = {
    n match {
      case ptn : PreTerminalNode => {
        lexScore(ptn.rule)
      }
      case un : UnderspecifiedNode => {
        if(un.assignment != null)
          scoreFromNode(un.assignment)
        else 
          1.0
      }
      case pn : ProtoNode => {
        (protoScore(pn.rule) /: pn.children)((a,b) => a * scoreFromNode(b)) 
      }
    }
  }

  def estimateCore(estFunc : ((ParseTree => Unit) => Unit)) = {
    
    val nonterminalCounts = new HashMap[Int,Int]()
    val ruleCounts = new HashMap[TreeRule,Int]()

    rules.keySet.foreach(r => {
      ruleCounts += r -> 1
      nonterminalCounts += r.lhs -> 1
    })
    lexiconRules.keySet.foreach(r => {
      ruleCounts += r -> 1
      nonterminalCounts += r.lhs -> 1
    })
    

    def getCounts(tree : ParseTree) : Unit = {
	  recGetCounts(tree.root)
    }	
  
    def recGetCounts(n : NonTerminalNode) : Unit = {
  	  var ntCount = nonterminalCounts.getOrElse(n.symbol,0) + 1
  	  nonterminalCounts += (n.symbol -> ntCount)
      n match {
	    case pt : PreTerminalNode => { 
		  val rule = new TerminalRule(pt.symbol,pt.kid.terminal)
		  var rCount = ruleCounts.getOrElse(rule,0) + 1
		  ruleCounts += (rule -> rCount)
          	  
  	    }
	    case nt : ProtoNode => {
		  val rule = nt.rule
  	   	  var rCount = ruleCounts.getOrElse(rule,0) + 1
  	   	  ruleCounts += (rule -> rCount)
          //recursively continue committing data
  	   	  nt.children.foreach(recGetCounts(_))
  	    }
	  }
    }

    estFunc(getCounts)

    //trees.foreach(d => getCounts(d))
    rules.clear
	lexiconRules.clear
	ruleCounts.foreach(_ match {
	  case (rule,d) => {
	    val ntCount = nonterminalCounts(rule.lhs).toDouble
	    rule match {	
	      case tr : TerminalRule => {
            val p = (tr -> (d / ntCount))
              if(p._2 == 0)
                println("tZ" + p)  
            lexiconRules += p
          }
	      case pr : ProtoRule => {
            val p = (pr -> (d / ntCount))
              if(p._2 == 0)
                println("pZ" + p)  
            rules += p
          }
	    }
	  }
    })
  }

}
