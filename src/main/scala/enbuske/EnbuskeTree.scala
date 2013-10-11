package enbuske

import multitool._


/**
 *
 *  Traits for Multitool Trees
 *
 */ 


trait Aspect extends ParseTree {
  import scala.collection.mutable.{HashMap,HashSet}
  var aspect = new HashMap[RefWrapper,Int]()
}

trait Markers extends ParseTree {
  import scala.collection.mutable.{HashMap,HashSet}
  val markers = new HashSet[RefWrapper] //the markers define cut points in the tree
  def mark(n : NonTerminalNode) = {
    markers += new RefWrapper(n)
  }
  
  def unmark(n : NonTerminalNode) = {
    markers -= new RefWrapper(n)
  }
  
  def isMarked(n : NonTerminalNode) = {
    markers contains new RefWrapper(n)
  }

  def clearMarkers() = {
    markers.clear()
  }
  
  def randomizeMarkers() : Unit = randomizeMarkers(.5)
  def randomizeMarkers(markProb : Double) : Unit = {
    val rando = new java.util.Random()
    nonterminals.foreach(n => {
    	if(rando.nextDouble() <= markProb)
    		mark(n)
    	else
    		unmark(n)
    })
    mark(root)
  }

  /**
   * Gets a list of tree segments from this tree, as defined by the current markers
   */
  def getSegments : List[ParseTree] = {
	  markers.map(m => new ParseTree(getSegmentFrom(m.n))).toList                                             
  }

  def getSegmentFrom(m : NonTerminalNode) : NonTerminalNode = {
    getSegmentFrom(m,markers)
  }
  
  def getSegmentFrom(m : NonTerminalNode, hs : HashSet[RefWrapper]) : NonTerminalNode = {  
      def recGetSeg(n : NonTerminalNode) : NonTerminalNode = {
		  val ret = 
			  if((!(m eq n)) && (hs contains new RefWrapper(n))) {
				new UnderspecifiedNode(n.symbol,null)  
			  } else {
				  n match {
			  	  	case ptn : PreTerminalNode => new PreTerminalNode(ptn.symbol,new TerminalNode(ptn.kid.terminal))
			  	  	case pn : ProtoNode => new ProtoNode(pn.symbol,pn.children.map(recGetSeg(_)))
			  	  	case un : UnderspecifiedNode => {
			  	  		if(un.assignment != null) 
			  	  		  new UnderspecifiedNode(un.symbol,recGetSeg(un.assignment))
			  	  		else
			  	  		  new UnderspecifiedNode(un.symbol,null)
			  	  	}                             
				  }	
			  }
		  ret
      }
      recGetSeg(m)
  }

  /**
   *   INITIALIZATION
   */
  
  markers += new RefWrapper(root) //the root is always marked
}



/**
 *
 *  Trees used by Lowmem
 *
 */ 



class CNode(val rule : Int, val parent : Char, val sibling : Char, var mark : Boolean, val isTerm : Boolean, var aspect : Char) {
  def this(rule : Int, parent : Char, sibling : Char, mark : Boolean, isTerm : Boolean) = {
    this(rule,parent,sibling,mark,isTerm,0)
  }
} 

class CParseTree(val nodez : Array[CNode]) {

  def print() = {
    println("Compacted Parse Tree")
    nodez.foreach(n => {
      println("R: " + n.rule + " P: " + n.parent.toInt + " S: " + n.sibling.toInt + " T: " + n.isTerm + " M : " + n.mark)
    })
  }

  def clearMarks() = {
    nodez.drop(1).foreach(_.mark = false)
  }

  def getSegments() : Array[CSegment] = {
    val curMarks = nodez.map(_.mark).toArray
    nodez.zipWithIndex.filter(_._1.mark).map(x => new CSegment(this,x._2,curMarks))
  }

  def getChildren(index : Int) : List[Int] = {

    val cn = nodez(index)
    if(cn.isTerm)
      Nil
    else {
      var kids : List[Int] = Nil
      var cInd = index + 1
      var n = nodez(cInd)
      kids ::= cInd
      while(n.sibling != 0) {
        cInd = n.sibling
        n = nodez(cInd)
        kids ::= cInd
      }

      kids.reverse
    }
  }

  def randomMarks() = {
    val rando = new java.util.Random()
    nodez.foreach(n => {
      n.mark = rando.nextBoolean()
    })
  }

}

class CSegment(val tree : CParseTree, val root : Int, val marks : Array[Boolean]) {

  def rNode() = tree.nodez(root)

  def print() = {
    println("Compacted Segment : H=" + myHash)
    getNodes().foreach(x => {
      val n = tree.nodez(x._1)
      if(x._2)
        println(x._1 +" STUB")
      else
        println(x._1+ " R: " + n.rule)
    })
  }
  
  def getNodes() : List[(Int,Boolean)] = {
    var ret : List[(Int,Boolean)] = Nil
    var cur = (root,false)
    while(cur != null) {
      ret ::= cur
      cur = getNext(cur)
    }
    ret.reverse
  }

  def getNext(n : (Int,Boolean)) : (Int,Boolean) = {
    n match {
      case (nodeI,stub) => {
        val node = tree.nodez(nodeI)
        if(node.isTerm || stub) {
          if(nodeI == root)
            null
          else
            getNextUp(n)
        } else {
          val nextI = nodeI + 1
          (nextI,marks(nextI))
        }
      }
    }
  }

  def getNextUp(n : (Int,Boolean)) : (Int,Boolean) = {
    n match {
      case (nodeI,stub) => {
        if(nodeI == root)
          null
        else {
          val node = tree.nodez(nodeI)
          if(node.sibling == 0)
            getNextUp(node.parent,false)
          else
            (node.sibling,marks(node.sibling))
        }
      }
    }
  }

  def nodesEq(t : CSegment) = {
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

  override def equals(any : Any) : Boolean = {
	any match {
	  case t : CSegment => {
        nodesEq(t)
	  }
	  case _ => false
	}	
  }
  
  def nodesHash() = {
    val nodez = getNodes()
    var shift = 0
    (0 /: nodez)((a,n) => {
      val nod = tree.nodez(n._1)
      var rule = if(n._2) {
        0
      } else {
        nod.rule
      }
      val ret = a ^ (rule << shift)
      shift += 2
      if(shift > 28)
        shift = 0
      ret
    })
  }

  def getHash() = nodesHash()
  lazy val myHash = getHash()
  override def hashCode() : Int = myHash
}

