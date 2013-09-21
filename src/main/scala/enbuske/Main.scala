package enbuske
import scala.collection.mutable.HashMap

object Main {

  def usage() : String = {
    println()
    println()
    println("Usage: use one of the following commands as the first argument,")
    println("       followed by key-value argument pairs")
    println()
    println("\thelp")
    println("\tprepare")
    println("\tsingle")
    println("\tdiagonal")
    println("\tmix")
    println("\tcontinue")
    println("\tcollect")
    println()
    println("use help to see the required and optional arguments for a command, e.g. \"help single\"")
    println()
    println("For additional information see the README or www.cs.brown.edu/~chonger/enbuske/")
    println()
    println()
    System.exit(-1)
    "X"
  }

  def printHelp(s : String) = {
    println()
    println()
    s match {
      case "prepare" => {
        println("prepare - Turn text files from a dir into a multitool XMLDoc needed for input to Enbuske.")
        println("          One data type is created for each document and data type labels set to the filenames.")
        println()
        println("\trequired arguments")
        println("\t\t-in\t\ta directory containing multiple files of trees in PTB format")
        println("\t\t-out\t\toutput file path, in Multitool XMLDoc format (an xml file)")
        println()
        println()
        println("Example:")
        println("java -jar enbuske.jar Main prepare -in dir_containing_files/ -out foo.xml")
      }
      case "single" => {
        println("single - Degenerate form that recreates single grammar block ")
        println("         sampler from Cohn/Blunsom 2010") 
        println()
        println("\trequired arguments")
        println("\t\t-in\t\tinput data file path, in Multitool XMLDoc format")
        println("\t\t-out\t\toutput file path of sampler state")
        println("\t\t-iters\t\tnumber of sampling iterations")
        println("\toptional arguments")
        println("\t\t-pp\t\ta file path to dump posterior probabilities, easily plotted with gnuplot")
        println()
        println()
        println("Example:")
        println("java -jar enbuske.jar Main single -in foo.xml -out bar.samp -iters 100 -pp pp.txt")
      }
      case "diagonal" => {
        println("diagonal - Learn one grammar for each data type.  The amount that these grammars ")
        println("           mutually smooth each other is controlled with \"gamma\", the concentration")
        println("           parameter of the base Dirichlet Process.  High gamma values cause more ")
        println("           independance between grammars")
        println()
        println("\trequired arguments")
        println("\t\t-in\t\tinput data file path, in Multitool XMLDoc format")
        println("\t\t-out\t\toutput file path of sampler state")
        println("\t\t-iters\t\tnumber of sampling iterations")
        println("\toptional arguments")
        println("\t\t-pp\t\ta file path to dump posterior probabilities, easily plotted with gnuplot")
        println("\t\t-dG\t\tgamma parameter described above - default = 100.")
        println("\t\t-semi\t\tpath to an XMLDoc to be used as additional unsupervised data")
        println()
        println("Example:")
        println("java -jar enbuske.jar Main diagonal -in foo.xml -out bar.samp -iters 100 -pp pp.txt -dG 100")
      }
      case "mix" => {
        println("mix - Let the data decide how to connect data and grammars (similar to topic modeling)")
        println()
        println("\trequired arguments")
        println("\t\t-in\t\tinput data file path, in Multitool XMLDoc format")
        println("\t\t-out\t\toutput file path of sampler state")
        println("\t\t-iters\t\tnumber of sampling iterations")
        println("\t\t-nG\t\tnumber of grammars")
        println("\toptional arguments")
        println("\t\t-pp\t\ta file path to dump posterior probabilities, easily plotted with gnuplot")
        println("\t\t-semi\t\tpath to an XMLDoc to be used as additional unsupervised data")
        println()
        println("Example:")
        println("java -jar enbuske.jar Main mix -in foo.xml -out bar.samp -iters 100 -pp pp.txt -nG 10")
      }
      case "continue" => {
        println("continue - sample an existing dataset starting from a given sampler state")
        println()
        println("\trequired arguments")
        println("\t\t-in\t\tinput data file path, in Multitool XMLDoc format")
        println("\t\t-cur\t\tstarting sampler state")
        println("\t\t-out\t\toutput file path of sampler state")
        println("\t\t-iters\t\tnumber of sampling iterations")
        println()
        println("\toptional arguments")
        println("\t\t-pp\t\ta file path to dump posterior probabilities, easily plotted with gnuplot")
        println()
        println("Example:")
        println("java -jar enbuske.jar Main continue -in foo.xml -cur boo.samp -out bar.samp -iters 100 -pp pp.txt")
      }
      case "collect" => {
        println("collect - collect and average samples from a converged MCMC chain and print PTSGs")
        println()
        println("\trequired arguments")
        println("\t\t-in\t\tinput data file path, in Multitool XMLDoc format")
        println("\t\t-cur\t\tstarting sampler state")
        println("\t\t-out\t\toutput file base for the resulting PTSGs")
        println("\t\t-iters\t\tnumber of samples to average")
        println("\t\t-every\t\tnumber of iterations per sample")
        println()
        println("Example: (this would take run for 50 iters, averaging 5 samples after every 10 samples")
        println("java -jar enbuske.jar Main collect -in foo.xml -cur boo.samp -out bar.ptsg -iters 5 -every 10")
      }
      case _ => {
        println("The command \"" + s + "\" is not valid")
        usage()
      }
    }
    println()
    println()
  }
    
  def main(args : Array[String]) {

    if(args.length < 3)
      usage()

    val opts = new HashMap[String,String]()
    if(args.length > 2)
      (args.drop(2) zip args.drop(2).tail).foreach(opts += _)

    val inF : String = opts.getOrElse("-in",null)
    val ppF : String = opts.getOrElse("-pp",null)
    val semiF : String = opts.getOrElse("-semi",null)
    val sampF : String = opts.getOrElse("-out",null)
    val curF : String = opts.getOrElse("-cur",null)
    val every : Int = opts.getOrElse("-every","10").toInt
    val nIter : Int = opts.getOrElse("-iters","-1").toInt
    val dG : Double = opts.getOrElse("-diagG","100").toDouble
    val nTop : Int = opts.getOrElse("-nG","-1").toInt


    args(1) match {
      case "help" => {
        printHelp(args(2))
      }
      case "prepare" => {
        import java.io.File
        import multitool.{CFGSymbolTable,XMLDoc}
        val st = new CFGSymbolTable()
        val dox = new File(inF).listFiles().map(f => {
          val tz = st.read(f.getAbsolutePath()).filter(t => {
            val nt = t.terminals.length 
            nt < 14 && nt >= 8
          }).slice(0,100)
          st.write(f.getAbsolutePath() + "2",tz)
        })
         
        //val lRegex = ".*/([^/\\.]*)\\..*".r
        /**
        val dox = new File(inF).listFiles().map(f => {
          val lRegex(label) = f.toString()
          println("Loading file " + f.toString() + " with label : " + label)
          new XMLDoc(st.read(f.getAbsolutePath()),Array(("goldLabel",label)))
        }).toArray
        XMLDoc.write(sampF,dox,st)
        */
      }
      case "single" => {
        val factory = new SampleSingle()
        if(semiF != null)
          println("Semi-supervised learning doesn't happen with a single grammar - aborting")
        else
          factory(inF,nIter,ppF,sampF)
      }
      case "diag" => {
        val factory = new SampleDiagonal(dG)
        if(semiF != null)
          factory(inF,semiF,nIter,ppF,sampF)
        else
          factory(inF,nIter,ppF,sampF)
      }
      case "mix" => {
        if(nTop <= 0)
          usage()
        val factory = new SampleMix(nTop)
        if(semiF != null)
          factory(inF,semiF,nIter,ppF,sampF)
        else
          factory(inF,nIter,ppF,sampF)
      }
      case "continue" => {
        if(curF == null)
          usage()
        ContinueSample(inF,curF,nIter,ppF,sampF)
      }
      case "collect" => {
        if(curF == null)
          usage()
        val ptsgs = CollectSamples(inF,curF,every,nIter)
        ptsgs.zipWithIndex.foreach({
          case (g,i) => {
            g.write(sampF + "-" + i)
          }
        })
      }
      case _ => {
        println()
        println()
        println(args(1) + " is not a recognized command")
        usage()
      }
    }
  }

}
