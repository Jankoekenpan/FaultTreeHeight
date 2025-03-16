package reallife

import bdd.BDD
import benchmark.Conversion
import dft.DFT

import java.io.File
import java.nio.file.{Files, Path, StandardOpenOption}
import scala.io.Source

object TestBDDHeights {

    def main(args: Array[String]): Unit = {
//        println(TreesInPaper.runTreeLikeFaultTree(FT3_ATC))
//        println(TreesInPaper.runTreeLikeFaultTree(FT3_ATC2))
//        println(TreesInPaper.runTreeLikeFaultTree(FT3_ATC3))
//        println(TreesInPaper.runTreeLikeFaultTree(FT3_ATC4))
        println(TreesInPaper.runDagLikeFaultTree(FT12_OGPF2))
    }
}

object TreesInPaper {

    // Trees discussed in the paper, always read from .dft file.

    val treeLikeFaultTrees = Seq[TreeLikeFaultTree](
        FT1_AssessingTheRisks2,
        FT3_ATC,
        FT4_Aircraft,
        FT5_LiquidStorageTank,
        FT6_LeakageFailure,
        FT7_AssessingTheRisks1,
        FT9_LossContainerPort,
        FT10_Stopper,
        FT11_PCBA,
        FT13_BHNGPipeline,
        FT14_HSC
    )

    val dagLikeFaultTrees = Seq[DagLikeFaultTree](
        FT2_T0Chopper,
        FT8_ChlorineRelease,
        FT12_OGPF
    )

    val faultTrees: Seq[SimpleFaultTree] =
        treeLikeFaultTrees ++ dagLikeFaultTrees

    def main(args: Array[String]): Unit = {
        val outFile = HeightResults.createOutFile()
        HeightResults.printHeader(outFile)

        for (tree <- treeLikeFaultTrees) {
            val results = {
                if tree == FT14_HSC then {
                    // HSC is known to be problematic for mince since it has 210 cut sets.
                    runTreeLikeFaultTree(tree, runMince = false)
                }
                else if tree == FT1_AssessingTheRisks2 then {
                    // Only AssessingTheRisks2 is small enough to finish the eminent algorithm in reasonable time.
                    runTreeLikeFaultTree(tree, runEminent = true)
                }
                else {
                    runTreeLikeFaultTree(tree)
                }
            }
            println()
            HeightResults.printResults(outFile, results)
        }

        for (tree <- dagLikeFaultTrees) {
            val results = runDagLikeFaultTree(tree)
            println()
            HeightResults.printResults(outFile, results)
        }
    }

    def runTreeLikeFaultTree(treeLikeFT: TreeLikeFaultTree,
                             runEminent: Boolean = false,
                             runRemind: Boolean = true,
                             runPase: Boolean = true,
                             runMince: Boolean = true,
                             runBDD: Boolean = true): HeightResults = {
        println(s"Processing tree-like fault tree ${treeLikeFT.name}")
        val treeFT = treeLikeFT.FT
        val (dagFT, probabilities) = Conversion.translateToDagTree(treeFT)
        val basicEvents = minimalcutpathset.getBasicEvents(dagFT)
        val (booleanFormula, _) = Conversion.translateToBooleanFormula(treeFT)
        val bdd = BDD.readStormSylvanBDDDotFile(new File(s"generated/bdd/${treeLikeFT.name}.dot"))
        val bddProbabilities = BDD.bddProbabilities(DFT.readDFTFile(Source.fromFile(new File(s"handcreated/${treeLikeFT.name}.dft"))))

        println("Flattening tree for recursive algorithm...")
        val flattenedTree = faulttree.flatten(treeFT)
        println("Calculating minimal cut sets...")
        val minimalCutSets = minimalcutpathset.minimalCutSets(dagFT)(basicEvents)
        println("Calculating minimal path sets...")
        val minimalPathSets = minimalcutpathset.minimalPathSets(dagFT)(basicEvents)

        println(s"DEBUG: cut sets (${minimalCutSets.size}) = ${minimalCutSets}")
        println(s"DEBUG: path sets (${minimalPathSets.size}) = ${minimalPathSets}")

        val heightEnumeration: Double = if runEminent then {
            println(s"Calculate height of ${treeLikeFT.name} using Enumeration algorithm (eminent)...")
            decisiontree.height(booleanFormula, probabilities)
        } else -1

        val heightRecursive2: Double = if runRemind then {
            println(s"Calculate height of ${treeLikeFT.name} using Recursive algorithm 2 (remind)...")
            faulttree.height7(flattenedTree) // Note: recursive algorithm uses flattened FT.
        } else -1

        val heightPathSet: Double = if runPase then {
            println(s"Calculate height of ${treeLikeFT.name} using PathSet algorithm (pase)...")
            minimalcutpathset.algorithm5(minimalPathSets, probabilities)._2
        } else -1

        val heightCutSet: Double = if runMince then {
            println(s"Calculate height of ${treeLikeFT.name} using CutSet algorithm (mince)...")
            minimalcutpathset.algorithm4(minimalCutSets, probabilities)._2
        } else -1

        val heightBDD: Double = if runBDD then {
            println(s"Calculate height of ${treeLikeFT.name} using BDD algorithm (storm-dft)...")
            BDD.height(bdd, bddProbabilities)
        } else -1

        HeightResults(
            treeName = treeLikeFT.name,
            basicEvents = basicEvents.size,
            cutSets = minimalCutSets.size,
            pathSets = minimalPathSets.size,
            heightExact = heightEnumeration,
            heightRecursive = heightRecursive2,
            heightCutSet = heightCutSet,
            heightPathSet = heightPathSet,
            heightBDD = heightBDD
        )
    }

    def runDagLikeFaultTree(dagLikeFT: DagLikeFaultTree,
                            runEminent: Boolean = false,
                            runRemind: Boolean = true,
                            runPase: Boolean = true,
                            runMince: Boolean = true,
                            runBDD: Boolean = true): HeightResults = {
        println(s"Processing dag-like fault tree ${dagLikeFT.name}")
        val dagFT = dagLikeFT.FT
        val basicEvents = minimalcutpathset.getBasicEvents(dagFT)
        val (booleanFormula, probabilities) = Conversion.translateToBooleanFormula(dagFT)
        val bdd = BDD.readStormSylvanBDDDotFile(new File(s"generated/bdd/${dagLikeFT.name}.dot"))
        val bddProbabilities = BDD.bddProbabilities(DFT.readDFTFile(Source.fromFile(new File(s"handcreated/${dagLikeFT.name}.dft"))))

        println("Flattening tree for recursive algorithm")
        val flattenedDag = minimalcutpathset.flatten(dagFT)
        println("Calculating minimal cut sets...")
        val minimalCutSets = minimalcutpathset.minimalCutSets(dagFT)(basicEvents)
        println("Calculating minimal path sets...")
        val minimalPathSets = minimalcutpathset.minimalPathSets(dagFT)(basicEvents)

        val heightEnumeration: Double = if runEminent then {
            println(s"Calculate height of ${dagLikeFT.name} using Enumeration algorithm (eminent)...")
            decisiontree.height(booleanFormula, probabilities)
        } else -1

        val heightRecursive3: Double = if runRemind then {
            println(s"Calculate height of ${dagLikeFT.name} using Recursive algorithm 3 (remind)...")
            decisiontree.algorithm8(flattenedDag, probabilities)._2 // Note: recursive algorithm uses flattened FT.
        } else -1

        val heightCutSet: Double = if runMince then {
            println(s"Calculate height of ${dagLikeFT.name} using CutSet algorithm (mince)...")
            minimalcutpathset.algorithm4(minimalCutSets, probabilities)._2
        } else -1

        val heightPathSet: Double = if runPase then {
            println(s"Calculate height of ${dagLikeFT.name} using PathSet algorithm (pase)...")
            minimalcutpathset.algorithm5(minimalPathSets, probabilities)._2
        } else -1

        val heightBDD: Double = if runBDD then {
            println(s"Calculate height of ${dagLikeFT.name} using BDD algorithm (storm-dft)...")
            BDD.height(bdd, bddProbabilities)
        } else -1

        HeightResults(
            treeName = dagLikeFT.name,
            basicEvents = basicEvents.size,
            cutSets = minimalCutSets.size,
            pathSets = minimalPathSets.size,
            heightExact = heightEnumeration,
            heightRecursive = heightRecursive3,
            heightCutSet = heightCutSet,
            heightPathSet = heightPathSet,
            heightBDD = heightBDD
        )
    }

}

case class HeightResults(
    treeName: String,
    basicEvents: Int,
    cutSets: Int,
    pathSets: Int,
    heightExact: Double,
    heightRecursive: Double,
    heightCutSet: Double,
    heightPathSet: Double,
    heightBDD: Double
)

object HeightResults {
    private val outFile = "real-world-fault trees.csv"

    def createOutFile(): Path =
        Files.createFile(Path.of(outFile))

    def printHeader(file: Path): Unit = {
        val line = "Fault Tree,# Basic events, # Minimal cut sets,# Minimal path sets,Height (eminent),Height (remind),Height (mince),Height (pase),Height (storm-dft bdd)"
        writeLine(file, line)
    }

    def printResults(file: Path, results: HeightResults): Unit = {
        val line = s""""${results.treeName}","${results.basicEvents}","${results.cutSets}","${results.pathSets}","${results.heightExact}","${results.heightRecursive}","${results.heightCutSet}","${results.heightPathSet}","${results.heightBDD}""""
        writeLine(file, line)
    }

    private def writeLine(file: Path, string: String): Unit =
        Files.writeString(file, string + "\r\n", StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.APPEND)

}

object FT1_AssessingTheRisks2 extends TreeLikeFaultTree {
    import faulttree.FaultTree
    override def name = "AssessingtheRisks2(FT1)"
    val FT: FaultTree = DFT.readTreeLikeFaultTree(Source.fromFile("handcreated/AssessingtheRisks2(FT1).dft"))
}

object FT2_T0Chopper extends DagLikeFaultTree {
    import minimalcutpathset.FaultTree
    override def name = "T0 Chopper(FT2)"
    val FT: FaultTree = DFT.readDagLikeFaultTree(Source.fromFile("handcreated/T0 Chopper(FT2).dft"))
}

object FT3_ATC extends TreeLikeFaultTree {
    import faulttree.FaultTree
    override def name = "ATC(FT3)";
    val FT: FaultTree = DFT.readTreeLikeFaultTree(Source.fromFile("handcreated/ATC(FT3).dft"))
}

object FT3_ATC2 extends TreeLikeFaultTree {
    import faulttree.FaultTree
    override def name = "ATC2(FT3)";
    val FT: FaultTree = DFT.readTreeLikeFaultTree(Source.fromFile("handcreated/ATC2(FT3).dft"))
}

object FT3_ATC3 extends TreeLikeFaultTree {
    import faulttree.FaultTree
    override def name = "ATC3(FT3)";
    val FT: FaultTree = DFT.readTreeLikeFaultTree(Source.fromFile("handcreated/ATC3(FT3).dft"))
}

object FT3_ATC4 extends TreeLikeFaultTree {
    import faulttree.FaultTree
    override def name = "ATC4(FT3)";
    val FT: FaultTree = DFT.readTreeLikeFaultTree(Source.fromFile("handcreated/ATC4(FT3).dft"))
}


object FT4_Aircraft extends TreeLikeFaultTree {
    import faulttree.FaultTree
    override def name = "Aircraft(FT4)"
    val FT: FaultTree = DFT.readTreeLikeFaultTree(Source.fromFile("handcreated/Aircraft(FT4).dft"))
}

object FT5_LiquidStorageTank extends TreeLikeFaultTree {
    import faulttree.FaultTree
    override def name = "LiquidStorageTank(FT5)"
    val FT: FaultTree = DFT.readTreeLikeFaultTree(Source.fromFile("handcreated/LiquidStorageTank(FT5).dft"))
}

object FT6_LeakageFailure extends TreeLikeFaultTree {
    import faulttree.FaultTree
    override def name = "Leakagefailure(FT6)"
    val FT: FaultTree = DFT.readTreeLikeFaultTree(Source.fromFile("handcreated/Leakagefailure(FT6).dft"))
}

object FT7_AssessingTheRisks1 extends TreeLikeFaultTree {
    import faulttree.FaultTree
    override def name = "AssessingtheRisks1(FT7)"
    val FT: FaultTree = DFT.readTreeLikeFaultTree(Source.fromFile("handcreated/AssessingtheRisks1(FT7).dft"))
}

object FT8_ChlorineRelease extends DagLikeFaultTree {
    import minimalcutpathset.FaultTree
    override def name = "Chlorine_release(FT8)"
    val FT: FaultTree = DFT.readDagLikeFaultTree(Source.fromFile("handcreated/Chlorine_release(FT8).dft"))
}

object FT9_LossContainerPort extends TreeLikeFaultTree {
    import faulttree.FaultTree
    override def name = "loss_container_port(FT9)"
    val FT: FaultTree = DFT.readTreeLikeFaultTree(Source.fromFile("handcreated/loss_container_port(FT9).dft"))
}

object FT10_Stopper extends TreeLikeFaultTree {
    import faulttree.FaultTree
    override def name = "stopper (FT10)"
    val FT: FaultTree = DFT.readTreeLikeFaultTree(Source.fromFile("handcreated/stopper (FT10).dft"))
}

object FT11_PCBA extends TreeLikeFaultTree {
    import faulttree.FaultTree
    override def name = "PCBA(FT11)"
    val FT: FaultTree = DFT.readTreeLikeFaultTree(Source.fromFile("handcreated/PCBA(FT11).dft"))
}

object FT12_OGPF extends DagLikeFaultTree {
    import minimalcutpathset.FaultTree
    override def name = "ogpf(FT12)"
    val FT: FaultTree = DFT.readDagLikeFaultTree(Source.fromFile("handcreated/ogpf(FT12).dft"))
}

object FT12_OGPF2 extends DagLikeFaultTree {
    import minimalcutpathset.FaultTree
    override def name = "ogpf2(FT12)"
    val FT: FaultTree = DFT.readDagLikeFaultTree(Source.fromFile("handcreated/ogpf2(FT12).dft"))
}

object FT13_BHNGPipeline extends TreeLikeFaultTree {
    import faulttree.FaultTree
    override def name = "BHNGPipeline(FT13)"
    val FT: FaultTree = DFT.readTreeLikeFaultTree(Source.fromFile("handcreated/BHNGPipeline(FT13).dft"))
}

object FT14_HSC extends TreeLikeFaultTree {
    import faulttree.FaultTree
    override def name = "HSC(FT14)"
    val FT: FaultTree = DFT.readTreeLikeFaultTree(Source.fromFile("handcreated/HSC(FT14).dft"))
}
