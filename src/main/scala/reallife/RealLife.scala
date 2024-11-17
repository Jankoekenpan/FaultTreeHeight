package reallife // or is this just fantasy?

import benchmark.Conversion
import decisiontree.RandomBDTs
import dft.DFT

import java.util.random.RandomGenerator
import scala.io.Source

trait SimpleFaultTree {
    def name: String = getClass.getSimpleName match
        case s"${name}$$" => name
        case name => name
}

trait TreeLikeFaultTree extends SimpleFaultTree {
    def FT: faulttree.FaultTree
}

trait DagLikeFaultTree extends SimpleFaultTree {
    def FT: minimalcutpathset.FaultTree
}

object RealLife {

    def runTreeLikeFaultTree(faultTree: faulttree.FaultTree)(using random: RandomGenerator): Unit = {
        val (formula, probabilities1) = Conversion.translateToBooleanFormula(faultTree)
        val (dagTree, probabilities2) = Conversion.translateToDagTree(faultTree)
        assert(probabilities1 == probabilities2)
        val basicEvents = minimalcutpathset.getBasicEvents(dagTree)

        val heightRecursive2 = faulttree.height(faultTree)
        val heightCutSet = minimalcutpathset.height4(dagTree, basicEvents, probabilities2)
        val heightPathSet = minimalcutpathset.height5(dagTree, basicEvents, probabilities2)
        val heightRandomBDT = RandomBDTs.algorithm13(formula, probabilities1)
        println(s"Heights: recursive2=$heightRecursive2, cutset=$heightCutSet, pathset=$heightPathSet, randomBDT=$heightRandomBDT")
    }

    def runDagLikeFaultTree(faultTree: minimalcutpathset.FaultTree)(using random: RandomGenerator): Unit = {
        val (formula, probabilities) = Conversion.translateToBooleanFormula(faultTree)
        val basicEvents = minimalcutpathset.getBasicEvents(faultTree)

        val heightRecursive3 = decisiontree.algorithm8(faultTree)._2
        val heightCutSet = minimalcutpathset.height4(faultTree, basicEvents, probabilities)
        val heightPathSet = minimalcutpathset.height5(faultTree, basicEvents, probabilities)
        val heightRandomBDT = RandomBDTs.algorithm13(formula, probabilities)
        println(s"Heights: recursive3=$heightRecursive3, cutset=$heightCutSet, pathset=$heightPathSet, randomBDT=$heightRandomBDT")
    }

    def runTrees(): Unit = {
        given random: RandomGenerator = new java.util.Random()

        println("Tree-like Fault Trees:")
        runTreeLikeFaultTree(AircraftRunwayExcursionAccidents.FT)
        runTreeLikeFaultTree(MainTrackTrainCollisionsLeadingToFatalitiesAndInjuries.FT)
        runTreeLikeFaultTree(ATCFailsToResolveTheConflict.FT)
        runTreeLikeFaultTree(LiquidStorageTank.FT)
        runTreeLikeFaultTree(LossContainerAtPort.FT)
        runTreeLikeFaultTree(HSC.FT)    // TODO this one seems to take a long time!
        runTreeLikeFaultTree(SubmarinePipelineStopperFailure.FT)
        runTreeLikeFaultTree(BHNGPipeline.FT)
        runTreeLikeFaultTree(BayesianNetwork.FT)
        runTreeLikeFaultTree(LeakageFailure.FT)
        runTreeLikeFaultTree(AssessingTheRisks1.FT)
        runTreeLikeFaultTree(PCBA.FT)

        println("DAG-like Fault Trees:")
        runDagLikeFaultTree(ChlorineRelease.FT)
        runDagLikeFaultTree(T0Chopper.FT)
        runDagLikeFaultTree(OGPF.FT)
    }

    def main(args: Array[String]): Unit = {
        //runTrees()
        given random: java.util.random.RandomGenerator = new java.util.Random()

        val treelikeFaultTrees: Seq[TreeLikeFaultTree] = Seq(
            AircraftRunwayExcursionAccidents,
            MainTrackTrainCollisionsLeadingToFatalitiesAndInjuries,
            ATCFailsToResolveTheConflict,
            LiquidStorageTank,
            LossContainerAtPort,
            //HSC, //TODO re-add this.   // TODO seems to take a very long time for Algorithm 4 (cut sets) (TODO possibly also the random BDT one..)
            SubmarinePipelineStopperFailure,
            BHNGPipeline,
            //BayesianNetwork, TODO re-add this.    // TODO don't use cut set algorithm for this one (because there are 2 billion cut sets) path set algorithm might be possible, but will still be slow
            LeakageFailure,
            AssessingTheRisks1,
            PCBA,
            //ChemicalCargoShortage,      // TODO recursive algorithm goes oom. should try and memory-optimise it and use a Trie-like structure instead of Map[Path, X]
        )

        val daglikeFaultTrees: Seq[DagLikeFaultTree] = Seq(
            ChlorineRelease,
            T0Chopper,
            OGPF,
        )

        val csvOuputTree = CSVOutput.createTreeLikeFile()
        CSVOutput.printTreeLikeHeader(csvOuputTree)

        for (treeLikeFT <- treelikeFaultTrees) {
            val treeFT = treeLikeFT.FT
            val (dagFT, probabilities) = Conversion.translateToDagTree(treeFT)
            val basicEvents = minimalcutpathset.getBasicEvents(dagFT)
            val (booleanFormula, _) = Conversion.translateToBooleanFormula(treeFT)

            // TODO flatten tree before running recursive algorithm.
            println("Calculating minimal cut sets...")
            val minimalCutSets = minimalcutpathset.minimalCutSets(dagFT)(basicEvents)
            println("Calculating minimal path sets...")
            val minimalPathSets = minimalcutpathset.minimalPathSets(dagFT)(basicEvents)

            // TODO probably want to remove this.
            println(s"DEBUG cut sets (${minimalCutSets.size}): $minimalCutSets")

            println(s"Calculate height of ${treeLikeFT.name} using Recursive algorithm 2...")
            val time_begin_recursive = System.nanoTime()
            val heightRecursive2 = faulttree.height7(treeFT)
            val time_end_recursive = System.nanoTime()
            println(s"Calculate height of ${treeLikeFT.name} using CutSet algorithm...")
            val time_begin_cutset = System.nanoTime()
            val heightCutSet = minimalcutpathset.algorithm4(minimalCutSets, probabilities)._2
            val time_end_cutset = System.nanoTime()
            println(s"Calculate height of ${treeLikeFT.name} using PathSet algorithm...")
            val time_begin_pathset = System.nanoTime()
            val heightPathSet = minimalcutpathset.algorithm5(minimalPathSets, probabilities)._2
            val time_end_pathset = System.nanoTime()
            println(s"Calculate height of ${treeLikeFT.name} using Random BDT algorithm...")
            val time_begin_randombdt = System.nanoTime()
            val heightRandomBDT = decisiontree.RandomBDTs.algorithm13(booleanFormula, probabilities)
            val time_end_randombdt = System.nanoTime()
            println("Finished calculating heights!")
            println()

            val time_recursive2_ns = time_end_recursive - time_begin_recursive
            val time_cutset_ns = time_end_cutset - time_begin_cutset
            val time_pathset_ns = time_end_pathset - time_begin_pathset
            val time_randombdt_ns = time_end_randombdt - time_begin_randombdt

            CSVOutput.printData(
                file = csvOuputTree,
                basicEvents = basicEvents.size,
                treeName = treeLikeFT.name,
                heightRecursive = heightRecursive2,
                timeRecursive = time_recursive2_ns,
                heightCutSet = heightCutSet,
                timeCutSet = time_cutset_ns,
                heightPathSet = heightPathSet,
                timePathSet = time_pathset_ns,
                heightRandomBDT = heightRandomBDT,
                timeRandomBDT = time_randombdt_ns
            )
        }

        val csvOutputDag = CSVOutput.createDagLikeFile()
        CSVOutput.printDagLikeHeader(csvOutputDag)

        for (dagLikeFT <- daglikeFaultTrees) {
            val dagFT = dagLikeFT.FT
            val basicEvents = minimalcutpathset.getBasicEvents(dagFT)
            val (booleanFormula, probabilities) = Conversion.translateToBooleanFormula(dagFT)

            // TODO flatten tree before running recursive algorithm.
            println("Calculating minimal cut sets...")
            val minimalCutSets = minimalcutpathset.minimalCutSets(dagFT)(basicEvents)
            println("Calculating minimal path sets...")
            val minimalPathSets = minimalcutpathset.minimalPathSets(dagFT)(basicEvents)

            println(s"Calculate height of ${dagLikeFT.name} using Recursive algorithm 3...")
            val time_begin_recursive = System.nanoTime()
            val heightRecursive3 = decisiontree.algorithm8(dagFT, probabilities)._2
            val time_end_recursive = System.nanoTime()
            println(s"Calculate height of ${dagLikeFT.name} using CutSet algorithm...")
            val time_begin_cutset = System.nanoTime()
            val heightCutSet = minimalcutpathset.algorithm4(minimalCutSets, probabilities)._2
            val time_end_cutset = System.nanoTime()
            println(s"Calculate height of ${dagLikeFT.name} using PathSet algorithm...")
            val time_begin_pathset = System.nanoTime()
            val heightPathSet = minimalcutpathset.algorithm5(minimalPathSets, probabilities)._2
            val time_end_pathset = System.nanoTime()
            println(s"Calculate height of ${dagLikeFT.name} using Random BDT algorithm...")
            val time_begin_randombdt = System.nanoTime()
            val heightRandomBDT = decisiontree.RandomBDTs.algorithm13(booleanFormula, probabilities)
            val time_end_randombdt = System.nanoTime()
            println("Finished calculating heights!")
            println()

            val time_recursive3_ns = time_end_recursive - time_begin_recursive
            val time_cutset_ns = time_end_cutset - time_begin_cutset
            val time_pathset_ns = time_end_pathset - time_begin_pathset
            val time_randombdt_ns = time_end_randombdt - time_begin_randombdt

            CSVOutput.printData(
                file = csvOutputDag,
                basicEvents = basicEvents.size,
                treeName = dagLikeFT.name,
                heightRecursive = heightRecursive3,
                timeRecursive = time_recursive3_ns,
                heightCutSet = heightCutSet,
                timeCutSet = time_cutset_ns,
                heightPathSet = heightPathSet,
                timePathSet = time_pathset_ns,
                heightRandomBDT = heightRandomBDT,
                timeRandomBDT = time_randombdt_ns
            )
        }
    }

}

object CSVOutput {
    import java.nio.file.{Files, Path, StandardOpenOption}

    private val outfileTree = "real-life-trees.csv"
    private val outfileDag = "real-life-dags.csv"

    def createTreeLikeFile(): Path = {
        Files.createFile(Path.of(outfileTree))
    }

    def createDagLikeFile(): Path = {
        Files.createFile(Path.of(outfileDag))
    }

    def printTreeLikeHeader(file: Path): Unit = {
        val line = "Fault Tree,# Basic events,Recursive algorithm 2 height,time (ns),Cut set algorithm height,time (ns),Path set algorithm height,time (ns),Random binary decision tree algorithm height,time (ns)\r\n"
        writeString(file, line)
    }

    def printDagLikeHeader(file: Path): Unit = {
        val line = "Fault Tree,# Basic events,Recursive algorithm 3 height,time (ns),Cut set algorithm height,time (ns),Path set algorithm height,time (ns),Random binary decision tree algorithm height,time (ns)\r\n"
        writeString(file, line)
    }

    def printData(file: Path, basicEvents: Int, treeName: String, heightRecursive: Double, timeRecursive: Long, heightCutSet: Double, timeCutSet: Long, heightPathSet: Double, timePathSet: Long, heightRandomBDT: Double, timeRandomBDT: Long): Unit = {
        val line = s""""${treeName}","${basicEvents}","${heightRecursive}","${timeRecursive}","${heightCutSet}","${timeCutSet}","${heightPathSet}","${timePathSet}","${heightRandomBDT}","${timeRandomBDT}"\r\n"""
        writeString(file, line)
    }

    private def writeString(file: Path, string: String): Unit = {
        Files.writeString(file, string, StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.APPEND)
    }
}

object AircraftRunwayExcursionAccidents extends TreeLikeFaultTree {
    import faulttree.FaultTree
    import faulttree.FaultTree.*

    val X1 = 1
    val X2 = 2
    val X3 = 3
    val X4 = 4
    val X5 = 5
    val X6 = 6
    val X7 = 7
    val X8 = 8
    val X9 = 9
    val X10 = 10
    val X11 = 11
    val X12 = 12
    val X13 = 13
    val X14 = 14
    val X15 = 15
    val X16 = 16
    val X17 = 17
    val X18 = 18
    val X19 = 19
    val X20 = 20
    val X21 = 21

    val T = 0

    final val M1 = 22
    final val M2 = 23
    final val M3 = 24
    final val M4 = 25
    final val M5 = 26
    final val M6 = 27
    final val M7 = 28
    final val M8 = 29
    final val M9 = 30
    final val M10 = 31
    final val M11 = 32
    final val M12 = 33

    final val p1 = 6E-4
    final val p2 = 2E-5
    final val p3 = 1E-4
    final val p4 = 1E-4
    final val p5 = 3E-4
    final val p6 = 2E-4
    final val p7 = 2E-4
    final val p8 = 2E-4
    final val p9 = 1E-4
    final val p10 = 2E-4
    final val p11 = 3E-5
    final val p12 = 5E-5
    final val p13 = 1E-4
    final val p14 = 5E-5
    final val p15 = 2E-5
    final val p16 = 2.5E-4
    final val p17 = 2.5E-4
    final val p18 = 3E-4
    final val p19 = 3E-5
    final val p20 = 3E-5
    final val p21 = 3E-5

    val FT: FaultTree = AndEvent(T, Seq(
        OrEvent(M1, Seq(
            OrEvent(M3, Seq(
                BasicEvent(X1, p1),
                BasicEvent(X2, p2)
            )),
            OrEvent(M4, Seq(
                OrEvent(M7, Seq(
                    BasicEvent(X7, p7),
                    BasicEvent(X8, p8),
                    BasicEvent(X9, p9)
                )),
                BasicEvent(X3, p3)
            ))
        )),
        OrEvent(M2, Seq(
            OrEvent(M5, Seq(
                BasicEvent(X4, p4),
                BasicEvent(X5, p5),
                BasicEvent(X6, p6)
            )),
            OrEvent(M6, Seq(
                OrEvent(M8, Seq(
                    OrEvent(M10, Seq(
                        AndEvent(M11, Seq(
                            BasicEvent(X16, p16),
                            BasicEvent(X17, p17),
                            BasicEvent(X18, p18)
                        )),
                        BasicEvent(X14, p14),
                        BasicEvent(X15, p15),
                        AndEvent(M12, Seq(
                            BasicEvent(X19, p19),
                            BasicEvent(X20, p20),
                            BasicEvent(X21, p21)
                        ))
                    )),
                    BasicEvent(X10, p10),
                    BasicEvent(X11, p11)
                )),
                OrEvent(M9, Seq(
                    BasicEvent(X12, p12),
                    BasicEvent(X13, p13)
                ))
            ))
        ))
    ))

}

object ChlorineRelease extends DagLikeFaultTree {
    import minimalcutpathset.FaultTree
    import minimalcutpathset.FaultTree.*
    import minimalcutpathset.TreeNode
    import minimalcutpathset.TreeNode.*
    import minimalcutpathset.Gate
    import minimalcutpathset.Gate.*

    final val B1 = 1
    final val B2 = 2
    final val B3 = 3
    final val B4 = 4
    final val B5 = 5
    final val B6 = 6
    final val B7 = 7
    final val B8 = 8
    final val B9 = 9
    final val B10 = 10
    final val B11 = 11
    final val B12 = 12
    final val B13 = 13
    final val B14 = 14
    final val B15 = 15
    final val B16 = 16
    final val B17 = 17
    final val B18 = 18
    final val B19 = 19
    final val B20 = 20
    final val B21 = 21
    final val B22 = 22
    final val B23 = 23
    final val B24 = 24
    final val B25 = 25
    final val B26 = 26
    final val B27 = 27

    final val ChlorineRelease = 0

    final val LeakBetweenTanksAndAA = 28
    final val PipeRupture = 29
    final val FlangeLeakInValveV2 = 30
    final val FlangeLeakInValveV21 = 31
    final val LeakBetweenAAAndBB = 32
    final val LeaksAfterBBDueToPipeRupture = 33
    final val ReleaseOfChlorineFromInlet = 34
    final val ReleaseOfChlorineFromOutlet = 35
    final val ReleaseOfChlorineFromVessel = 36
    final val ReleaseOfChlorineFromFillingPoint = 37
    final val LeakRuptureDueToOverPressure = 38
    final val SafetyReliefSystem = 39
    final val FailureOfReliefSystem = 40
    final val FailureOf2ndReliefSystem = 41
    final val OverFilling = 42
    final val InletValveFailsToHold = 43
    final val InletValveNotClosedAfterFilling = 44
    final val HumanError = 45
    final val OverTemperature = 46;
    final val FlangeLeakInValveV3 = 47
    final val FlangeLeakInValveV31 = 48

    final val LeakRuptureDueToOverPressureOr = 49 // or gate without name

    final val p1 = exp(2.790, -5)
    final val p2 = exp(2.864, -5)
    final val p3 = exp(4.093, -5)
    final val p4 = exp(2.357, -3)
    final val p5 = exp(2.864, -5)
    final val p6 = exp(2.559, -5)
    final val p7 = exp(8.763, -5)
    final val p8 = exp(2.841, -5)
    final val p9 = exp(9.527, -5)
    final val p10 = exp(1.266, -5)
    final val p11 = exp(4.049, -7)
    final val p12 = exp(3.033, -7)
    final val p13 = exp(4.856, -7)
    final val p14 = exp(3.242, -3)
    final val p15 = exp(3.902, -3)
    final val p16 = exp(3.149, -3)
    final val p17 = exp(2.551, -2)
    final val p18 = exp(4.178, -3)
    final val p19 = exp(2.759, -3)
    final val p20 = exp(2.751, -3)
    final val p21 = exp(1.266, -5)
    final val p22 = exp(2.145, -3)
    final val p23 = exp(2.759, -3)
    final val p24 = exp(3.902, -3)
    final val p25 = exp(5.121, -4)
    final val p26 = exp(1.081, -3)
    final val p27 = exp(7.659, -4)

    val FT: FaultTree = FaultTree(ChlorineRelease, Map(
        B1 -> BasicEvent(B1, p1),
        B2 -> BasicEvent(B2, p2),
        B3 -> BasicEvent(B3, p3),
        B4 -> BasicEvent(B4, p4),
        B5 -> BasicEvent(B5, p5),
        B6 -> BasicEvent(B6, p6),
        B7 -> BasicEvent(B7, p7),
        B8 -> BasicEvent(B8, p8),
        B9 -> BasicEvent(B9, p9),
        B10 -> BasicEvent(B10, p10),
        B11 -> BasicEvent(B11, p11),
        B12 -> BasicEvent(B12, p12),
        B13 -> BasicEvent(B13, p13),
        B14 -> BasicEvent(B14, p14),
        B15 -> BasicEvent(B15, p15),
        B16 -> BasicEvent(B16, p16),
        B17 -> BasicEvent(B17, p17),
        B18 -> BasicEvent(B18, p18),
        B19 -> BasicEvent(B19, p19),
        B20 -> BasicEvent(B20, p20),
        B21 -> BasicEvent(B21, p21),
        B22 -> BasicEvent(B22, p22),
        B23 -> BasicEvent(B23, p23),
        B24 -> BasicEvent(B24, p24),
        B25 -> BasicEvent(B25, p25),
        B26 -> BasicEvent(B26, p26),
        B27 -> BasicEvent(B27, p27),

        ChlorineRelease -> Combination(ChlorineRelease, Or, Set(ReleaseOfChlorineFromInlet, ReleaseOfChlorineFromOutlet, ReleaseOfChlorineFromVessel, ReleaseOfChlorineFromFillingPoint)),

        ReleaseOfChlorineFromInlet -> Combination(ReleaseOfChlorineFromInlet, Or, Set(LeakBetweenTanksAndAA, LeakBetweenAAAndBB, LeaksAfterBBDueToPipeRupture)),
        LeakBetweenTanksAndAA -> Combination(LeakBetweenTanksAndAA, Or, Set(PipeRupture, FlangeLeakInValveV2)),
        PipeRupture -> Combination(PipeRupture, Or, Set(B1, B2)),
        FlangeLeakInValveV2 -> Combination(FlangeLeakInValveV2, Or, Set(B4, B5)),
        LeakBetweenAAAndBB -> Combination(LeakBetweenAAAndBB, Or, Set(FlangeLeakInValveV21, PipeRupture)),
        FlangeLeakInValveV21 -> Combination(FlangeLeakInValveV21, Or, Set(B4, B5)),
        LeaksAfterBBDueToPipeRupture -> Combination(LeaksAfterBBDueToPipeRupture, Or, Set(B1, B2)),

        ReleaseOfChlorineFromOutlet -> Combination(ReleaseOfChlorineFromOutlet, Or, Set(LeakBetweenTanksAndAA, LeakBetweenAAAndBB, LeaksAfterBBDueToPipeRupture)),
        LeakBetweenTanksAndAA -> Combination(LeakBetweenTanksAndAA, Or, Set(PipeRupture, FlangeLeakInValveV3)),
        LeakBetweenAAAndBB -> Combination(LeakBetweenAAAndBB, Or, Set(FlangeLeakInValveV31, PipeRupture)),
        FlangeLeakInValveV3 -> Combination(FlangeLeakInValveV3, Or, Set(B4, B5)),
        FlangeLeakInValveV31 -> Combination(FlangeLeakInValveV31, Or, Set(B4, B5)),

        ReleaseOfChlorineFromFillingPoint -> Combination(ReleaseOfChlorineFromFillingPoint, Or, Set(B21, B22, B23, B20, B24)),

        ReleaseOfChlorineFromVessel -> Combination(ReleaseOfChlorineFromVessel, Or, Set(B10, LeakRuptureDueToOverPressure, B6, B25)),
        LeakRuptureDueToOverPressure -> Combination(LeakRuptureDueToOverPressure, And, Set(SafetyReliefSystem, LeakRuptureDueToOverPressureOr)),
        SafetyReliefSystem -> Combination(SafetyReliefSystem, And, Set(FailureOfReliefSystem, FailureOf2ndReliefSystem)),
        FailureOfReliefSystem -> Combination(FailureOfReliefSystem, Or, Set(B7, B8, B3)),
        FailureOf2ndReliefSystem -> Combination(FailureOf2ndReliefSystem, Or, Set(B7, B8, B3)),
        LeakRuptureDueToOverPressureOr -> Combination(LeakRuptureDueToOverPressureOr, Or, Set(OverFilling, OverTemperature)),
        OverTemperature -> Combination(OverTemperature, Or, Set(B11, B13, B12)),
        OverFilling -> Combination(OverFilling, Or, Set(InletValveFailsToHold, InletValveNotClosedAfterFilling)),
        InletValveFailsToHold -> Combination(InletValveFailsToHold, And, Set(B9, B9)),
        InletValveNotClosedAfterFilling -> Combination(InletValveNotClosedAfterFilling, Or, Set(B26, B15, B27, B14, HumanError)),
        HumanError -> Combination(HumanError, Or, Set(B16, B17, B18, B19)),
    ))

    def exp(scalar: Double, exponent: Double) =
        scalar * Math.pow(10, exponent)
}

object MainTrackTrainCollisionsLeadingToFatalitiesAndInjuries extends TreeLikeFaultTree {

    import faulttree.FaultTree
    import faulttree.FaultTree.*

    val X1 = 1
    val X2 = 2
    val X3 = 3
    val X4 = 4
    val X5 = 5
    val X6 = 6
    val X7 = 7
    val X8 = 8
    val X9 = 9

    val T = 0

    final val M1 = 10

    final val p1 = 3.7E-4
    final val p2 = 1.1E-3
    final val p3 = 7.4E-4
    final val p4 = 3.7E-4
    final val p5 = 1.5E-3
    final val p6 = 7.4E-4
    final val p7 = 3E-3
    final val p8 = 1.1E-3
    final val p9 = 3.7E-4

    val FT: FaultTree = OrEvent(T, Seq(
        BasicEvent(X1,p1),
        BasicEvent(X9,p9),
        OrEvent(M1, Seq(
            BasicEvent(X2,p2),
            BasicEvent(X3,p3),
            BasicEvent(X4,p4),
            BasicEvent(X5,p5),
            BasicEvent(X6,p6),
            BasicEvent(X7,p7)
        ))
    ))
}

object ATCFailsToResolveTheConflict extends TreeLikeFaultTree {
    import faulttree.FaultTree
    import faulttree.FaultTree.*

    val X1 = 1
    val X2 = 2
    val X3 = 3
    val X4 = 4
    val X5 = 5
    val X6 = 6
    val X7 = 7
    val X8 = 8
    val X9 = 9
    val X10 = 10
    val X11 = 11
    val X12 = 12
    val X13= 13
    val X14 = 14
    val X15 = 15
    val X16 = 16
    val X17 = 17
    val X18 = 18

    val T = 0

    final val M1 = 19
    final val M2 = 20
    final val M3 = 21
    final val M4 = 22
    final val M5 = 23
    final val M6 = 24
    final val M7 = 25

    final val p1 = 4.31E-3
    final val p2 =  1.29e-1
    final val p3 =  1.29e-1
    final val p4 =  1.29e-1
    final val p5 = 2.70e-4
    final val p6 = 2.70e-4
    final val p7 = 2.70e-4
    final val p8 = 2.70e-4
    final val p9 = 2.70e-4
    final val p10 = 2.70e-4
    final val p11=  2.70e-4
    final val p12 = 2.70e-4
    final val p13 = 3.79e-2
    final val p14 = 3.79e-2
    final val p15=  3.79e-2
    final val p16 = 3.79e-2
    final val p17 =  7.20e-4
    final val p18 =  7.20e-4

    val FT: FaultTree = OrEvent(T, Seq(
        BasicEvent(X1,p1),
        OrEvent(M1, Seq(
            AndEvent(M2, Seq(
                BasicEvent(X2,p2),
                BasicEvent(X3,p3),
                BasicEvent(X4,p4)
            )),
            OrEvent(M3, Seq(
                BasicEvent(X5,p5),
                BasicEvent(X6,p6),
                BasicEvent(X7,p7),
                BasicEvent(X8,p8),
                BasicEvent(X9,p9),
                BasicEvent(X10,p10),
                BasicEvent(X11,p11),
                BasicEvent(X12,p12)
            ))
        )),
        OrEvent(M4,Seq(
            AndEvent(M5,Seq(
                BasicEvent(X13,p13),
                BasicEvent(X14,p14)
            )),
            AndEvent(M6,Seq(
                BasicEvent(X15,p15),
                BasicEvent(X16,p16)
            )),
            OrEvent(M7,Seq(
                BasicEvent(X17,p17),
                BasicEvent(X18,p18)
            ))
        ))
    ))

}

object T0Chopper extends DagLikeFaultTree {
    import minimalcutpathset.FaultTree
    import minimalcutpathset.FaultTree.*
    import minimalcutpathset.TreeNode
    import minimalcutpathset.TreeNode.*
    import minimalcutpathset.Gate
    import minimalcutpathset.Gate.*

    final val X1 = 1
    final val X2 = 2
    final val X3 = 3
    final val X4 = 4
    final val X5 = 5
    final val X6 = 6
    final val X7 = 7
    final val X8 = 8
    final val X9 = 9
    final val X10 = 10
    final val X11 = 11
    final val X12 = 12
    final val X13 = 13
    final val X14 = 14
    final val X15 = 15
    final val X16 = 16

    final val p1 = 0.000499
    final val p2 = 0.000361
    final val p3 = 0.001789
    final val p4 = 0.000120
    final val p5 = 0.001693
    final val p6 = 0.001290
    final val p7 = 0.000680
    final val p8 = 0.001033
    final val p9 = 0.000233
    final val p10 = 0.000139
    final val p11 = 0.001380
    final val p12 = 0.000467
    final val p13 = 0.000501
    final val p14 = 0.000501
    final val p15 = 0.000534
    final val p16 = 0.000347

    final val T = 0

    final val M1 = 17
    final val M2 = 18
    final val M3 = 19
    final val M4 = 20
    final val M5 = 21
    final val M6 = 22
    final val M7 = 23

    val FT: FaultTree = FaultTree(T, Map(
        X1 -> BasicEvent(X1, p1),
        X2 -> BasicEvent(X2, p2),
        X3 -> BasicEvent(X3, p3),
        X4 -> BasicEvent(X4, p4),
        X5 -> BasicEvent(X5, p5),
        X6 -> BasicEvent(X6, p6),
        X7 -> BasicEvent(X7, p7),
        X8 -> BasicEvent(X8, p8),
        X9 -> BasicEvent(X9, p9),
        X10 -> BasicEvent(X10, p10),
        X11 -> BasicEvent(X11, p11),
        X12 -> BasicEvent(X12, p12),
        X13 -> BasicEvent(X13, p13),
        X14 -> BasicEvent(X14, p14),
        X15 -> BasicEvent(X15, p15),
        X16 -> BasicEvent(X16, p16),

        M1 -> Combination(M1, Or, Set(X1, X2, X3, X4)),
        M2 -> Combination(M2, Or, Set(X5, X6)),
        M3 -> Combination(M3, Or, Set(X7, X8, X6)),
        M4 -> Combination(M4, Or, Set(X9, X10)),
        M5 -> Combination(M5, Or, Set(X11, X12)),
        M6 -> Combination(M6, Or, Set(X13, X14, X6)),
        M7 -> Combination(M7, Or, Set(X15, X16)),

        T -> Combination(T, Or, Set(M1, M2, M3, M4, M5, M6, M7))
    ))
}

@java.lang.Deprecated
// Probabilities listed here might not actually be failure probabilities.
// The values in the DFT benchmark set and the corresponding paper don't seem to be the same.
object LiquidStorageTank extends TreeLikeFaultTree {
    import faulttree.FaultTree
    import faulttree.FaultTree.*

    val X1 = 1
    val X2 = 2
    val X3 = 3
    val X4 = 4
    val X5 = 5
    val X6 = 6
    val X7 = 7
    val X8 = 8
    val X9 = 9
    val X10 = 10
    val X11 = 11
    val X12 = 12
    val X13= 13
    val X14 = 14
    val X15 = 15
    val X16 = 16
    val X17 = 17
    val X18 = 18
    val X19 = 19
    val X20 = 20
    val X21 = 21
    val X22 = 22

    val T = 0

    final val M1 = 23
    final val M2 = 24
    final val M3 = 25
    final val M4 = 26
    final val M5 = 27
    final val M6 = 28
    final val M7 = 29
    final val M8 = 30
    final val M9 = 31
    final val M10 = 32
    final val M11 = 33
    final val M12 = 34

    final val p1 = 0.0800
    final val p2 =  0.0020
    final val p3 =  0.0762
    final val p4 =  0.0403
    final val p5 = 0.0403
    final val p6 = 0.1610
    final val p7 = 0.1
    final val p8 = 0.0403
    final val p9 = 0.1250
    final val p10 = 0.1
    final val p11=  0.0360
    final val p12 = 0.0260
    final val p13 = 0.0550
    final val p14 = 0.0250
    final val p15 = 0.0403
    final val p16 = 0.1260
    final val p17 = 0.0374
    final val p18 = 0.0374
    final val p19 = 0.0004
    final val p20 = 0.0004
    final val p21 = 0.0003
    final val p22 = 0.0040

    val FT: FaultTree = OrEvent(T, Seq(
        OrEvent(M1, Seq(
            BasicEvent(X1,p1),
            BasicEvent(X2,p2),
            BasicEvent(X3,p3)
        )),
        OrEvent(M2, Seq(
            OrEvent(M3, Seq(
                BasicEvent(X4,p4),
                BasicEvent(X5,p5)
            )),
            OrEvent(M4, Seq(
                BasicEvent(X6,p6),
                BasicEvent(X7,p7)
            )),
            BasicEvent(X8,p8)
        )),

        OrEvent(M5,Seq(
            BasicEvent(X9,p9),
            BasicEvent(X10,p10),
            BasicEvent(X11,p11)
        )),

        OrEvent(M6,Seq(
            OrEvent(M7,Seq(
                BasicEvent(X12,p12),
                BasicEvent(X13,p13),
                BasicEvent(X14,p14)
            )),
            OrEvent(M8,Seq(
                BasicEvent(X15,p15),
                BasicEvent(X16,p16)
            ))
        )),

        OrEvent(M9,Seq(
            OrEvent(M10,Seq(
                BasicEvent(X17,p17),
                BasicEvent(X18,p18)
            )),
            OrEvent(M11,Seq(
                BasicEvent(X19,p19),
                BasicEvent(X20,p20)
            )),
            OrEvent(M12,Seq(
                BasicEvent(X21,p21),
                BasicEvent(X22,p22)
            ))
        ))
    ))

}

object LossContainerAtPort extends TreeLikeFaultTree {

    import faulttree.FaultTree
    import faulttree.FaultTree.*

    val X1 = 1
    val X2 = 2
    val X3 = 3
    val X4 = 4
    val X5 = 5
    val X6 = 6
    val X7 = 7
    val X8 = 8
    val X9 = 9
    val X10 = 10
    val X11 = 11
    val X12 = 12
    val X13= 13
    val X14 = 14
    val X15 = 15
    val X16 = 16
    val X17 = 17
    val X18 = 18
    val X19 = 19
    val X20 = 20
    val X21 = 21
    val X22 = 22
    val X23 = 23
    val X24= 24
    val X25 = 25
    val X26 = 26
    val X27 = 27
    val X28 = 28
    val X29 = 29
    val X30 = 30

    val T = 0

    final val M1 = 31
    final val M2 = 32
    final val M3 = 33
    final val M4 = 34
    final val M5 = 35
    final val M6 = 36
    final val M7 = 37
    final val M8 = 38
    final val M9 = 39
    final val M10 = 40
    final val M11 = 41
    final val M12 = 42
    final val M13 = 43


    final val p1 = 4.48e-4
    final val p2 =  3.86e-4
    final val p3 =  1.85e-3
    final val p4 =  8.61e-5
    final val p5 = 1.65e-4
    final val p6 = 1.38e-1
    final val p7 = 1.2e-1
    final val p8 = 6.72e-2
    final val p9 = 2.09e-3
    final val p10 = 3.38e-4
    final val p11=  3.7e-4
    final val p12 = 9.12e-6
    final val p13 = 2.92e-3
    final val p14 = 3.87e-3
    final val p15=  1.71e-5
    final val p16 = 3.97e-4
    final val p17 =  1.09e-1
    final val p18 =  3.91e-3
    final val p19=  2.27e-4
    final val p20 = 3.95e-2
    final val p21 =  1.14e-1
    final val p22 =  2.58e-3
    final val p23 = 1.13e-1
    final val p24 =  3.83e-3
    final val p25 =  3.52e-3
    final val p26 =  3.76e-2
    final val p27 = 4.22e-4
    final val p28 = 1.21e-3
    final val p29 = 1.94e-3
    final val p30 = 1.19e-3

    val FT: FaultTree = OrEvent(T, Seq(
        AndEvent(M1, Seq(
            OrEvent(M2, Seq(
                BasicEvent(X1,p1),
                BasicEvent(X2,p2),
                BasicEvent(X3,p3)
            )),
            OrEvent(M3, Seq(
                BasicEvent(X4,p4),
                BasicEvent(X5,p5)
            ))

        )),

        OrEvent(M4, Seq(
            OrEvent(M5, Seq(
                BasicEvent(X6,p6),
                BasicEvent(X7,p7),
                BasicEvent(X8,p8),
                BasicEvent(X9,p9)
            )),
            OrEvent(M6, Seq(
                BasicEvent(X10,p10),
                BasicEvent(X11,p11),
                BasicEvent(X12,p12)
            ))
        )),

        OrEvent(M7,Seq(

            OrEvent(M8, Seq(
                BasicEvent(X13,p13),
                BasicEvent(X14,p14),
                BasicEvent(X15,p15),
                BasicEvent(X16,p16)
            )),

            OrEvent(M9,Seq(
                OrEvent(M13,Seq(
                    BasicEvent(X26,p26),
                    BasicEvent(X27,p27),
                    BasicEvent(X28,p28),
                    BasicEvent(X29,p29),
                    BasicEvent(X30,p30)
                )),
                BasicEvent(X17,p11)
            )),
            OrEvent(M10,Seq(
                BasicEvent(X18,p18),
                BasicEvent(X19,p19)

            )),
            OrEvent(M11, Seq(
                BasicEvent(X20,p20),
                BasicEvent(X21,p18),
                BasicEvent(X22,p22),
                BasicEvent(X23,p23)
            )),
            OrEvent(M12, Seq(
                BasicEvent(X24,p24),
                BasicEvent(X25,p25)
            ))
        ))

    ))

}

object HSC extends TreeLikeFaultTree {

    import faulttree.FaultTree
    import faulttree.FaultTree.*

    val X1 = 1
    val X2 = 2
    val X3 = 3
    val X4 = 4
    val X5 = 5
    val X6 = 6
    val X7 = 7
    val X8 = 8
    val X9 = 9
    val X10 = 10
    val X11 = 11
    val X12 = 12
    val X13 = 13
    val X14 = 14
    val X15 = 15
    val X16 = 16
    val X17 = 17
    val X18 = 18
    val X19 = 19
    val X20 = 20
    val X21 = 21
    val X22 = 22
    val X23 = 23
    val X24 = 24
    val X25 = 25
    val X26 = 26
    val X27 = 27
    val X28 = 28
    val X29 = 29
    val X30 = 30
    val X31 = 31
    val X32 = 32
    val X33 = 33
    val X34 = 34
    val X35 = 35
    val X36 = 36
    val X37 = 37
    val X38 = 38
    val X39 = 39
    val X40 = 40
    val X41 = 41
    val X42 = 42
    val X43 = 43
    val X44 = 44
    val X45 = 45
    val X46 = 46
    val X47 = 47
    val X48 = 48
    val X49 = 49
    val X50 = 50
    val X51 = 51
    val X52 = 52
    val X53 = 53

    val T = 0

    final val M1 = 54
    final val M2 = 55
    final val M3 = 56
    final val M4 = 57
    final val M5 = 58
    final val M6 = 59
    final val M7 = 60
    final val M8 = 61
    final val M9 = 62
    final val M10 = 63
    final val M11 = 64
    final val M12 = 65
    final val M13 = 66


    final val p1 = 5.61e-6
    final val p2 = 3.42e-7
    final val p3 = 1.38e-6
    final val p4 = 2.89e-6
    final val p5 = 5.61e-6
    final val p6 = 2.4e-6
    final val p7 = 2.54e-6
    final val p8 = 2.54e-6
    final val p9 = 2.54e-6
    final val p10 = 2.54e-6
    final val p11 = 2.54e-6
    final val p12 = 2.54e-6
    final val p13 = 2.54e-6
    final val p14 = 2.54e-6
    final val p15 = 2.54e-6
    final val p16 = 2.54e-6
    final val p17 = 2.54e-6
    final val p18 = 2.54e-6
    final val p19 = 2.54e-6
    final val p20 = 2.54e-6
    final val p21 = 2.54e-6
    final val p22 = 2.54e-6
    final val p23 = 2.54e-6
    final val p24 = 2.54e-6
    final val p25 = 2.54e-6
    final val p26 = 2.54e-6
    final val p27 = 2.54e-6
    final val p28 = 5.3e-6
    final val p29 = 5.3e-6
    final val p30 = 3.42e-7
    final val p31 = 3.42e-7
    final val p32 = 3.42e-7
    final val p33 = 3.42e-7
    final val p34 = 3.42e-7
    final val p35 = 3.42e-7
    final val p36 = 3.42e-7
    final val p37 = 3.42e-7
    final val p38 = 3.42e-7
    final val p39 = 3.42e-7
    final val p40 = 2.1e-6
    final val p41 =  1.42e-4
    final val p42 = 8.35e-7
    final val p43 = 2.4e-3
    final val p44 = 8.27e-3
    final val p45 = 5.12e-3
    final val p46 =  5.44e-5
    final val p47 = 1.38e-6
    final val p48 = 4.5e-7
    final val p49 = 4.5e-7
    final val p50 =  4.2e-6
    final val p51 = 4.2e-6
    final val p52 = 5e-6
    final val p53 = 7.34e-5

    val FT: FaultTree = AndEvent(T, Seq(
        AndEvent(M1, Seq(
            AndEvent(M6, Seq(
                BasicEvent(X1,p1),
                BasicEvent(X2,p2)
            )),
            AndEvent(M7, Seq(
                BasicEvent(X3,p3),
                BasicEvent(X4,p4)
            )),
            AndEvent(M8, Seq(
                BasicEvent(X5,p5),
                BasicEvent(X6,p6)
            ))

        )),
        AndEvent(M2, Seq(
            OrEvent(M9, Seq(
                BasicEvent(X7,p7),
                BasicEvent(X8,p8),
                BasicEvent(X9,p9),
                BasicEvent(X10,p10),
                BasicEvent(X11,p11),
                BasicEvent(X12,p12),
                BasicEvent(X13,p13),
                BasicEvent(X14,p14),
                BasicEvent(X15,p15),
                BasicEvent(X16,p16),
                BasicEvent(X17,p17),
                BasicEvent(X18,p18),
                BasicEvent(X19,p19),
                BasicEvent(X20,p20),
                BasicEvent(X21,p21),
                BasicEvent(X22,p22),
                BasicEvent(X23,p23),
                BasicEvent(X24,p24),
                BasicEvent(X25,p25),
                BasicEvent(X26,p26),
                BasicEvent(X27,p27)
            )),
            OrEvent(M10, Seq(
                BasicEvent(X30,p30),
                BasicEvent(X31,p31),
                BasicEvent(X32,p32),
                BasicEvent(X33,p33),
                BasicEvent(X34,p34),
                BasicEvent(X35,p35),
                BasicEvent(X36,p36),
                BasicEvent(X37,p37),
                BasicEvent(X38,p38),
                BasicEvent(X39,p39)
            )),
            BasicEvent(X28,p28),
            BasicEvent(X29,p29)
        )),

        AndEvent(M3, Seq(
            BasicEvent(X40,p40),
            BasicEvent(X41,p41),
            BasicEvent(X42,p42),
            BasicEvent(X43,p43)
        )),

        AndEvent(M4, Seq(
            BasicEvent(X44,p44),
            BasicEvent(X45,p45),
            BasicEvent(X46,p46),
            BasicEvent(X47,p47)
        )),

        AndEvent(M5, Seq(
            AndEvent(M11, Seq(
                BasicEvent(X48,p48),
                BasicEvent(X49,p49)
            )),
            AndEvent(M12, Seq(
                BasicEvent(X50,p50),
                BasicEvent(X51,p51)
            )),
            AndEvent(M13, Seq(
                BasicEvent(X52,p52),
                BasicEvent(X53,p53)
            )),
        ))
    ))

}

object SubmarinePipelineStopperFailure extends TreeLikeFaultTree {

    import faulttree.FaultTree
    import faulttree.FaultTree.*

    val X1 = 1
    val X2 = 2
    val X3 = 3
    val X4 = 4
    val X5 = 5
    val X6 = 6
    val X7 = 7
    val X8 = 8
    val X9 = 9
    val X10 = 10
    val X11 = 11
    val X12 = 12
    val X13 = 13
    val X14 = 14
    val X15 = 15
    val X16 = 16
    val X17 = 17
    val X18 = 18
    val X19 = 19
    val X20 = 20
    val X21 = 21
    val X22 = 22
    val X23 = 23
    val X24 = 24
    val X25 = 25
    val X26 = 26
    val X27 = 27
    val X28 = 28
    val X29 = 29
    val X30 = 30
    val X31 = 31

    val T = 0

    final val M1 = 32
    final val M2 = 33
    final val M3 = 34
    final val M4 = 35
    final val M5 = 36
    final val M6 = 37
    final val M7 = 38
    final val M8 = 39
    final val M9 = 40
    final val M10 = 41
    final val M11 = 42
    final val M12 = 43
    final val M13 = 44
    final val M14 = 45
    final val M15 = 46
    final val M16 = 47
    final val M17 = 48
    final val M18 = 49
    final val M19 = 50
    final val M20 = 51
    final val M21 = 52


    final val p1 = 0.41664
    final val p2 = 0.35836
    final val p3 = 0.40541
    final val p4 = 0.36886
    final val p5 = 0.38109
    final val p6 = 0.38
    final val p7 = 0.4135
    final val p8 = 0.37491
    final val p9 = 0.38653
    final val p10 = 0.3953
    final val p11= 0.38805
    final val p12 = 0.39623
    final val p13 = 0.39314
    final val p14 = 0.38944
    final val p15= 0.39414
    final val p16 = 0.34752
    final val p17 = 0.33197
    final val p18 = 0.37795
    final val p19= 0.38086
    final val p20 = 0.38755
    final val p21 = 0.39199
    final val p22 = 0.40955
    final val p23 = 0.36668
    final val p24 = 0.38914
    final val p25 = 0.39118
    final val p26 = 0.28445
    final val p27 = 0.36541
    final val p28 = 0.38814
    final val p29 = 0.40023
    final val p30 = 0.40023
    final val p31 = 0.36668

    val FT: FaultTree = OrEvent(T, Seq(
        OrEvent(M1, Seq(
            OrEvent(M5, Seq(
                OrEvent(M15, Seq(
                    BasicEvent(X1,p1),
                    BasicEvent(X2,p2),
                    BasicEvent(X3,p3)
                )),
                OrEvent(M16, Seq(
                    BasicEvent(X4,p4),
                    BasicEvent(X5,p5)
                ))
            )),
            OrEvent(M6, Seq(
                OrEvent(M17, Seq(
                    BasicEvent(X7,p7),
                    BasicEvent(X6,p6)
                )),
                BasicEvent(X8,p8)
            )),
        )),
        OrEvent(M2, Seq(
            OrEvent(M7, Seq(
                OrEvent(M18, Seq(
                    BasicEvent(X9,p9),
                    BasicEvent(X10,p10)
                )),
                OrEvent(M19, Seq(
                    BasicEvent(X11,p11),
                    BasicEvent(X12,p12)
                )),
                OrEvent(M20, Seq(
                    BasicEvent(X13,p13),
                    BasicEvent(X14,p14),
                    BasicEvent(X15,p15)
                ))
            )),
            OrEvent(M8, Seq(
                BasicEvent(X17,p17),
                BasicEvent(X16,p16)
            )),
            OrEvent(M9, Seq(
                BasicEvent(X18,p18),
                BasicEvent(X19,p19)
            )),
            OrEvent(M10, Seq(
                BasicEvent(X20,p20),
                BasicEvent(X21,p21)
            ))
        )),
        OrEvent(M3, Seq(
            OrEvent(M11, Seq(
                BasicEvent(X22,p22),
                BasicEvent(X23,p23)
            )),
            OrEvent(M12, Seq(
                BasicEvent(X24,p24),
                BasicEvent(X25,p25)
            ))
        )),
        OrEvent(M4, Seq(
            OrEvent(M13, Seq(
                BasicEvent(X26,p26),
                BasicEvent(X27,p27)
            )),
            OrEvent(M14, Seq(
                OrEvent(M21, Seq(
                    BasicEvent(X28,p28),
                    BasicEvent(X29,p29),
                    BasicEvent(X30,p30)
                )),
                BasicEvent(X31,p31)
            ))
        ))

    ))

}

object BHNGPipeline extends TreeLikeFaultTree {

    import faulttree.FaultTree
    import faulttree.FaultTree.*

    val X1 = 1
    val X2 = 2
    val X3 = 3
    val X4 = 4
    val X5 = 5
    val X6 = 6
    val X7 = 7
    val X8 = 8
    val X9 = 9
    val X10 = 10
    val X11 = 11
    val X12 = 12
    val X13 = 13
    val X14 = 14
    val X15 = 15
    val X16 = 16
    val X17 = 17
    val X18 = 18
    val X19 = 19
    val X20 = 20
    val X21 = 21
    val X22 = 22
    val X23 = 23
    val X24 = 24
    val X25 = 25
    val X26 = 26
    val X27 = 27
    val X28 = 28
    val X29 = 29
    val X30 = 30
    val X31 = 31
    val X32 = 32
    val X33 = 33
    val X34 = 34
    val X35 = 35
    val X36 = 36
    val X37 = 37
    val X38 = 38
    val X39 = 39
    val X40 = 40
    val X41 = 41
    val X42 = 42
    val X43 = 43
    val X44 = 44
    val X45 = 45

    val T = 0

    final val M0 = 64
    final val M1 = 46
    final val M2 = 47
    final val M3 = 48
    final val M4 = 49
    final val M5 = 50
    final val M6 = 51
    final val M7 = 52
    final val M8 = 53
    final val M9 = 54
    final val M10 = 55
    final val M11 = 56
    final val M12 = 57
    final val M13 = 58
    final val M14 = 59
    final val M15 = 60
    final val M16 = 61
    final val M17 = 62
    final val M18 = 63

    final val p1 = 0.02365
    final val p2 = 0.03979
    final val p3 = 0.07824
    final val p4 = 0.03573
    final val p5 = 0.02317
    final val p6 = 0.07778
    final val p7 = 0.07368
    final val p8 = 0.08013
    final val p9 = 0.03493
    final val p10 = 0.02893
    final val p11 = 0.03023
    final val p12 = 0.03099
    final val p13 = 0.04430
    final val p14 = 0.04241
    final val p15 = 0.04644
    final val p16 = 0.03584
    final val p17 = 0.03934
    final val p18 = 0.04440
    final val p19 = 0.04241
    final val p20 = 0.02915
    final val p21 = 0.04070
    final val p22 = 0.04567
    final val p23 = 0.04006
    final val p24 = 0.02483
    final val p25 = 0.06356
    final val p26 = 0.06745
    final val p27 = 0.02915
    final val p28 = 0.06011
    final val p29 = 0.07254
    final val p30 = 0.02317
    final val p31 = 0.07318
    final val p32 = 0.07757
    final val p33 = 0.06908
    final val p34 = 0.03438
    final val p35 = 0.07056
    final val p36 = 0.03714
    final val p37 = 0.02915
    final val p38 = 0.06111
    final val p39 = 0.03438
    final val p40 = 0.03099
    final val p41 = 0.04464
    final val p42 = 0.03584
    final val p43 = 0.03934
    final val p44 = 0.03143
    final val p45 = 0.03338

    val FT: FaultTree = OrEvent(T, Seq(
        OrEvent(M0, Seq(
            OrEvent(M1, Seq(
                OrEvent(M3, Seq(
                    OrEvent(M8, Seq(
                        BasicEvent(X1,p1),
                        BasicEvent(X2,p3)
                    )),
                    BasicEvent(X3,p3)
                )),
                OrEvent(M4, Seq(
                    BasicEvent(X4,p4),
                    BasicEvent(X5,p5)
                ))
            )),
            OrEvent(M2, Seq(
                OrEvent(M5, Seq(
                    OrEvent(M9, Seq(
                        BasicEvent(X6,p6),
                        BasicEvent(X7,p7)
                    )),
                    OrEvent(M10, Seq(
                        BasicEvent(X8,p8),
                        BasicEvent(X9,p9)
                    )),
                    OrEvent(M11, Seq(
                        BasicEvent(X10,p10),
                        BasicEvent(X11,p11)
                    ))
                )),
                OrEvent(M6, Seq(
                    BasicEvent(X12,p12),
                    OrEvent(M12, Seq(
                        BasicEvent(X13,p13),
                        BasicEvent(X14,p14),
                        BasicEvent(X15,p15),
                        BasicEvent(X16,p16),
                        BasicEvent(X17,p17),
                        BasicEvent(X18,p18),
                        BasicEvent(X19,p19),
                        BasicEvent(X20,p20),
                        BasicEvent(X21,p21),
                        BasicEvent(X22,p22),
                        BasicEvent(X23,p23),
                    )),
                    OrEvent(M13, Seq(
                        BasicEvent(X24,p24),
                        BasicEvent(X25,p25),
                        BasicEvent(X26,p26),
                        BasicEvent(X27,p27)
                    ))
                )),
                OrEvent(M7, Seq(
                    OrEvent(M17, Seq(
                        BasicEvent(X28,p28),
                        BasicEvent(X29,p29)
                    )),
                    BasicEvent(X30,p30)
                ))
            ))
        )),
        OrEvent(M15, Seq(
            BasicEvent(X31,p31),
            BasicEvent(X32,p32),
            BasicEvent(X33,p33),
            BasicEvent(X34,p34),
            BasicEvent(X35,p35)
        )),
        OrEvent(M16, Seq(
            BasicEvent(X36,p36),
            BasicEvent(X37,p37),
            BasicEvent(X38,p38),
            BasicEvent(X44,p44),
            BasicEvent(X45,p45),
            OrEvent(M17, Seq(
                BasicEvent(X39,p39),
                BasicEvent(X40,p40),
                BasicEvent(X41,p41),
                BasicEvent(X42,p42),
                BasicEvent(X43,p43)
            ))
        ))
    ))
}

object ChemicalCargoShortage extends TreeLikeFaultTree {

    import faulttree.FaultTree
    import faulttree.FaultTree.*

    val X1 = 1
    val X2 = 2
    val X3 = 3
    val X4 = 4
    val X5 = 5
    val X6 = 6
    val X7 = 7
    val X8 = 8
    val X9 = 9
    val X10 = 10
    val X11 = 11
    val X12 = 12
    val X13 = 13
    val X14 = 14
    val X15 = 15
    val X16 = 16
    val X17 = 17
    val X18 = 18
    val X19 = 19
    val X20 = 20
    val X21 = 21
    val X22 = 22
    val X23 = 23
    val X24 = 24
    val X25 = 25
    val X26 = 26
    val X27 = 27
    val X28 = 28
    val X29 = 29
    val X30 = 30
    val X31 = 31
    val X32 = 32
    val X33 = 33
    val X34 = 34
    val X35 = 35
    val X36 = 36
    val X37 = 37
    val X38 = 38
    val X39 = 39
    val X40 = 40
    val X41 = 41
    val X42 = 42
    val X43 = 43
    val X44 = 44
    val X45 = 45
    val X46 = 46
    val X47 = 47
    val X48 = 48
    val X49 = 49
    val X50 = 50
    val X51 = 51
    val X52 = 52
    val X53 = 53
    val X54 = 54
    val X55 = 55
    val X56 = 56
    val X57 = 57
    val X58 = 58
    val X59 = 59
    val X60 = 60
    val X61 = 61
    val X62 = 62
    val X63 = 63
    val X64 = 64
    val X65 = 65
    val X66 = 66
    val X67 = 67
    val X68 = 68
    val X69 = 69
    val X70 = 70
    val X71 = 71
    val X72 = 72
    val X73 = 73
    val X74 = 74
    val X75 = 75
    val X76 = 76
    val X77 = 77
    val X78 = 78
    val X79 = 79
    val X80 = 80
    val X81 = 81
    val X82 = 82
    val X83 = 83
    val X84 = 84
    val X85 = 85
    val X86 = 86
    val X87 = 87
    val X88 = 88
    val X89 = 89
    val X90 = 90
    val X91 = 91
    val X92 = 92
    val X93 = 93
    val X94 = 94
    val X95 = 95
    val X96 = 96
    val X97 = 97
    val X98 = 98
    val X99 = 99
    val X100 = 100
    val X101 = 101
    val X102 = 102
    val X103 = 103

    val T = 0

    final val M1 = 104
    final val M2 = 105
    final val M3 = 106
    final val M4 = 107
    final val M6 = 109
    final val M7 = 110
    final val M8 = 111
    final val M9 = 112
    final val M10 = 113
    final val M11 = 114
    final val M12 = 115
    final val M13 = 116
    final val M14 = 117
    final val M15 = 118
    final val M16 = 119
    final val M17 = 120
    final val M18 = 121
    final val M19 = 122
    final val M20 = 123
    final val M21 = 124
    final val M22 = 125
    final val M23 = 126
    final val M24 = 127
    final val M25 = 128
    final val M26 = 129
    final val M27 = 130
    final val M28 = 131
    final val M29 = 132
    final val M30 = 133
    final val M31 = 134
    final val M32 = 135
    final val M33 = 136
    final val M34 = 137
    final val M35 = 138
    final val M36 = 139
    final val M37 = 140
    final val M38 = 141
    final val M39 = 142
    final val M40 = 143
    final val M41 = 144
    final val M42 = 145
    final val M43 = 146
    final val M44 = 147
    final val M45 = 148
    final val M46 = 149
    final val M47 = 150
    final val M48 = 151
    final val M49 = 152
    final val M50 = 153
    final val M51 = 154
    final val M52 = 155
    final val M53 = 156
    final val M54 = 108
    final val M55 = 157
    final val M56 = 158
    final val M57 = 159
    final val M58 = 160
    final val M59 = 161
    final val M60 = 162
    final val M61 = 163
    final val M62 = 164
    final val M63 = 165

    final val p1 = 0.00069
    final val p2 = 0.00005
    final val p3 = 0.01599
    final val p4 = 0.00595
    final val p5 = 0.00252
    final val p6 = 0.00145
    final val p7 = 0.00011
    final val p8 = 0.00006
    final val p9 = 0.00005
    final val p10 = 0.00091
    final val p11 = 0.00310
    final val p12 = 0.00603
    final val p13 = 0.00147
    final val p14 = 0.00324
    final val p15 = 0.00001
    final val p16 = 0.00340
    final val p17 = 0.00300
    final val p18 = 0.00045
    final val p19 = 0.00338
    final val p20 = 0.00002
    final val p21 = 0.00777
    final val p22 = 0.00433
    final val p23 = 0.00992
    final val p24 = 0.00007
    final val p25 = 0.00776
    final val p26 = 0.00030
    final val p27 = 0.00010
    final val p28 = 0.00173
    final val p29 = 0.00072
    final val p30 = 0.00045
    final val p31 = 0.00462
    final val p32 = 0.00773
    final val p33 = 0.00286
    final val p34 = 0.00760
    final val p35 = 0.00002
    final val p36 = 0.02206
    final val p37 = 0.00064
    final val p38 = 0.01750
    final val p39 = 0.01854
    final val p40 = 0.00001
    final val p41 = 0.00152
    final val p42 = 0.00273
    final val p43 = 0.01250
    final val p44 = 0.01153
    final val p45 = 0.00891
    final val p46 = 0.00054
    final val p47 = 0.01959
    final val p48 = 0.00022
    final val p49 = 0.01756
    final val p50 = 0.02284
    final val p51 = 0.01736
    final val p52 = 0.02824
    final val p53 = 0.00636
    final val p54 = 0.00336
    final val p55 = 0.00040
    final val p56 = 0.00152
    final val p57 = 0.00676
    final val p58 = 0.01264
    final val p59 = 0.03504
    final val p60 = 0.01452
    final val p61 = 0.00259
    final val p62 = 0.00463
    final val p63 = 0.00139
    final val p64 = 0.00972
    final val p65 = 0.00420
    final val p66 = 0.02238
    final val p67 = 0.02250
    final val p68 = 0.01929
    final val p69 = 0.02321
    final val p70 = 0.00748
    final val p71 = 0.00276
    final val p72 = 0.00040
    final val p73 = 0.00143
    final val p74 = 0.00582
    final val p75 = 0.01455
    final val p76 = 0.03156
    final val p77 = 0.00048
    final val p78 = 0.00346
    final val p79 = 0.00139
    final val p80 = 0.00972
    final val p81 = 0.00493
    final val p82 = 0.01442
    final val p83 = 0.03670
    final val p84 = 0.00364
    final val p85 = 0.01402
    final val p86 = 0.00727
    final val p87 = 0.03504
    final val p88 = 0.03136
    final val p89 = 0.00723
    final val p90 = 0.02883
    final val p91 = 0.00550
    final val p92 = 0.01018
    final val p93 = 0.00231
    final val p94 = 0.01365
    final val p95 = 0.01444
    final val p96 = 0.00228
    final val p97 = 0.00742
    final val p98 = 0.00799
    final val p99 = 0.00382
    final val p100 = 0.02230
    final val p101 = 0.01467
    final val p102 = 0.00324
    final val p103 = 0.02939

    val FT: FaultTree = OrEvent(T, Seq(
        OrEvent(M1, Seq(
            BasicEvent(X39,p39),
            AndEvent(M35, Seq(
                OrEvent(M40, Seq(
                    BasicEvent(X1,p1),
                    BasicEvent(X2,p2)
                )),
                BasicEvent(X3,p3)
            )),
            AndEvent(M36, Seq(
                BasicEvent(X4,p4),
                BasicEvent(X5,p5)
            )),
            AndEvent(M37, Seq(
                OrEvent(M41, Seq(
                    OrEvent(M44, Seq(
                        AndEvent(M47, Seq(
                            AndEvent(M50, Seq(
                                OrEvent(M54, Seq(
                                    BasicEvent(X6,p6),
                                    BasicEvent(X7,p7),
                                    BasicEvent(X8,p8),
                                    BasicEvent(X9,p9),
                                    BasicEvent(X10,p10)
                                )),
                                OrEvent(M55, Seq(
                                    BasicEvent(X12,p12),
                                    BasicEvent(X13,p13),
                                    BasicEvent(X14,p14)
                                ))
                            )),
                            BasicEvent(X11,p11)
                        )),
                        BasicEvent(X15,p15)
                    )),
                    OrEvent(M45, Seq(
                        AndEvent(M48, Seq(
                            OrEvent(M51, Seq(
                                OrEvent(M56, Seq(
                                    BasicEvent(X17,p17),
                                    BasicEvent(X18,p18),
                                    BasicEvent(X19,p19)
                                )),
                                OrEvent(M57, Seq(
                                    BasicEvent(X21,p21),
                                    BasicEvent(X22,p22),
                                    BasicEvent(X23,p23),
                                    BasicEvent(X24,p24),
                                    BasicEvent(X25,p25)
                                ))
                            )),
                            BasicEvent(X20,p20)
                        )),
                        BasicEvent(X16,p16),
                        BasicEvent(X26,p26)
                    )),
                )),
                OrEvent(M42, Seq(
                    BasicEvent(X27,p27),
                    BasicEvent(X28,p28),
                    BasicEvent(X29,p29),
                    AndEvent(M46, Seq(
                        BasicEvent(X30,p30),
                        OrEvent(M49, Seq(
                            OrEvent(M52, Seq(
                                BasicEvent(X31,p31),
                                BasicEvent(X32,p32),
                                BasicEvent(X33,p33),
                                BasicEvent(X34,p34),
                                BasicEvent(X35,p35)
                            )),
                            OrEvent(M53, Seq(
                                BasicEvent(X36,p36),
                                BasicEvent(X37,p37),
                                BasicEvent(X38,p38)
                            ))
                        )),
                        BasicEvent(M30,p30)
                    ))
                ))
            )),
            OrEvent(M38, Seq(
                BasicEvent(X40,p40),
                BasicEvent(X41,p41)
            )),
            AndEvent(M39, Seq(
                OrEvent(M43, Seq(
                    BasicEvent(X43,p43),
                    BasicEvent(X44,p44)
                )),
                BasicEvent(X42,p42)
            ))
        )),
        OrEvent(M2, Seq(
            OrEvent(M4, Seq(
                AndEvent(M13, Seq(
                    OrEvent(M14, Seq(
                        BasicEvent(X45,p45),
                        BasicEvent(X46,p46),
                        BasicEvent(X47,p47),
                        BasicEvent(X48,p48),
                        BasicEvent(X49,p49)
                    )),
                    BasicEvent(X50,p50)
                )),
                OrEvent(M12, Seq(
                    OrEvent(M15, Seq(
                        AndEvent(M26, Seq(
                            OrEvent(M29, Seq(
                                BasicEvent(X55,p55),
                                BasicEvent(X56,p56),
                                BasicEvent(X57,p57)
                            )),
                            BasicEvent(X54,p54)
                        )),
                        AndEvent(M27, Seq(
                            OrEvent(M28, Seq(
                                BasicEvent(X51,p51),
                                BasicEvent(X52,p52)
                            )),
                            BasicEvent(X53,p53)
                        ))
                    )),
                    AndEvent(M16, Seq(
                        OrEvent(M25, Seq(
                            BasicEvent(X58,p58),
                            BasicEvent(X59,p59)
                        )),
                        BasicEvent(X60,p60),
                        BasicEvent(X61,p61)
                    )),
                )),
                AndEvent(M11, Seq(
                    OrEvent(M17, Seq(
                        BasicEvent(X62,p62),
                        BasicEvent(X63,p63),
                        BasicEvent(X64,p64)
                    )),
                    BasicEvent(X65,p65)
                ))
            )),
            AndEvent(M6, Seq(
                BasicEvent(X66,p66),
                BasicEvent(X67,p67)
            )),
            OrEvent(M7, Seq(
                AndEvent(M10, Seq(
                    OrEvent(M18, Seq(
                        BasicEvent(X84,p84),
                        BasicEvent(X85,p85),
                        BasicEvent(X86,p86)
                    )),
                    BasicEvent(X87,p87)
                )),
                OrEvent(M9, Seq(
                    OrEvent(M19, Seq(
                        OrEvent(M24, Seq(
                            AndEvent(M30, Seq(
                                OrEvent(M33, Seq(
                                    BasicEvent(X68,p68),
                                    BasicEvent(X69,p69)
                                )),
                                BasicEvent(X70,p70)
                            )),
                            AndEvent(M31, Seq(
                                OrEvent(M34, Seq(
                                    BasicEvent(X72,p72),
                                    BasicEvent(X73,p73),
                                    BasicEvent(X74,p74)
                                )),
                                BasicEvent(X71,p71)
                            ))
                        )),
                        AndEvent(M23, Seq(
                            OrEvent(M32, Seq(
                                BasicEvent(X75,p75),
                                BasicEvent(X76,p76)
                            )),
                            BasicEvent(X77,p77)
                        ))
                    )),
                    AndEvent(M20, Seq(
                        OrEvent(M22, Seq(
                            BasicEvent(X78,p78),
                            BasicEvent(X79,p79),
                            BasicEvent(X80,p80)
                        )),
                        BasicEvent(X81,p81)
                    )),
                    OrEvent(M21, Seq(
                        BasicEvent(X82,p82),
                        BasicEvent(X83,p83)
                    ))
                )),
                AndEvent(M8, Seq(
                    BasicEvent(X88,p88),
                    BasicEvent(X89,p89)
                ))
            ))
        )),
        OrEvent(M3, Seq(
            AndEvent(M58, Seq(
                OrEvent(M61, Seq(
                    BasicEvent(X92,p92),
                    BasicEvent(X93,p93)
                )),
                BasicEvent(X90,p90),
                BasicEvent(X91,p91)
            )),
            AndEvent(M59, Seq(
                OrEvent(M62, Seq(
                    BasicEvent(X94,p94),
                    BasicEvent(X95,p95),
                    BasicEvent(X96,p96),
                    BasicEvent(X97,p97),
                    BasicEvent(X98,p98)
                )),
                BasicEvent(X99,p99)
            )),
            AndEvent(M60, Seq(
                OrEvent(M63, Seq(
                    BasicEvent(X100,p100),
                    BasicEvent(X101,p101)
                )),
                BasicEvent(X102,p102),
                BasicEvent(X103,p103)
            ))
        ))
    ))
}

object BayesianNetwork extends TreeLikeFaultTree {
    import faulttree.FaultTree

    val FT: FaultTree = DFT.readTreeLikeFaultTree(Source.fromResource("Bayesian_network.dft"))
}

object LeakageFailure extends TreeLikeFaultTree {
    import faulttree.FaultTree

    val FT: FaultTree = DFT.readTreeLikeFaultTree(Source.fromResource("Leakagefailure.dft"))
}

object AssessingTheRisks1 extends TreeLikeFaultTree {
    import faulttree.FaultTree

    val FT: FaultTree = DFT.readTreeLikeFaultTree(Source.fromResource("AssessingtheRisks1.dft"))
}

object OGPF extends DagLikeFaultTree {
    import minimalcutpathset.FaultTree

    val FT: FaultTree = DFT.readDagLikeFaultTree(Source.fromResource("ogpf.dft"))
}

object PCBA extends TreeLikeFaultTree {
    import faulttree.FaultTree

    val FT: FaultTree = DFT.readTreeLikeFaultTree(Source.fromResource("PCBA.dft"))
}
