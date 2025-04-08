package benchmark

import minimalcutpathset.{MinceNormalised, MinceOrderedSet, PaseNormalised, PaseOrderedSet}
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Mode, OutputTimeUnit, Scope, Setup, State}

import java.util.concurrent.TimeUnit
import scala.collection.immutable.IntMap
import scala.compiletime.uninitialized

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
//@Fork(1) // enables faster benchmark runs, but results may be slightly less accurate
class RealWorldFaultTreesBenchmark {

    // Tree-like FaultTrees:

    private var aircraftRunwayExcursionAccidents_formula: decisiontree.BooleanFormula = uninitialized
    private var aircraftRunwayExcursionAccidents_flattened: faulttree.FaultTree = uninitialized
    private var aircraftRunwayExcursionAccidents_cutsets: minimalcutpathset.CutSets = uninitialized
    private var aircraftRunwayExcursionAccidents_pathsets: minimalcutpathset.PathSets = uninitialized
    private var aircraftRunwayExcursionAccidents_probabilities: IntMap[Double] = uninitialized
    private var aircraftRunwayExcursionAccidents_basicevents: Set[Int] = uninitialized
    private var aircraftRunwayExcursionAccidents_dagtree: minimalcutpathset.FaultTree = uninitialized

    private var mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_faulttree: faulttree.FaultTree = uninitialized
    private var mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_formula: decisiontree.BooleanFormula = uninitialized
    private var mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_flattened: faulttree.FaultTree = uninitialized
    private var mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_cutsets: minimalcutpathset.CutSets = uninitialized
    private var mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_pathsets: minimalcutpathset.PathSets = uninitialized
    private var mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_probabilities: IntMap[Double] = uninitialized
    private var mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_basicevents: Set[Int] = uninitialized
    private var mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_dagtree: minimalcutpathset.FaultTree = uninitialized

    private var atcFailsToResolveTheConflict_formula: decisiontree.BooleanFormula = uninitialized
    private var atcFailsToResolveTheConflict_flattened: faulttree.FaultTree = uninitialized
    private var atcFailsToResolveTheConflict_cutsets: minimalcutpathset.CutSets = uninitialized
    private var atcFailsToResolveTheConflict_pathsets: minimalcutpathset.PathSets = uninitialized
    private var atcFailsToResolveTheConflict_probabilities: IntMap[Double] = uninitialized
    private var atcFailsToResolveTheConflict_basicevents: Set[Int] = uninitialized
    private var atcFailsToResolveTheConflict_dagtree: minimalcutpathset.FaultTree = uninitialized

    private var liquidStorageTank_formula: decisiontree.BooleanFormula = uninitialized
    private var liquidStorageTank_flattened: faulttree.FaultTree = uninitialized
    private var liquidStorageTank_cutsets: minimalcutpathset.CutSets = uninitialized
    private var liquidStorageTank_pathsets: minimalcutpathset.PathSets = uninitialized
    private var liquidStorageTank_probabilities: IntMap[Double] = uninitialized
    private var liquidStorageTank_basicevents: Set[Int] = uninitialized
    private var liquidStorageTank_dagtree: minimalcutpathset.FaultTree = uninitialized

    private var lossContainerAtPort_formula: decisiontree.BooleanFormula = uninitialized
    private var lossContainerAtPort_flattened: faulttree.FaultTree = uninitialized
    private var lossContainerAtPort_cutsets: minimalcutpathset.CutSets = uninitialized
    private var lossContainerAtPort_pathsets: minimalcutpathset.PathSets = uninitialized
    private var lossContainerAtPort_probabilities: IntMap[Double] = uninitialized
    private var lossContainerAtPort_basicevents: Set[Int] = uninitialized
    private var lossContainerAtPort_dagtree: minimalcutpathset.FaultTree = uninitialized

    private var submarinePipelineStopperFailure_formula: decisiontree.BooleanFormula = uninitialized
    private var submarinePipelineStopperFailure_flattened: faulttree.FaultTree = uninitialized
    private var submarinePipelineStopperFailure_cutsets: minimalcutpathset.CutSets = uninitialized
    private var submarinePipelineStopperFailure_pathsets: minimalcutpathset.PathSets = uninitialized
    private var submarinePipelineStopperFailure_probabilities: IntMap[Double] = uninitialized
    private var submarinePipelineStopperFailure_basicevents: Set[Int] = uninitialized
    private var submarinePipelineStopperFailure_dagtree: minimalcutpathset.FaultTree = uninitialized

    private var bhngPipeline_formula: decisiontree.BooleanFormula = uninitialized
    private var bhngPipeline_flattened: faulttree.FaultTree = uninitialized
    private var bhngPipeline_cutsets: minimalcutpathset.CutSets = uninitialized
    private var bhngPipeline_pathsets: minimalcutpathset.PathSets = uninitialized
    private var bhngPipeline_probabilities: IntMap[Double] = uninitialized
    private var bhngPipeline_basicevents: Set[Int] = uninitialized
    private var bhngPipeline_dagtree: minimalcutpathset.FaultTree = uninitialized

    private var leakageFailure_formula: decisiontree.BooleanFormula = uninitialized
    private var leakageFailure_flattened: faulttree.FaultTree = uninitialized
    private var leakageFailure_cutsets: minimalcutpathset.CutSets = uninitialized
    private var leakageFailure_pathsets: minimalcutpathset.PathSets = uninitialized
    private var leakageFailure_probabilities: IntMap[Double] = uninitialized
    private var leakageFailure_basicevents: Set[Int] = uninitialized
    private var leakageFailure_dagtree: minimalcutpathset.FaultTree = uninitialized

    private var assessingTheRisks1_formula: decisiontree.BooleanFormula = uninitialized
    private var assessingTheRisks1_flattened: faulttree.FaultTree = uninitialized
    private var assessingTheRisks1_cutsets: minimalcutpathset.CutSets = uninitialized
    private var assessingTheRisks1_pathsets: minimalcutpathset.PathSets = uninitialized
    private var assessingTheRisks1_probabilities: IntMap[Double] = uninitialized
    private var assessingTheRisks1_basicevents: Set[Int] = uninitialized
    private var assessingTheRisks1_dagtree: minimalcutpathset.FaultTree = uninitialized

    private var pcba_formula: decisiontree.BooleanFormula = uninitialized
    private var pcba_flattened: faulttree.FaultTree = uninitialized
    private var pcba_cutsets: minimalcutpathset.CutSets = uninitialized
    private var pcba_pathsets: minimalcutpathset.PathSets = uninitialized
    private var pcba_probabilities: IntMap[Double] = uninitialized
    private var pcba_basicevents: Set[Int] = uninitialized
    private var pcba_dagtree: minimalcutpathset.FaultTree = uninitialized

    private var hsc_flattened: faulttree.FaultTree = uninitialized
    private var hsc_pathsets: minimalcutpathset.PathSets = uninitialized
    private var hsc_probabilities: IntMap[Double] = uninitialized
    private var hsc_basicevents: Set[Int] = uninitialized
    private var hsc_dagtree: minimalcutpathset.FaultTree = uninitialized

    // DAG-like FaultTrees:

    private var chlorineRelease_formula: decisiontree.BooleanFormula = uninitialized
    private var chlorineRelease_flattened: minimalcutpathset.FaultTree = uninitialized
    private var chlorineRelease_cutsets: minimalcutpathset.CutSets = uninitialized
    private var chlorineRelease_pathsets: minimalcutpathset.PathSets = uninitialized
    private var chlorineRelease_probabilities: IntMap[Double] = uninitialized
    private var chlorineRelease_basicevents: Set[Int] = uninitialized

    private var t0Chopper_formula: decisiontree.BooleanFormula = uninitialized
    private var t0Chopper_flattened: minimalcutpathset.FaultTree = uninitialized
    private var t0Chopper_cutsets: minimalcutpathset.CutSets = uninitialized
    private var t0Chopper_pathsets: minimalcutpathset.PathSets = uninitialized
    private var t0Chopper_probabilities: IntMap[Double] = uninitialized
    private var t0Chopper_basicevents: Set[Int] = uninitialized

    private var ogpf_formula: decisiontree.BooleanFormula = uninitialized
    private var ogpf_flattened: minimalcutpathset.FaultTree = uninitialized
    private var ogpf_cutsets: minimalcutpathset.CutSets = uninitialized
    private var ogpf_pathsets: minimalcutpathset.PathSets = uninitialized
    private var ogpf_probabilities: IntMap[Double] = uninitialized
    private var ogpf_basicevents: Set[Int] = uninitialized

    @Setup
    def setup(): Unit = {
        {
            val aircraftRunwayExcursionAccidents_FT = reallife.FT4_Aircraft.FT
            val (booleanFormula, probabilities) = Conversion.translateToBooleanFormula(aircraftRunwayExcursionAccidents_FT)
            aircraftRunwayExcursionAccidents_formula = booleanFormula
            aircraftRunwayExcursionAccidents_probabilities = probabilities
            val (dagTree, _) = Conversion.translateToDagTree(aircraftRunwayExcursionAccidents_FT)
            val basicEvents = minimalcutpathset.getBasicEvents(dagTree)
            aircraftRunwayExcursionAccidents_cutsets = minimalcutpathset.minimalCutSets(dagTree)(basicEvents)
            aircraftRunwayExcursionAccidents_pathsets = minimalcutpathset.minimalPathSets(dagTree)(basicEvents)
            aircraftRunwayExcursionAccidents_flattened = faulttree.flatten(aircraftRunwayExcursionAccidents_FT)
            aircraftRunwayExcursionAccidents_basicevents = decisiontree.RandomBDTs.getBasicEvents(aircraftRunwayExcursionAccidents_formula)
            aircraftRunwayExcursionAccidents_dagtree = dagTree
        }

        {
            mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_faulttree = reallife.FT1_AssessingTheRisks2.FT
            val (booleanFormula, probabilities) = Conversion.translateToBooleanFormula(mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_faulttree)
            mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_formula = booleanFormula
            mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_probabilities = probabilities
            val (dagTree, _) = Conversion.translateToDagTree(mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_faulttree)
            val basicEvents = minimalcutpathset.getBasicEvents(dagTree)
            mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_cutsets = minimalcutpathset.minimalCutSets(dagTree)(basicEvents)
            mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_pathsets = minimalcutpathset.minimalPathSets(dagTree)(basicEvents)
            mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_flattened = faulttree.flatten(mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_faulttree)
            mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_basicevents = decisiontree.RandomBDTs.getBasicEvents(mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_formula)
            mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_dagtree = dagTree
        }

        {
            val atcFailsToResolveTheConflict_FT = reallife.FT3_ATC.FT
            val (booleanFormula, probabilities) = Conversion.translateToBooleanFormula(atcFailsToResolveTheConflict_FT)
            atcFailsToResolveTheConflict_formula = booleanFormula
            atcFailsToResolveTheConflict_probabilities = probabilities
            val (dagTree, _) = Conversion.translateToDagTree(atcFailsToResolveTheConflict_FT)
            val basicEvents = minimalcutpathset.getBasicEvents(dagTree)
            atcFailsToResolveTheConflict_cutsets = minimalcutpathset.minimalCutSets(dagTree)(basicEvents)
            atcFailsToResolveTheConflict_pathsets = minimalcutpathset.minimalPathSets(dagTree)(basicEvents)
            atcFailsToResolveTheConflict_flattened = faulttree.flatten(atcFailsToResolveTheConflict_FT)
            atcFailsToResolveTheConflict_basicevents = decisiontree.RandomBDTs.getBasicEvents(atcFailsToResolveTheConflict_formula)
            atcFailsToResolveTheConflict_dagtree = dagTree
        }

        {
            val liquidStorageTank_FT = reallife.FT5_LiquidStorageTank.FT
            val (booleanFormula, probabilities) = Conversion.translateToBooleanFormula(liquidStorageTank_FT)
            liquidStorageTank_formula = booleanFormula
            liquidStorageTank_probabilities = probabilities
            val (dagTree, _) = Conversion.translateToDagTree(liquidStorageTank_FT)
            val basicEvents = minimalcutpathset.getBasicEvents(dagTree)
            liquidStorageTank_cutsets = minimalcutpathset.minimalCutSets(dagTree)(basicEvents)
            liquidStorageTank_pathsets = minimalcutpathset.minimalPathSets(dagTree)(basicEvents)
            liquidStorageTank_flattened = faulttree.flatten(liquidStorageTank_FT)
            liquidStorageTank_basicevents = decisiontree.RandomBDTs.getBasicEvents(liquidStorageTank_formula)
            liquidStorageTank_dagtree = dagTree
        }

        {
            val lossContainerAtPort_FT = reallife.FT9_LossContainerPort.FT
            val (booleanFormula, probabilities) = Conversion.translateToBooleanFormula(lossContainerAtPort_FT)
            lossContainerAtPort_formula = booleanFormula
            lossContainerAtPort_probabilities = probabilities
            val (dagTree, _) = Conversion.translateToDagTree(lossContainerAtPort_FT)
            val basicEvents = minimalcutpathset.getBasicEvents(dagTree)
            lossContainerAtPort_cutsets = minimalcutpathset.minimalCutSets(dagTree)(basicEvents)
            lossContainerAtPort_pathsets = minimalcutpathset.minimalPathSets(dagTree)(basicEvents)
            lossContainerAtPort_flattened = faulttree.flatten(lossContainerAtPort_FT)
            lossContainerAtPort_basicevents = decisiontree.RandomBDTs.getBasicEvents(lossContainerAtPort_formula)
            lossContainerAtPort_dagtree = dagTree
        }

        {
            val submarinePipelineStopperFailure_FT = reallife.FT10_Stopper.FT
            val (booleanFormula, probabilities) = Conversion.translateToBooleanFormula(submarinePipelineStopperFailure_FT)
            submarinePipelineStopperFailure_formula = booleanFormula
            submarinePipelineStopperFailure_probabilities = probabilities
            val (dagTree, _) = Conversion.translateToDagTree(submarinePipelineStopperFailure_FT)
            val basicEvents = minimalcutpathset.getBasicEvents(dagTree)
            submarinePipelineStopperFailure_cutsets = minimalcutpathset.minimalCutSets(dagTree)(basicEvents)
            submarinePipelineStopperFailure_pathsets = minimalcutpathset.minimalPathSets(dagTree)(basicEvents)
            submarinePipelineStopperFailure_flattened = faulttree.flatten(submarinePipelineStopperFailure_FT)
            submarinePipelineStopperFailure_basicevents = decisiontree.RandomBDTs.getBasicEvents(submarinePipelineStopperFailure_formula)
            submarinePipelineStopperFailure_dagtree = dagTree
        }

        {
            val bhngPipeline_FT = reallife.FT13_BHNGPipeline.FT
            val (booleanFormula, probabilities) = Conversion.translateToBooleanFormula(bhngPipeline_FT)
            bhngPipeline_formula = booleanFormula
            bhngPipeline_probabilities = probabilities
            val (dagTree, _) = Conversion.translateToDagTree(bhngPipeline_FT)
            val basicEvents = minimalcutpathset.getBasicEvents(dagTree)
            bhngPipeline_cutsets = minimalcutpathset.minimalCutSets(dagTree)(basicEvents)
            bhngPipeline_pathsets = minimalcutpathset.minimalPathSets(dagTree)(basicEvents)
            bhngPipeline_flattened = faulttree.flatten(bhngPipeline_FT)
            bhngPipeline_basicevents = decisiontree.RandomBDTs.getBasicEvents(bhngPipeline_formula)
            bhngPipeline_dagtree = dagTree
        }

        {
            val leakageFailure_FT = reallife.FT6_LeakageFailure.FT
            val (booleanFormula, probabilities) = Conversion.translateToBooleanFormula(leakageFailure_FT)
            leakageFailure_formula = booleanFormula
            leakageFailure_probabilities = probabilities
            val (dagTree, _) = Conversion.translateToDagTree(leakageFailure_FT)
            val basicEvents = minimalcutpathset.getBasicEvents(dagTree)
            leakageFailure_cutsets = minimalcutpathset.minimalCutSets(dagTree)(basicEvents)
            leakageFailure_pathsets = minimalcutpathset.minimalPathSets(dagTree)(basicEvents)
            leakageFailure_flattened = faulttree.flatten(leakageFailure_FT)
            leakageFailure_basicevents = decisiontree.RandomBDTs.getBasicEvents(leakageFailure_formula)
            leakageFailure_dagtree = dagTree
        }

        {
            val assessingTheRisks1_FT = reallife.FT7_AssessingTheRisks1.FT
            val (booleanFormula, probabilities) = Conversion.translateToBooleanFormula(assessingTheRisks1_FT)
            assessingTheRisks1_formula = booleanFormula
            assessingTheRisks1_probabilities = probabilities
            val (dagTree, _) = Conversion.translateToDagTree(assessingTheRisks1_FT)
            val basicEvents = minimalcutpathset.getBasicEvents(dagTree)
            assessingTheRisks1_cutsets = minimalcutpathset.minimalCutSets(dagTree)(basicEvents)
            assessingTheRisks1_pathsets = minimalcutpathset.minimalPathSets(dagTree)(basicEvents)
            assessingTheRisks1_flattened = faulttree.flatten(assessingTheRisks1_FT)
            assessingTheRisks1_basicevents = decisiontree.RandomBDTs.getBasicEvents(assessingTheRisks1_formula)
            assessingTheRisks1_dagtree = dagTree
        }

        {
            val pcba_FT = reallife.FT11_PCBA.FT
            val (booleanFormula, probabilities) = Conversion.translateToBooleanFormula(pcba_FT)
            pcba_formula = booleanFormula
            pcba_probabilities = probabilities
            val (dagTree, _) = Conversion.translateToDagTree(pcba_FT)
            val basicEvents = minimalcutpathset.getBasicEvents(dagTree)
            pcba_cutsets = minimalcutpathset.minimalCutSets(dagTree)(basicEvents)
            pcba_pathsets = minimalcutpathset.minimalPathSets(dagTree)(basicEvents)
            pcba_flattened = faulttree.flatten(pcba_FT)
            pcba_basicevents = decisiontree.RandomBDTs.getBasicEvents(pcba_formula)
            pcba_dagtree = dagTree
        }

        {
            val hsc_FT = reallife.FT14_HSC.FT
            val (dagTree, probabilities) = Conversion.translateToDagTree(hsc_FT)
            hsc_flattened = faulttree.flatten(hsc_FT)
            hsc_probabilities = probabilities
            hsc_basicevents = minimalcutpathset.getBasicEvents(dagTree)
            hsc_pathsets = minimalcutpathset.minimalPathSets(dagTree)(hsc_basicevents)
            hsc_dagtree = dagTree
        }

        {
            val chlorineRelease_FT = reallife.FT8_ChlorineRelease.FT
            val (booleanFormula, probabilities) = Conversion.translateToBooleanFormula(chlorineRelease_FT)
            chlorineRelease_formula = booleanFormula
            chlorineRelease_probabilities = probabilities
            val basicEvents = minimalcutpathset.getBasicEvents(chlorineRelease_FT)
            chlorineRelease_cutsets = minimalcutpathset.minimalCutSets(chlorineRelease_FT)(basicEvents)
            chlorineRelease_pathsets = minimalcutpathset.minimalPathSets(chlorineRelease_FT)(basicEvents)
            chlorineRelease_flattened = minimalcutpathset.flatten(chlorineRelease_FT)
            chlorineRelease_basicevents = decisiontree.RandomBDTs.getBasicEvents(chlorineRelease_formula)
        }

        {
            val t0Chopper_FT = reallife.FT2_T0Chopper.FT
            val (booleanFormula, probabilities) = Conversion.translateToBooleanFormula(t0Chopper_FT)
            t0Chopper_formula = booleanFormula
            t0Chopper_probabilities = probabilities
            val basicEvents = minimalcutpathset.getBasicEvents(t0Chopper_FT)
            t0Chopper_cutsets = minimalcutpathset.minimalCutSets(t0Chopper_FT)(basicEvents)
            t0Chopper_pathsets = minimalcutpathset.minimalPathSets(t0Chopper_FT)(basicEvents)
            t0Chopper_flattened = minimalcutpathset.flatten(t0Chopper_FT)
            t0Chopper_basicevents = decisiontree.RandomBDTs.getBasicEvents(t0Chopper_formula)
        }

        {
            val ogpf_FT = reallife.FT12_OGPF.FT
            val (booleanFormula, probabilities) = Conversion.translateToBooleanFormula(ogpf_FT)
            ogpf_formula = booleanFormula
            ogpf_probabilities = probabilities
            val basicEvents = minimalcutpathset.getBasicEvents(ogpf_FT)
            ogpf_cutsets = minimalcutpathset.minimalCutSets(ogpf_FT)(basicEvents)
            ogpf_pathsets = minimalcutpathset.minimalPathSets(ogpf_FT)(basicEvents)
            ogpf_flattened = minimalcutpathset.flatten(ogpf_FT)
            ogpf_basicevents = decisiontree.RandomBDTs.getBasicEvents(ogpf_formula)
        }
    }

    // MainTrackTrainCollisionsLeadingToFatalitiesAndInjuries

    @Benchmark
    def timeFT1_MainTrackTrainCollisionsLeadingToFatalitiesAndInjuries_EDA(): Double =
        decisiontree.height(mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_formula, mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_probabilities)

    @Benchmark
    def timeFT1_MainTrackTrainCollisionsLeadingToFatalitiesAndInjuries_BUDA(): Double =
        faulttree.height7(mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_flattened)

    @Benchmark
    def timeFT1_MainTrackTrainCollisionsLeadingToFatalitiesAndInjuries_CuDA(): Double =
        val cutsets = minimalcutpathset.minimalCutSets(mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_dagtree)(mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_basicevents)
        minimalcutpathset.algorithm4(cutsets, mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_probabilities)._2

    @Benchmark
    def timeFT1_MainTrackTrainCollisionsLeadingToFatalitiesAndInjuries_CuDA_normalised(): Double =
        val cutsets = minimalcutpathset.minimalCutSets(mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_dagtree)(mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_basicevents)
        MinceNormalised.minceNormalised(cutsets, mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_probabilities)._2

    @Benchmark
    def timeFT1_MainTrackTrainCollisionsLeadingToFatalitiesAndInjuries_CuDA_orderedSet(): Double =
        val cutsets = minimalcutpathset.minimalCutSets(mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_dagtree)(mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_basicevents)
        MinceOrderedSet.minceOrderedSet(cutsets, mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_probabilities)._2

    @Benchmark
    def timeFT1_MainTrackTrainCollisionsLeadingToFatalitiesAndInjuries_PaDA(): Double =
        val pathsets = minimalcutpathset.minimalPathSets(mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_dagtree)(mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_basicevents)
        minimalcutpathset.algorithm5(pathsets, mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_probabilities)._2

    @Benchmark
    def timeFT1_MainTrackTrainCollisionsLeadingToFatalitiesAndInjuries_PaDA_normalised(): Double =
        val pathsets = minimalcutpathset.minimalPathSets(mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_dagtree)(mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_basicevents)
        PaseNormalised.paseNormalised(pathsets, mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_probabilities)._2

    @Benchmark
    def timeFT1_MainTrackTrainCollisionsLeadingToFatalitiesAndInjuries_PaDA_orderedSet(): Double =
        val pathsets = minimalcutpathset.minimalPathSets(mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_dagtree)(mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_basicevents)
        PaseOrderedSet.paseOrderedSet(pathsets, mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_probabilities)._2

    // T0Chopper

    @Benchmark
    def timeFT2_T0Chopper_BUDA(): Double =
        decisiontree.algorithm8(t0Chopper_flattened, t0Chopper_probabilities)._2

    @Benchmark
    def timeFT2_T0Chopper_CuDA(): Double =
        val cutsets = minimalcutpathset.minimalCutSets(t0Chopper_flattened)(t0Chopper_basicevents)
        minimalcutpathset.algorithm4(cutsets, t0Chopper_probabilities)._2

    @Benchmark
    def timeFT2_T0Chopper_CuDA_normalised(): Double =
        val cutsets = minimalcutpathset.minimalCutSets(t0Chopper_flattened)(t0Chopper_basicevents)
        MinceNormalised.minceNormalised(cutsets, t0Chopper_probabilities)._2

    @Benchmark
    def timeFT2_T0Chopper_CuDA_orderedSet(): Double =
        val cutsets = minimalcutpathset.minimalCutSets(t0Chopper_flattened)(t0Chopper_basicevents)
        MinceOrderedSet.minceOrderedSet(cutsets, t0Chopper_probabilities)._2

    @Benchmark
    def timeFT2_T0Chopper_PaDA(): Double =
        val pathsets = minimalcutpathset.minimalPathSets(t0Chopper_flattened)(t0Chopper_basicevents)
        minimalcutpathset.algorithm5(pathsets, t0Chopper_probabilities)._2

    @Benchmark
    def timeFT2_T0Chopper_PaDA_normalised(): Double =
        val pathsets = minimalcutpathset.minimalPathSets(t0Chopper_flattened)(t0Chopper_basicevents)
        PaseNormalised.paseNormalised(pathsets, t0Chopper_probabilities)._2

    @Benchmark
    def timeFT2_T0Chopper_PaDA_orderedSet(): Double =
        val pathsets = minimalcutpathset.minimalPathSets(t0Chopper_flattened)(t0Chopper_basicevents)
        PaseOrderedSet.paseOrderedSet(pathsets, t0Chopper_probabilities)._2

    // ATCFailsToResolveTheConflict

    @Benchmark
    def timeFT3_ATCFailsToResolveTheConflict_BUDA(): Double =
        faulttree.height7(atcFailsToResolveTheConflict_flattened)

    @Benchmark
    def timeFT3_ATCFailsToResolveTheConflict_CuDA(): Double =
        val cutsets = minimalcutpathset.minimalCutSets(atcFailsToResolveTheConflict_dagtree)(atcFailsToResolveTheConflict_basicevents)
        minimalcutpathset.algorithm4(cutsets, atcFailsToResolveTheConflict_probabilities)._2

    @Benchmark
    def timeFT3_ATCFailsToResolveTheConflict_CuDA_normalised(): Double =
        val cutsets = minimalcutpathset.minimalCutSets(atcFailsToResolveTheConflict_dagtree)(atcFailsToResolveTheConflict_basicevents)
        MinceNormalised.minceNormalised(cutsets, atcFailsToResolveTheConflict_probabilities)._2

    @Benchmark
    def timeFT3_ATCFailsToResolveTheConflict_CuDA_orderedSet(): Double =
        val cutsets = minimalcutpathset.minimalCutSets(atcFailsToResolveTheConflict_dagtree)(atcFailsToResolveTheConflict_basicevents)
        MinceOrderedSet.minceOrderedSet(cutsets, atcFailsToResolveTheConflict_probabilities)._2

    @Benchmark
    def timeFT3_ATCFailsToResolveTheConflict_PaDA(): Double =
        val pathsets = minimalcutpathset.minimalPathSets(atcFailsToResolveTheConflict_dagtree)(atcFailsToResolveTheConflict_basicevents)
        minimalcutpathset.algorithm5(pathsets, atcFailsToResolveTheConflict_probabilities)._2

    @Benchmark
    def timeFT3_ATCFailsToResolveTheConflict_PaDA_normalised(): Double =
        val pathsets = minimalcutpathset.minimalPathSets(atcFailsToResolveTheConflict_dagtree)(atcFailsToResolveTheConflict_basicevents)
        PaseNormalised.paseNormalised(pathsets, atcFailsToResolveTheConflict_probabilities)._2

    @Benchmark
    def timeFT3_ATCFailsToResolveTheConflict_PaDA_orderedSet(): Double =
        val pathsets = minimalcutpathset.minimalPathSets(atcFailsToResolveTheConflict_dagtree)(atcFailsToResolveTheConflict_basicevents)
        PaseOrderedSet.paseOrderedSet(pathsets, atcFailsToResolveTheConflict_probabilities)._2

    // AircraftRunwayExcursionAccidents

    @Benchmark
    def timeFT4_AircraftRunwayExcursionAccidents_BUDA(): Double =
        faulttree.height7(aircraftRunwayExcursionAccidents_flattened)

    @Benchmark
    def timeFT4_AircraftRunwayExcursionAccidents_CuDA(): Double =
        val cutsets = minimalcutpathset.minimalCutSets(aircraftRunwayExcursionAccidents_dagtree)(aircraftRunwayExcursionAccidents_basicevents)
        minimalcutpathset.algorithm4(cutsets, aircraftRunwayExcursionAccidents_probabilities)._2

    @Benchmark
    def timeFT4_AircraftRunwayExcursionAccidents_CuDA_normalised(): Double =
        val cutsets = minimalcutpathset.minimalCutSets(aircraftRunwayExcursionAccidents_dagtree)(aircraftRunwayExcursionAccidents_basicevents)
        MinceNormalised.minceNormalised(cutsets, aircraftRunwayExcursionAccidents_probabilities)._2

    @Benchmark
    def timeFT4_AircraftRunwayExcursionAccidents_CuDA_orderedSet(): Double =
        val cutsets = minimalcutpathset.minimalCutSets(aircraftRunwayExcursionAccidents_dagtree)(aircraftRunwayExcursionAccidents_basicevents)
        MinceOrderedSet.minceOrderedSet(cutsets, aircraftRunwayExcursionAccidents_probabilities)._2

    @Benchmark
    def timeFT4_AircraftRunwayExcursionAccidents_PaDA(): Double =
        val pathsets = minimalcutpathset.minimalPathSets(aircraftRunwayExcursionAccidents_dagtree)(aircraftRunwayExcursionAccidents_basicevents)
        minimalcutpathset.algorithm5(pathsets, aircraftRunwayExcursionAccidents_probabilities)._2

    @Benchmark
    def timeFT4_AircraftRunwayExcursionAccidents_PaDA_normalised(): Double =
        val pathsets = minimalcutpathset.minimalPathSets(aircraftRunwayExcursionAccidents_dagtree)(aircraftRunwayExcursionAccidents_basicevents)
        PaseNormalised.paseNormalised(pathsets, aircraftRunwayExcursionAccidents_probabilities)._2

    @Benchmark
    def timeFT4_AircraftRunwayExcursionAccidents_PaDA_orderedSet(): Double =
        val pathsets = minimalcutpathset.minimalPathSets(aircraftRunwayExcursionAccidents_dagtree)(aircraftRunwayExcursionAccidents_basicevents)
        PaseOrderedSet.paseOrderedSet(pathsets, aircraftRunwayExcursionAccidents_probabilities)._2

    // LiquidStorageTank

    @Benchmark
    def timeFT5_LiquidStorageTank_BUDA(): Double =
        faulttree.height7(liquidStorageTank_flattened)

    @Benchmark
    def timeFT5_LiquidStorageTank_CuDA(): Double =
        val cutsets = minimalcutpathset.minimalCutSets(liquidStorageTank_dagtree)(liquidStorageTank_basicevents)
        minimalcutpathset.algorithm4(cutsets, liquidStorageTank_probabilities)._2

    @Benchmark
    def timeFT5_LiquidStorageTank_CuDA_normalised(): Double =
        val cutsets = minimalcutpathset.minimalCutSets(liquidStorageTank_dagtree)(liquidStorageTank_basicevents)
        MinceNormalised.minceNormalised(cutsets, liquidStorageTank_probabilities)._2

    @Benchmark
    def timeFT5_LiquidStorageTank_CuDA_orderedSet(): Double =
        val cutsets = minimalcutpathset.minimalCutSets(liquidStorageTank_dagtree)(liquidStorageTank_basicevents)
        MinceOrderedSet.minceOrderedSet(cutsets, liquidStorageTank_probabilities)._2

    @Benchmark
    def timeFT5_LiquidStorageTank_PaDA(): Double =
        val pathsets = minimalcutpathset.minimalPathSets(liquidStorageTank_dagtree)(liquidStorageTank_basicevents)
        minimalcutpathset.algorithm5(pathsets, liquidStorageTank_probabilities)._2

    @Benchmark
    def timeFT5_LiquidStorageTank_PaDA_normalised(): Double =
        val pathsets = minimalcutpathset.minimalPathSets(liquidStorageTank_dagtree)(liquidStorageTank_basicevents)
        PaseNormalised.paseNormalised(pathsets, liquidStorageTank_probabilities)._2

    @Benchmark
    def timeFT5_LiquidStorageTank_PaDA_orderedSet(): Double =
        val pathsets = minimalcutpathset.minimalPathSets(liquidStorageTank_dagtree)(liquidStorageTank_basicevents)
        PaseOrderedSet.paseOrderedSet(pathsets, liquidStorageTank_probabilities)._2

    // LeakageFailure

    @Benchmark
    def timeFT6_LeakageFailure_BUDA(): Double =
        faulttree.height7(leakageFailure_flattened)

    @Benchmark
    def timeFT6_LeakageFailure_CuDA(): Double =
        val cutsets = minimalcutpathset.minimalCutSets(leakageFailure_dagtree)(leakageFailure_basicevents)
        minimalcutpathset.algorithm4(cutsets, leakageFailure_probabilities)._2

    @Benchmark
    def timeFT6_LeakageFailure_CuDA_normalised(): Double =
        val cutsets = minimalcutpathset.minimalCutSets(leakageFailure_dagtree)(leakageFailure_basicevents)
        MinceNormalised.minceNormalised(cutsets, leakageFailure_probabilities)._2

    @Benchmark
    def timeFT6_LeakageFailure_CuDA_orderedSet(): Double =
        val cutsets = minimalcutpathset.minimalCutSets(leakageFailure_dagtree)(leakageFailure_basicevents)
        MinceOrderedSet.minceOrderedSet(cutsets, leakageFailure_probabilities)._2

    @Benchmark
    def timeFT6_LeakageFailure_PaDA(): Double =
        val pathsets = minimalcutpathset.minimalPathSets(leakageFailure_dagtree)(leakageFailure_basicevents)
        minimalcutpathset.algorithm5(pathsets, leakageFailure_probabilities)._2

    @Benchmark
    def timeFT6_LeakageFailure_PaDA_normalised(): Double =
        val pathsets = minimalcutpathset.minimalPathSets(leakageFailure_dagtree)(leakageFailure_basicevents)
        PaseNormalised.paseNormalised(pathsets, leakageFailure_probabilities)._2

    @Benchmark
    def timeFT6_LeakageFailure_PaDA_orderedSet(): Double =
        val pathsets = minimalcutpathset.minimalPathSets(leakageFailure_dagtree)(leakageFailure_basicevents)
        PaseOrderedSet.paseOrderedSet(pathsets, leakageFailure_probabilities)._2

    // AssessingTheRisks1

    @Benchmark
    def timeFT7_AssessingTheRisks1_BUDA(): Double =
        faulttree.height7(assessingTheRisks1_flattened)

    @Benchmark
    def timeFT7_AssessingTheRisks1_CuDA(): Double =
        val cutsets = minimalcutpathset.minimalCutSets(assessingTheRisks1_dagtree)(assessingTheRisks1_basicevents)
        minimalcutpathset.algorithm4(cutsets, assessingTheRisks1_probabilities)._2

    @Benchmark
    def timeFT7_AssessingTheRisks1_CuDA_normalised(): Double =
        val cutsets = minimalcutpathset.minimalCutSets(assessingTheRisks1_dagtree)(assessingTheRisks1_basicevents)
        MinceNormalised.minceNormalised(cutsets, assessingTheRisks1_probabilities)._2

    @Benchmark
    def timeFT7_AssessingTheRisks1_CuDA_orderedSet(): Double =
        val cutsets = minimalcutpathset.minimalCutSets(assessingTheRisks1_dagtree)(assessingTheRisks1_basicevents)
        MinceOrderedSet.minceOrderedSet(cutsets, assessingTheRisks1_probabilities)._2

    @Benchmark
    def timeFT7_AssessingTheRisks1_PaDA(): Double =
        val pathsets = minimalcutpathset.minimalPathSets(assessingTheRisks1_dagtree)(assessingTheRisks1_basicevents)
        minimalcutpathset.algorithm5(pathsets, assessingTheRisks1_probabilities)._2

    @Benchmark
    def timeFT7_AssessingTheRisks1_PaDA_normalised(): Double =
        val pathsets = minimalcutpathset.minimalPathSets(assessingTheRisks1_dagtree)(assessingTheRisks1_basicevents)
        PaseNormalised.paseNormalised(pathsets, assessingTheRisks1_probabilities)._2

    @Benchmark
    def timeFT7_AssessingTheRisks1_PaDA_orderedSet(): Double =
        val pathsets = minimalcutpathset.minimalPathSets(assessingTheRisks1_dagtree)(assessingTheRisks1_basicevents)
        PaseOrderedSet.paseOrderedSet(pathsets, assessingTheRisks1_probabilities)._2

    // ChlorineRelease

    @Benchmark
    def timeFT8_ChlorineRelease_BUDA(): Double =
        decisiontree.algorithm8(chlorineRelease_flattened, chlorineRelease_probabilities)._2

    @Benchmark
    def timeFT8_ChlorineRelease_CuDA(): Double =
        val cutsets = minimalcutpathset.minimalCutSets(chlorineRelease_flattened)(chlorineRelease_basicevents)
        minimalcutpathset.algorithm4(cutsets, chlorineRelease_probabilities)._2

    @Benchmark
    def timeFT8_ChlorineRelease_CuDA_normalised(): Double =
        val cutsets = minimalcutpathset.minimalCutSets(chlorineRelease_flattened)(chlorineRelease_basicevents)
        MinceNormalised.minceNormalised(cutsets, chlorineRelease_probabilities)._2

    @Benchmark
    def timeFT8_ChlorineRelease_CuDA_orderedSet(): Double =
        val cutsets = minimalcutpathset.minimalCutSets(chlorineRelease_flattened)(chlorineRelease_basicevents)
        MinceOrderedSet.minceOrderedSet(cutsets, chlorineRelease_probabilities)._2

    @Benchmark
    def timeFT8_ChlorineRelease_PaDA(): Double =
        val pathsets = minimalcutpathset.minimalPathSets(chlorineRelease_flattened)(chlorineRelease_basicevents)
        minimalcutpathset.algorithm5(pathsets, chlorineRelease_probabilities)._2

    @Benchmark
    def timeFT8_ChlorineRelease_PaDA_normalised(): Double =
        val pathsets = minimalcutpathset.minimalPathSets(chlorineRelease_flattened)(chlorineRelease_basicevents)
        PaseNormalised.paseNormalised(pathsets, chlorineRelease_probabilities)._2

    @Benchmark
    def timeFT8_ChlorineRelease_PaDA_orderedSet(): Double =
        val pathsets = minimalcutpathset.minimalPathSets(chlorineRelease_flattened)(chlorineRelease_basicevents)
        PaseOrderedSet.paseOrderedSet(pathsets, chlorineRelease_probabilities)._2

    // LossContainerAtPort

    @Benchmark
    def timeFT9_LossContainerAtPort_BUDA(): Double =
        faulttree.height7(lossContainerAtPort_flattened)

    @Benchmark
    def timeFT9_LossContainerAtPort_CuDA(): Double =
        val cutsets = minimalcutpathset.minimalCutSets(lossContainerAtPort_dagtree)(lossContainerAtPort_basicevents)
        minimalcutpathset.algorithm4(cutsets, lossContainerAtPort_probabilities)._2

    @Benchmark
    def timeFT9_LossContainerAtPort_CuDA_normalised(): Double =
        val cutsets = minimalcutpathset.minimalCutSets(lossContainerAtPort_dagtree)(lossContainerAtPort_basicevents)
        MinceNormalised.minceNormalised(cutsets, lossContainerAtPort_probabilities)._2

    @Benchmark
    def timeFT9_LossContainerAtPort_CuDA_orderedSet(): Double =
        val cutsets = minimalcutpathset.minimalCutSets(lossContainerAtPort_dagtree)(lossContainerAtPort_basicevents)
        MinceOrderedSet.minceOrderedSet(cutsets, lossContainerAtPort_probabilities)._2

    @Benchmark
    def timeFT9_LossContainerAtPort_PaDA(): Double =
        val pathsets = minimalcutpathset.minimalPathSets(lossContainerAtPort_dagtree)(lossContainerAtPort_basicevents)
        minimalcutpathset.algorithm5(pathsets, lossContainerAtPort_probabilities)._2

    @Benchmark
    def timeFT9_LossContainerAtPort_PaDA_normalised(): Double =
        val pathsets = minimalcutpathset.minimalPathSets(lossContainerAtPort_dagtree)(lossContainerAtPort_basicevents)
        PaseNormalised.paseNormalised(pathsets, lossContainerAtPort_probabilities)._2

    @Benchmark
    def timeFT9_LossContainerAtPort_PaDA_orderedSet(): Double =
        val pathsets = minimalcutpathset.minimalPathSets(lossContainerAtPort_dagtree)(lossContainerAtPort_basicevents)
        PaseOrderedSet.paseOrderedSet(pathsets, lossContainerAtPort_probabilities)._2

    // SubmarinePipelineStopperFailure

    @Benchmark
    def timeFT10_SubmarinePipelineStopperFailure_BUDA(): Double =
        faulttree.height7(submarinePipelineStopperFailure_flattened)

    @Benchmark
    def timeFT10_SubmarinePipelineStopperFailure_CuDA(): Double =
        val cutsets = minimalcutpathset.minimalCutSets(submarinePipelineStopperFailure_dagtree)(submarinePipelineStopperFailure_basicevents)
        minimalcutpathset.algorithm4(cutsets, submarinePipelineStopperFailure_probabilities)._2

    @Benchmark
    def timeFT10_SubmarinePipelineStopperFailure_CuDA_normalised(): Double =
        val cutsets = minimalcutpathset.minimalCutSets(submarinePipelineStopperFailure_dagtree)(submarinePipelineStopperFailure_basicevents)
        MinceNormalised.minceNormalised(cutsets, submarinePipelineStopperFailure_probabilities)._2

    @Benchmark
    def timeFT10_SubmarinePipelineStopperFailure_CuDA_orderedSet(): Double =
        val cutsets = minimalcutpathset.minimalCutSets(submarinePipelineStopperFailure_dagtree)(submarinePipelineStopperFailure_basicevents)
        MinceOrderedSet.minceOrderedSet(cutsets, submarinePipelineStopperFailure_probabilities)._2

    @Benchmark
    def timeFT10_SubmarinePipelineStopperFailure_PaDA(): Double =
        val pathsets = minimalcutpathset.minimalPathSets(submarinePipelineStopperFailure_dagtree)(submarinePipelineStopperFailure_basicevents)
        minimalcutpathset.algorithm5(pathsets, submarinePipelineStopperFailure_probabilities)._2

    @Benchmark
    def timeFT10_SubmarinePipelineStopperFailure_PaDA_normalised(): Double =
        val pathsets = minimalcutpathset.minimalPathSets(submarinePipelineStopperFailure_dagtree)(submarinePipelineStopperFailure_basicevents)
        PaseNormalised.paseNormalised(pathsets, submarinePipelineStopperFailure_probabilities)._2

    @Benchmark
    def timeFT10_SubmarinePipelineStopperFailure_PaDA_orderedSet(): Double =
        val pathsets = minimalcutpathset.minimalPathSets(submarinePipelineStopperFailure_dagtree)(submarinePipelineStopperFailure_basicevents)
        PaseOrderedSet.paseOrderedSet(pathsets, submarinePipelineStopperFailure_probabilities)._2

    // PCBA

    @Benchmark
    def timeFT11_PCBA_BUDA(): Double =
        faulttree.height7(pcba_flattened)

    @Benchmark
    def timeFT11_PCBA_CuDA(): Double =
        val cutsets = minimalcutpathset.minimalCutSets(pcba_dagtree)(pcba_basicevents)
        minimalcutpathset.algorithm4(cutsets, pcba_probabilities)._2

    @Benchmark
    def timeFT11_PCBA_CuDA_normalised(): Double =
        val cutsets = minimalcutpathset.minimalCutSets(pcba_dagtree)(pcba_basicevents)
        MinceNormalised.minceNormalised(cutsets, pcba_probabilities)._2

    @Benchmark
    def timeFT11_PCBA_CuDA_orderedSet(): Double =
        val cutsets = minimalcutpathset.minimalCutSets(pcba_dagtree)(pcba_basicevents)
        MinceOrderedSet.minceOrderedSet(cutsets, pcba_probabilities)._2

    @Benchmark
    def timeFT11_PCBA_PaDA(): Double =
        val pathsets = minimalcutpathset.minimalPathSets(pcba_dagtree)(pcba_basicevents)
        minimalcutpathset.algorithm5(pathsets, pcba_probabilities)._2

    @Benchmark
    def timeFT11_PCBA_PaDA_normalised(): Double =
        val pathsets = minimalcutpathset.minimalPathSets(pcba_dagtree)(pcba_basicevents)
        PaseNormalised.paseNormalised(pathsets, pcba_probabilities)._2

    @Benchmark
    def timeFT11_PCBA_PaDA_orderedSet(): Double =
        val pathsets = minimalcutpathset.minimalPathSets(pcba_dagtree)(pcba_basicevents)
        PaseOrderedSet.paseOrderedSet(pathsets, pcba_probabilities)._2

    // OGPF

    @Benchmark
    def timeFT12_OGPF_BUDA(): Double =
        decisiontree.algorithm8(ogpf_flattened, ogpf_probabilities)._2

    @Benchmark
    def timeFT12_OGPF_CuDA(): Double =
        val cutsets = minimalcutpathset.minimalCutSets(ogpf_flattened)(ogpf_basicevents)
        minimalcutpathset.algorithm4(cutsets, ogpf_probabilities)._2

    @Benchmark
    def timeFT12_OGPF_CuDA_normalised(): Double =
        val cutsets = minimalcutpathset.minimalCutSets(ogpf_flattened)(ogpf_basicevents)
        MinceNormalised.minceNormalised(cutsets, ogpf_probabilities)._2

    @Benchmark
    def timeFT12_OGPF_CuDA_orderedSet(): Double =
        val cutsets = minimalcutpathset.minimalCutSets(ogpf_flattened)(ogpf_basicevents)
        MinceOrderedSet.minceOrderedSet(cutsets, ogpf_probabilities)._2

    @Benchmark
    def timeFT12_OGPF_PaDA(): Double =
        val pathsets = minimalcutpathset.minimalPathSets(ogpf_flattened)(ogpf_basicevents)
        minimalcutpathset.algorithm5(pathsets, ogpf_probabilities)._2

    @Benchmark
    def timeFT12_OGPF_PaDA_normalised(): Double =
        val pathsets = minimalcutpathset.minimalPathSets(ogpf_flattened)(ogpf_basicevents)
        PaseNormalised.paseNormalised(pathsets, ogpf_probabilities)._2

    @Benchmark
    def timeFT12_OGPF_PaDA_orderedSet(): Double =
        val pathsets = minimalcutpathset.minimalPathSets(ogpf_flattened)(ogpf_basicevents)
        PaseOrderedSet.paseOrderedSet(pathsets, ogpf_probabilities)._2

    // BHNGPipeline

    @Benchmark
    def timeFT13_BHNGPipeline_BUDA(): Double =
        faulttree.height7(bhngPipeline_flattened)

    @Benchmark
    def timeFT13_BHNGPipeline_CuDA(): Double =
        val cutsets = minimalcutpathset.minimalCutSets(bhngPipeline_dagtree)(bhngPipeline_basicevents)
        minimalcutpathset.algorithm4(cutsets, bhngPipeline_probabilities)._2

    @Benchmark
    def timeFT13_BHNGPipeline_CuDA_normalised(): Double =
        val cutsets = minimalcutpathset.minimalCutSets(bhngPipeline_dagtree)(bhngPipeline_basicevents)
        MinceNormalised.minceNormalised(cutsets, bhngPipeline_probabilities)._2

    @Benchmark
    def timeFT13_BHNGPipeline_CuDA_orderedSet(): Double =
        val cutsets = minimalcutpathset.minimalCutSets(bhngPipeline_dagtree)(bhngPipeline_basicevents)
        MinceOrderedSet.minceOrderedSet(cutsets, bhngPipeline_probabilities)._2

    @Benchmark
    def timeFT13_BHNGPipeline_PaDA(): Double =
        val pathsets = minimalcutpathset.minimalPathSets(bhngPipeline_dagtree)(bhngPipeline_basicevents)
        minimalcutpathset.algorithm5(pathsets, bhngPipeline_probabilities)._2

    @Benchmark
    def timeFT13_BHNGPipeline_PaDA_normalised(): Double =
        val pathsets = minimalcutpathset.minimalPathSets(bhngPipeline_dagtree)(bhngPipeline_basicevents)
        PaseNormalised.paseNormalised(pathsets, bhngPipeline_probabilities)._2

    @Benchmark
    def timeFT13_BHNGPipeline_PaDA_orderedSet(): Double =
        val pathsets = minimalcutpathset.minimalPathSets(bhngPipeline_dagtree)(bhngPipeline_basicevents)
        PaseOrderedSet.paseOrderedSet(pathsets, bhngPipeline_probabilities)._2

    // HSC

    @Benchmark
    def timeFT14_HSC_BUDA(): Double =
        faulttree.height7(hsc_flattened)

    @Benchmark
    def timeFT14_HSC_PaDA(): Double =
        val pathsets = minimalcutpathset.minimalPathSets(hsc_dagtree)(hsc_basicevents)
        minimalcutpathset.algorithm5(pathsets, hsc_probabilities)._2

    @Benchmark
    def timeFT14_HSC_PaDA_normalised(): Double =
        val pathsets = minimalcutpathset.minimalPathSets(hsc_dagtree)(hsc_basicevents)
        PaseNormalised.paseNormalised(pathsets, hsc_probabilities)._2

    @Benchmark
    def timeFT14_HSC_PaDA_orderedSet(): Double =
        val pathsets = minimalcutpathset.minimalPathSets(hsc_dagtree)(hsc_basicevents)
        PaseOrderedSet.paseOrderedSet(pathsets, hsc_probabilities)._2

}
