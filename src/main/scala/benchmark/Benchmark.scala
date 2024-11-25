package benchmark

import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Mode, OutputTimeUnit, Scope, Setup, State}

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.switch
import scala.collection.immutable.IntMap
import scala.compiletime.uninitialized

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
class RealWorldFaultTreesBenchmark {

    // Tree-like FaultTrees:

    private var aircraftRunwayExcursionAccidents_formula: decisiontree.BooleanFormula = uninitialized
    private var aircraftRunwayExcursionAccidents_flattened: faulttree.FaultTree = uninitialized
    private var aircraftRunwayExcursionAccidents_cutsets: minimalcutpathset.CutSets = uninitialized
    private var aircraftRunwayExcursionAccidents_pathsets: minimalcutpathset.PathSets = uninitialized
    private var aircraftRunwayExcursionAccidents_probabilities: IntMap[Double] = uninitialized

    private var mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_faulttree: faulttree.FaultTree = uninitialized
    private var mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_formula: decisiontree.BooleanFormula = uninitialized
    private var mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_flattened: faulttree.FaultTree = uninitialized
    private var mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_cutsets: minimalcutpathset.CutSets = uninitialized
    private var mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_pathsets: minimalcutpathset.PathSets = uninitialized
    private var mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_probabilities: IntMap[Double] = uninitialized

    private var atcFailsToResolveTheConflict_formula: decisiontree.BooleanFormula = uninitialized
    private var atcFailsToResolveTheConflict_flattened: faulttree.FaultTree = uninitialized
    private var atcFailsToResolveTheConflict_cutsets: minimalcutpathset.CutSets = uninitialized
    private var atcFailsToResolveTheConflict_pathsets: minimalcutpathset.PathSets = uninitialized
    private var atcFailsToResolveTheConflict_probabilities: IntMap[Double] = uninitialized

    private var liquidStorageTank_formula: decisiontree.BooleanFormula = uninitialized
    private var liquidStorageTank_flattened: faulttree.FaultTree = uninitialized
    private var liquidStorageTank_cutsets: minimalcutpathset.CutSets = uninitialized
    private var liquidStorageTank_pathsets: minimalcutpathset.PathSets = uninitialized
    private var liquidStorageTank_probabilities: IntMap[Double] = uninitialized

    private var lossContainerAtPort_formula: decisiontree.BooleanFormula = uninitialized
    private var lossContainerAtPort_flattened: faulttree.FaultTree = uninitialized
    private var lossContainerAtPort_cutsets: minimalcutpathset.CutSets = uninitialized
    private var lossContainerAtPort_pathsets: minimalcutpathset.PathSets = uninitialized
    private var lossContainerAtPort_probabilities: IntMap[Double] = uninitialized

    private var submarinePipelineStopperFailure_formula: decisiontree.BooleanFormula = uninitialized
    private var submarinePipelineStopperFailure_flattened: faulttree.FaultTree = uninitialized
    private var submarinePipelineStopperFailure_cutsets: minimalcutpathset.CutSets = uninitialized
    private var submarinePipelineStopperFailure_pathsets: minimalcutpathset.PathSets = uninitialized
    private var submarinePipelineStopperFailure_probabilities: IntMap[Double] = uninitialized

    private var bhngPipeline_formula: decisiontree.BooleanFormula = uninitialized
    private var bhngPipeline_flattened: faulttree.FaultTree = uninitialized
    private var bhngPipeline_cutsets: minimalcutpathset.CutSets = uninitialized
    private var bhngPipeline_pathsets: minimalcutpathset.PathSets = uninitialized
    private var bhngPipeline_probabilities: IntMap[Double] = uninitialized

    private var leakageFailure_formula: decisiontree.BooleanFormula = uninitialized
    private var leakageFailure_flattened: faulttree.FaultTree = uninitialized
    private var leakageFailure_cutsets: minimalcutpathset.CutSets = uninitialized
    private var leakageFailure_pathsets: minimalcutpathset.PathSets = uninitialized
    private var leakageFailure_probabilities: IntMap[Double] = uninitialized

    private var assessingTheRisks1_formula: decisiontree.BooleanFormula = uninitialized
    private var assessingTheRisks1_flattened: faulttree.FaultTree = uninitialized
    private var assessingTheRisks1_cutsets: minimalcutpathset.CutSets = uninitialized
    private var assessingTheRisks1_pathsets: minimalcutpathset.PathSets = uninitialized
    private var assessingTheRisks1_probabilities: IntMap[Double] = uninitialized

    private var pcba_formula: decisiontree.BooleanFormula = uninitialized
    private var pcba_flattened: faulttree.FaultTree = uninitialized
    private var pcba_cutsets: minimalcutpathset.CutSets = uninitialized
    private var pcba_pathsets: minimalcutpathset.PathSets = uninitialized
    private var pcba_probabilities: IntMap[Double] = uninitialized

    private var hsc_flattened: faulttree.FaultTree = uninitialized
    private var hsc_pathsets: minimalcutpathset.PathSets = uninitialized
    private var hsc_probabilities: IntMap[Double] = uninitialized

    // DAG-like FaultTrees:

    private var chlorineRelease_formula: decisiontree.BooleanFormula = uninitialized
    private var chlorineRelease_flattened: minimalcutpathset.FaultTree = uninitialized
    private var chlorineRelease_cutsets: minimalcutpathset.CutSets = uninitialized
    private var chlorineRelease_pathsets: minimalcutpathset.PathSets = uninitialized
    private var chlorineRelease_probabilities: IntMap[Double] = uninitialized

    private var t0Chopper_formula: decisiontree.BooleanFormula = uninitialized
    private var t0Chopper_flattened: minimalcutpathset.FaultTree = uninitialized
    private var t0Chopper_cutsets: minimalcutpathset.CutSets = uninitialized
    private var t0Chopper_pathsets: minimalcutpathset.PathSets = uninitialized
    private var t0Chopper_probabilities: IntMap[Double] = uninitialized

    private var ogpf_formula: decisiontree.BooleanFormula = uninitialized
    private var ogpf_flattened: minimalcutpathset.FaultTree = uninitialized
    private var ogpf_cutsets: minimalcutpathset.CutSets = uninitialized
    private var ogpf_pathsets: minimalcutpathset.PathSets = uninitialized
    private var ogpf_probabilities: IntMap[Double] = uninitialized


    @Setup
    def setup(): Unit = {
        {
            val aircraftRunwayExcursionAccidents_FT = reallife.AircraftRunwayExcursionAccidents.FT
            val (booleanFormula, probabilities) = Conversion.translateToBooleanFormula(aircraftRunwayExcursionAccidents_FT)
            aircraftRunwayExcursionAccidents_formula = booleanFormula
            aircraftRunwayExcursionAccidents_probabilities = probabilities
            val (dagTree, _) = Conversion.translateToDagTree(aircraftRunwayExcursionAccidents_FT)
            val basicEvents = minimalcutpathset.getBasicEvents(dagTree)
            aircraftRunwayExcursionAccidents_cutsets = minimalcutpathset.minimalCutSets(dagTree)(basicEvents)
            aircraftRunwayExcursionAccidents_pathsets = minimalcutpathset.minimalPathSets(dagTree)(basicEvents)
            aircraftRunwayExcursionAccidents_flattened = faulttree.flatten(aircraftRunwayExcursionAccidents_FT)
        }

        {
            mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_faulttree = reallife.MainTrackTrainCollisionsLeadingToFatalitiesAndInjuries.FT
            val (booleanFormula, probabilities) = Conversion.translateToBooleanFormula(mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_faulttree)
            mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_formula = booleanFormula
            mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_probabilities = probabilities
            val (dagTree, _) = Conversion.translateToDagTree(mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_faulttree)
            val basicEvents = minimalcutpathset.getBasicEvents(dagTree)
            mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_cutsets = minimalcutpathset.minimalCutSets(dagTree)(basicEvents)
            mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_pathsets = minimalcutpathset.minimalPathSets(dagTree)(basicEvents)
            mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_flattened = faulttree.flatten(mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_faulttree)
        }

        {
            val atcFailsToResolveTheConflict_FT = reallife.ATCFailsToResolveTheConflict.FT
            val (booleanFormula, probabilities) = Conversion.translateToBooleanFormula(atcFailsToResolveTheConflict_FT)
            atcFailsToResolveTheConflict_formula = booleanFormula
            atcFailsToResolveTheConflict_probabilities = probabilities
            val (dagTree, _) = Conversion.translateToDagTree(atcFailsToResolveTheConflict_FT)
            val basicEvents = minimalcutpathset.getBasicEvents(dagTree)
            atcFailsToResolveTheConflict_cutsets = minimalcutpathset.minimalCutSets(dagTree)(basicEvents)
            atcFailsToResolveTheConflict_pathsets = minimalcutpathset.minimalPathSets(dagTree)(basicEvents)
            atcFailsToResolveTheConflict_flattened = faulttree.flatten(atcFailsToResolveTheConflict_FT)
        }

        {
            val liquidStorageTank_FT = reallife.LiquidStorageTank.FT
            val (booleanFormula, probabilities) = Conversion.translateToBooleanFormula(liquidStorageTank_FT)
            liquidStorageTank_formula = booleanFormula
            liquidStorageTank_probabilities = probabilities
            val (dagTree, _) = Conversion.translateToDagTree(liquidStorageTank_FT)
            val basicEvents = minimalcutpathset.getBasicEvents(dagTree)
            liquidStorageTank_cutsets = minimalcutpathset.minimalCutSets(dagTree)(basicEvents)
            liquidStorageTank_pathsets = minimalcutpathset.minimalPathSets(dagTree)(basicEvents)
            liquidStorageTank_flattened = faulttree.flatten(liquidStorageTank_FT)
        }

        {
            val lossContainerAtPort_FT = reallife.LossContainerAtPort.FT
            val (booleanFormula, probabilities) = Conversion.translateToBooleanFormula(lossContainerAtPort_FT)
            lossContainerAtPort_formula = booleanFormula
            lossContainerAtPort_probabilities = probabilities
            val (dagTree, _) = Conversion.translateToDagTree(lossContainerAtPort_FT)
            val basicEvents = minimalcutpathset.getBasicEvents(dagTree)
            lossContainerAtPort_cutsets = minimalcutpathset.minimalCutSets(dagTree)(basicEvents)
            lossContainerAtPort_pathsets = minimalcutpathset.minimalPathSets(dagTree)(basicEvents)
            lossContainerAtPort_flattened = faulttree.flatten(lossContainerAtPort_FT)
        }

        {
            val submarinePipelineStopperFailure_FT = reallife.SubmarinePipelineStopperFailure.FT
            val (booleanFormula, probabilities) = Conversion.translateToBooleanFormula(submarinePipelineStopperFailure_FT)
            submarinePipelineStopperFailure_formula = booleanFormula
            submarinePipelineStopperFailure_probabilities = probabilities
            val (dagTree, _) = Conversion.translateToDagTree(submarinePipelineStopperFailure_FT)
            val basicEvents = minimalcutpathset.getBasicEvents(dagTree)
            submarinePipelineStopperFailure_cutsets = minimalcutpathset.minimalCutSets(dagTree)(basicEvents)
            submarinePipelineStopperFailure_pathsets = minimalcutpathset.minimalPathSets(dagTree)(basicEvents)
            submarinePipelineStopperFailure_flattened = faulttree.flatten(submarinePipelineStopperFailure_FT)
        }

        {
            val bhngPipeline_FT = reallife.BHNGPipeline.FT
            val (booleanFormula, probabilities) = Conversion.translateToBooleanFormula(bhngPipeline_FT)
            bhngPipeline_formula = booleanFormula
            bhngPipeline_probabilities = probabilities
            val (dagTree, _) = Conversion.translateToDagTree(bhngPipeline_FT)
            val basicEvents = minimalcutpathset.getBasicEvents(dagTree)
            bhngPipeline_cutsets = minimalcutpathset.minimalCutSets(dagTree)(basicEvents)
            bhngPipeline_pathsets = minimalcutpathset.minimalPathSets(dagTree)(basicEvents)
            bhngPipeline_flattened = faulttree.flatten(bhngPipeline_FT)
        }

        {
            val leakageFailure_FT = reallife.LeakageFailure.FT
            val (booleanFormula, probabilities) = Conversion.translateToBooleanFormula(leakageFailure_FT)
            leakageFailure_formula = booleanFormula
            leakageFailure_probabilities = probabilities
            val (dagTree, _) = Conversion.translateToDagTree(leakageFailure_FT)
            val basicEvents = minimalcutpathset.getBasicEvents(dagTree)
            leakageFailure_cutsets = minimalcutpathset.minimalCutSets(dagTree)(basicEvents)
            leakageFailure_pathsets = minimalcutpathset.minimalPathSets(dagTree)(basicEvents)
            leakageFailure_flattened = faulttree.flatten(leakageFailure_FT)
        }

        {
            val assessingTheRisks1_FT = reallife.AssessingTheRisks1.FT
            val (booleanFormula, probabilities) = Conversion.translateToBooleanFormula(assessingTheRisks1_FT)
            assessingTheRisks1_formula = booleanFormula
            assessingTheRisks1_probabilities = probabilities
            val (dagTree, _) = Conversion.translateToDagTree(assessingTheRisks1_FT)
            val basicEvents = minimalcutpathset.getBasicEvents(dagTree)
            assessingTheRisks1_cutsets = minimalcutpathset.minimalCutSets(dagTree)(basicEvents)
            assessingTheRisks1_pathsets = minimalcutpathset.minimalPathSets(dagTree)(basicEvents)
            assessingTheRisks1_flattened = faulttree.flatten(assessingTheRisks1_FT)
        }

        {
            val pcba_FT = reallife.PCBA.FT
            val (booleanFormula, probabilities) = Conversion.translateToBooleanFormula(pcba_FT)
            pcba_formula = booleanFormula
            pcba_probabilities = probabilities
            val (dagTree, _) = Conversion.translateToDagTree(pcba_FT)
            val basicEvents = minimalcutpathset.getBasicEvents(dagTree)
            pcba_cutsets = minimalcutpathset.minimalCutSets(dagTree)(basicEvents)
            pcba_pathsets = minimalcutpathset.minimalPathSets(dagTree)(basicEvents)
            pcba_flattened = faulttree.flatten(pcba_FT)
        }

        {
            val hsc_FT = reallife.HSC.FT
            val (dagTree, probabilities) = Conversion.translateToDagTree(hsc_FT)
            hsc_flattened = faulttree.flatten(hsc_FT)
            hsc_probabilities = probabilities
            val basicEvents = minimalcutpathset.getBasicEvents(dagTree)
            hsc_pathsets = minimalcutpathset.minimalPathSets(dagTree)(basicEvents)
        }

        {
            val chlorineRelease_FT = reallife.ChlorineRelease.FT
            val (booleanFormula, probabilities) = Conversion.translateToBooleanFormula(chlorineRelease_FT)
            chlorineRelease_formula = booleanFormula
            chlorineRelease_probabilities = probabilities
            val basicEvents = minimalcutpathset.getBasicEvents(chlorineRelease_FT)
            chlorineRelease_cutsets = minimalcutpathset.minimalCutSets(chlorineRelease_FT)(basicEvents)
            chlorineRelease_pathsets = minimalcutpathset.minimalPathSets(chlorineRelease_FT)(basicEvents)
            chlorineRelease_flattened = minimalcutpathset.flatten(chlorineRelease_FT)
        }

        {
            val t0Chopper_FT = reallife.T0Chopper.FT
            val (booleanFormula, probabilities) = Conversion.translateToBooleanFormula(t0Chopper_FT)
            t0Chopper_formula = booleanFormula
            t0Chopper_probabilities = probabilities
            val basicEvents = minimalcutpathset.getBasicEvents(t0Chopper_FT)
            t0Chopper_cutsets = minimalcutpathset.minimalCutSets(t0Chopper_FT)(basicEvents)
            t0Chopper_pathsets = minimalcutpathset.minimalPathSets(t0Chopper_FT)(basicEvents)
            t0Chopper_flattened = minimalcutpathset.flatten(t0Chopper_FT)
        }

        {
            val ogpf_FT = reallife.OGPF.FT
            val (booleanFormula, probabilities) = Conversion.translateToBooleanFormula(ogpf_FT)
            ogpf_formula = booleanFormula
            ogpf_probabilities = probabilities
            val basicEvents = minimalcutpathset.getBasicEvents(ogpf_FT)
            ogpf_cutsets = minimalcutpathset.minimalCutSets(ogpf_FT)(basicEvents)
            ogpf_pathsets = minimalcutpathset.minimalPathSets(ogpf_FT)(basicEvents)
            ogpf_flattened = minimalcutpathset.flatten(ogpf_FT)
        }
    }

    // MainTrackTrainCollisionsLeadingToFatalitiesAndInjuries

    @Benchmark
    def timeMainTrackTrainCollisionsLeadingToFatalitiesAndInjuries_eminent() =
        decisiontree.height(mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_formula, mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_probabilities)

    @Benchmark
    def timeMainTrackTrainCollisionsLeadingToFatalitiesAndInjuries_remind() =
        faulttree.height(mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_flattened)

    @Benchmark
    def timeMainTrackTrainCollisionsLeadingToFatalitiesAndInjuries_mince() =
        minimalcutpathset.algorithm4(mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_cutsets, mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_probabilities)

    @Benchmark
    def timeMainTrackTrainCollisionsLeadingToFatalitiesAndInjuries_pase() =
        minimalcutpathset.algorithm5(mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_pathsets, mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_probabilities)

    @Benchmark
    def timeMainTrackTrainCollisionsLeadingToFatalitiesAndInjuries_ranger() =
        decisiontree.RandomBDTs.algorithm13(mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_formula, mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_probabilities)

    // T0Chopper

    @Benchmark
    def timeT0Chopper_remind() =
        decisiontree.algorithm8(t0Chopper_flattened, t0Chopper_probabilities)

    @Benchmark
    def timeT0Chopper_mince() =
        minimalcutpathset.algorithm4(t0Chopper_cutsets, t0Chopper_probabilities)

    @Benchmark
    def timeT0Chopper_pase() =
        minimalcutpathset.algorithm5(t0Chopper_pathsets, t0Chopper_probabilities)

    @Benchmark
    def timeT0Chopper_ranger() =
        decisiontree.RandomBDTs.algorithm13(t0Chopper_formula, t0Chopper_probabilities)

    // ATCFailsToResolveTheConflict

    @Benchmark
    def timeATCFailsToResolveTheConflict_remind() =
        faulttree.height(atcFailsToResolveTheConflict_flattened)

    @Benchmark
    def timeATCFailsToResolveTheConflict_mince() =
        minimalcutpathset.algorithm4(atcFailsToResolveTheConflict_cutsets, atcFailsToResolveTheConflict_probabilities)

    @Benchmark
    def timeATCFailsToResolveTheConflict_pase() =
        minimalcutpathset.algorithm5(atcFailsToResolveTheConflict_pathsets, atcFailsToResolveTheConflict_probabilities)

    @Benchmark
    def timeATCFailsToResolveTheConflict_ranger() =
        decisiontree.RandomBDTs.algorithm13(atcFailsToResolveTheConflict_formula, atcFailsToResolveTheConflict_probabilities)

    // AircraftRunwayExcursionAccidents

    @Benchmark
    def timeAircraftRunwayExcursionAccidents_remind() =
        faulttree.height(aircraftRunwayExcursionAccidents_flattened)

    @Benchmark
    def timeAircraftRunwayExcursionAccidents_mince() =
        minimalcutpathset.algorithm4(aircraftRunwayExcursionAccidents_cutsets, aircraftRunwayExcursionAccidents_probabilities)

    @Benchmark
    def timeAircraftRunwayExcursionAccidents_pase() =
        minimalcutpathset.algorithm5(aircraftRunwayExcursionAccidents_pathsets, aircraftRunwayExcursionAccidents_probabilities)

    @Benchmark
    def timeAircraftRunwayExcursionAccidents_ranger() =
        decisiontree.RandomBDTs.algorithm13(aircraftRunwayExcursionAccidents_formula, aircraftRunwayExcursionAccidents_probabilities)

    // LiquidStorageTank

    @Benchmark
    def timeLiquidStorageTank_remind() =
        faulttree.height(liquidStorageTank_flattened)

    @Benchmark
    def timeLiquidStorageTank_mince() =
        minimalcutpathset.algorithm4(liquidStorageTank_cutsets, liquidStorageTank_probabilities)

    @Benchmark
    def timeLiquidStorageTank_pase() =
        minimalcutpathset.algorithm5(liquidStorageTank_pathsets, liquidStorageTank_probabilities)

    @Benchmark
    def timeLiquidStorageTank_ranger() =
        decisiontree.RandomBDTs.algorithm13(liquidStorageTank_formula, liquidStorageTank_probabilities)

    // LeakageFailure

    @Benchmark
    def timeLeakageFailure_remind() =
        faulttree.height(leakageFailure_flattened)

    @Benchmark
    def timeLeakageFailure_mince() =
        minimalcutpathset.algorithm4(leakageFailure_cutsets, leakageFailure_probabilities)

    @Benchmark
    def timeLeakageFailure_pase() =
        minimalcutpathset.algorithm5(leakageFailure_pathsets, leakageFailure_probabilities)

    @Benchmark
    def timeLeakageFailure_ranger() =
        decisiontree.RandomBDTs.algorithm13(leakageFailure_formula, leakageFailure_probabilities)

    // AssessingTheRisks1

    @Benchmark
    def timeAssessingTheRisks1_remind() =
        faulttree.height(assessingTheRisks1_flattened)

    @Benchmark
    def timeAssessingTheRisks1_mince() =
        minimalcutpathset.algorithm4(assessingTheRisks1_cutsets, assessingTheRisks1_probabilities)

    @Benchmark
    def timeAssessingTheRisks1_pase() =
        minimalcutpathset.algorithm5(assessingTheRisks1_pathsets, assessingTheRisks1_probabilities)

    @Benchmark
    def timeAssessingTheRisks1_ranger() =
        decisiontree.RandomBDTs.algorithm13(assessingTheRisks1_formula, assessingTheRisks1_probabilities)

    // ChlorineRelease

    @Benchmark
    def timeChlorineRelease_remind() =
        decisiontree.algorithm8(chlorineRelease_flattened, chlorineRelease_probabilities)

    @Benchmark
    def timeChlorineRelease_mince() =
        minimalcutpathset.algorithm4(chlorineRelease_cutsets, chlorineRelease_probabilities)

    @Benchmark
    def timeChlorineRelease_pase() =
        minimalcutpathset.algorithm5(chlorineRelease_pathsets, chlorineRelease_probabilities)

    @Benchmark
    def timeChlorineRelease_ranger() =
        decisiontree.RandomBDTs.algorithm13(chlorineRelease_formula, t0Chopper_probabilities)

    // LossContainerAtPort

    @Benchmark
    def timeLossContainerAtPort_remind() =
        faulttree.height(lossContainerAtPort_flattened)

    @Benchmark
    def timeLossContainerAtPort_mince() =
        minimalcutpathset.algorithm4(lossContainerAtPort_cutsets, lossContainerAtPort_probabilities)

    @Benchmark
    def timeLossContainerAtPort_pase() =
        minimalcutpathset.algorithm5(lossContainerAtPort_pathsets, lossContainerAtPort_probabilities)

    @Benchmark
    def timeLossContainerAtPort_ranger() =
        decisiontree.RandomBDTs.algorithm13(lossContainerAtPort_formula, lossContainerAtPort_probabilities)

    // SubmarinePipelineStopperFailure

    @Benchmark
    def timeSubmarinePipelineStopperFailure_remind() =
        faulttree.height(submarinePipelineStopperFailure_flattened)

    @Benchmark
    def timeSubmarinePipelineStopperFailure_mince() =
        minimalcutpathset.algorithm4(submarinePipelineStopperFailure_cutsets, submarinePipelineStopperFailure_probabilities)

    @Benchmark
    def timeSubmarinePipelineStopperFailure_pase() =
        minimalcutpathset.algorithm5(submarinePipelineStopperFailure_pathsets, submarinePipelineStopperFailure_probabilities)

    @Benchmark
    def timeSubmarinePipelineStopperFailure_ranger() =
        decisiontree.RandomBDTs.algorithm13(submarinePipelineStopperFailure_formula, submarinePipelineStopperFailure_probabilities)

    // PCBA

    @Benchmark
    def timePCBA_remind() =
        faulttree.height(pcba_flattened)

    @Benchmark
    def timePCBA_mince() =
        minimalcutpathset.algorithm4(pcba_cutsets, pcba_probabilities)

    @Benchmark
    def timePCBA_pase() =
        minimalcutpathset.algorithm5(pcba_pathsets, pcba_probabilities)

    @Benchmark
    def timePCBA_ranger() =
        decisiontree.RandomBDTs.algorithm13(pcba_formula, pcba_probabilities)

    // OGPF

    @Benchmark
    def timeOGPF_remind() =
        decisiontree.algorithm8(ogpf_flattened, ogpf_probabilities)

    @Benchmark
    def timeOGPF_mince() =
        minimalcutpathset.algorithm4(ogpf_cutsets, ogpf_probabilities)

    @Benchmark
    def timeOGPF_pase() =
        minimalcutpathset.algorithm5(ogpf_pathsets, ogpf_probabilities)

    @Benchmark
    def timeOGPF_ranger() =
        decisiontree.RandomBDTs.algorithm13(ogpf_formula, ogpf_probabilities)

    // BHNGPipeline

    @Benchmark
    def timeBHNGPipeline_remind() =
        faulttree.height(bhngPipeline_flattened)

    @Benchmark
    def timeBHNGPipeline_mince() =
        minimalcutpathset.algorithm4(bhngPipeline_cutsets, bhngPipeline_probabilities)

    @Benchmark
    def timeBHNGPipeline_pase() =
        minimalcutpathset.algorithm5(bhngPipeline_pathsets, bhngPipeline_probabilities)

    @Benchmark
    def timeBHNGPipeline_ranger() =
        decisiontree.RandomBDTs.algorithm13(bhngPipeline_formula, bhngPipeline_probabilities)

    // HSC

    @Benchmark
    def timeHSC_remind() =
        faulttree.height(hsc_flattened)

    @Benchmark
    def timeHSC_pase() =
        minimalcutpathset.algorithm5(hsc_pathsets, hsc_probabilities)

}







// TODO refactor/remove this.
object Setup {

    enum Branching extends java.lang.Enum[Branching]:
        case And, Or

    def other(branchType: Branching): Branching = (branchType: @switch) match
        case Branching.And => Branching.Or
        case Branching.Or => Branching.And

    type ChildIsBasicProbability = Double
    type BranchingWidth = Int
    type Depth = Int
    type Id = Int
    type Probability = Double
    type ProbabilityGen = Id => Probability

    case class Recipe(depth: Depth,
                      branching: Seq[Branching],
                      childIsBasicProbability: ChildIsBasicProbability,
                      branchingWidth: BranchingWidth,
                      probabilityOf: ProbabilityGen)

    val depth1 = fullTreeRecipe(1)
    val depth2 = fullTreeRecipe(2)
    val depth3 = fullTreeRecipe(3)
    val depth4 = fullTreeRecipe(4)
    val depth5 = fullTreeRecipe(5)

    def fullTreeRecipe(depth: Int): Recipe = Recipe(
        depth = depth,
        branching = Seq(Setup.Branching.And, Setup.Branching.Or),
        childIsBasicProbability = 0,
        branchingWidth = 2,
        probabilityOf = id => 1D / id
    )

    def makeFaultTree(recipe: Recipe): faulttree.FaultTree = {
        val idGen = new AtomicInteger()

        def makeFaultTree(recipe: Recipe): faulttree.FaultTree = {
            val id = idGen.getAndIncrement()

            if (recipe.depth == 1) {
                faulttree.FaultTree.BasicEvent(id, recipe.probabilityOf(id))
            } else {
                val branchType = recipe.branching(id % recipe.branching.size)
                val children = for
                    _ <- 0 until recipe.branchingWidth
                yield if Math.random() > recipe.childIsBasicProbability then
                    // child is and-gate or or-gate
                    makeFaultTree(recipe.copy(depth = recipe.depth - 1))
                else
                    // child is basic event
                    val childId = idGen.getAndIncrement()
                    faulttree.FaultTree.BasicEvent(childId, recipe.probabilityOf(childId))

                branchType match
                    case Branching.And => faulttree.FaultTree.AndEvent(id, children)
                    case Branching.Or => faulttree.FaultTree.OrEvent(id, children)
            }
        }

        makeFaultTree(recipe)
    }

    def makeDecisionTree(recipe: Recipe): (decisiontree.BooleanFormula, IntMap[Probability], (decisiontree.BooleanFormula, Id) => Boolean) = {
        val idGen = new AtomicInteger()
        val probabilitiesBuilder = new scala.collection.mutable.HashMap[Int, Probability]()

        def nextId(): Id = {
            val id = idGen.getAndIncrement()
            probabilitiesBuilder.addOne((id, recipe.probabilityOf(id)))
            id
        }

        def makeDecisionTree(recipe: Recipe): decisiontree.BooleanFormula = {
            val id = nextId()

            if recipe.depth == 1 then
                decisiontree.BooleanFormula.Variable(id)
            else
                val branchType = recipe.branching(id % recipe.branching.size)
                val children = for
                    _ <- 0 until recipe.branchingWidth
                yield if Math.random() > recipe.childIsBasicProbability then
                    makeDecisionTree(recipe.copy(depth = recipe.depth - 1)) // and-gate or or-gate
                else
                    decisiontree.BooleanFormula.Variable(nextId()) // basic event

                branchType match
                    case Branching.And => createBalancedAnd(children, nextId)
                    case Branching.Or => createBalancedOr(children, nextId)
            end if
        }

        val tree = makeDecisionTree(recipe)
        val probabilities = IntMap.from(probabilitiesBuilder)
        val variableLookup: (decisiontree.BooleanFormula, Id) => Boolean = decisiontree.computeLookupById()
        (tree, probabilities, variableLookup)
    }

    def createBalancedOr(children: Seq[decisiontree.BooleanFormula], nextId: () => Id): decisiontree.BooleanFormula = children match {
        case Seq(single) => single
        case _ =>
            val nodeCount = children.size

            val leftHalf = nodeCount / 2

            val (leftChildren, rightChildren) = children.splitAt(leftHalf)
            decisiontree.BooleanFormula.Or(createBalancedOr(leftChildren, nextId), createBalancedOr(rightChildren, nextId))
    }

    def createBalancedAnd(children: Seq[decisiontree.BooleanFormula], nextId: () => Id): decisiontree.BooleanFormula = children match {
        case Seq(single) => single
        case _ =>
            val nodeCount = children.size

            val leftHalf = nodeCount / 2

            val (leftChildren, rightChildren) = children.splitAt(leftHalf)
            decisiontree.BooleanFormula.And(createBalancedAnd(leftChildren, nextId), createBalancedAnd(rightChildren, nextId))
    }

    // main method to manually verify our generators indeed work the same way - the generated trees are 'the same'.
    def main(args: Array[String]): Unit = {
        //val recipe = depth4;
        val recipe = Recipe(
            depth = 4,
            branching = Seq(Branching.And, Branching.Or),
            childIsBasicProbability = 0,
            branchingWidth = 3,
            probabilityOf = id => 1D / id
        )

        val (decisionTree, probabilities, containsId) = makeDecisionTree(recipe)
        println(ppDecisionTree(decisionTree, probabilities))

        val faultTree = makeFaultTree(recipe)
        val layers = faulttree.layers(faultTree)
        println(ppFaultTree(faultTree))

        // calculate heights, they should be the same also.
//        println(decisiontree.height(decisionTree, probabilities, containsId))
//        println(faulttree.height(faultTree, layers))
    }

    def ppDecisionTree(tree: decisiontree.BooleanFormula, probabilities: IntMap[Probability]): String = tree match {
        case decisiontree.BooleanFormula.Variable(id) => s"Leaf(id=${id},prob=${probabilities(id)})"
        case decisiontree.BooleanFormula.Or(left, right) => s"Or(${ppDecisionTree(left, probabilities)},${ppDecisionTree(right, probabilities)})"
        case decisiontree.BooleanFormula.And(left, right) => s"And(${ppDecisionTree(left, probabilities)},${ppDecisionTree(right, probabilities)})"
    }

    def ppFaultTree(tree: faulttree.FaultTree): String = tree match {
        case faulttree.FaultTree.BasicEvent(id, prob) => s"Leaf(id=${id},prob=${prob})"
        case faulttree.FaultTree.OrEvent(id, children) => s"Or(${children.map(ppFaultTree).mkString(",")})"
        case faulttree.FaultTree.AndEvent(id, children) => s"And(${children.map(ppFaultTree).mkString(",")})"
    }
}