package benchmark

import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Mode, OutputTimeUnit, Scope, Setup, State}

import java.util.concurrent.TimeUnit
import scala.collection.immutable.IntMap
import scala.compiletime.uninitialized

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
class RealWorldFaultTreesBenchmark {

    private given randomGenerator: java.util.random.RandomGenerator = new java.util.Random()

    // Tree-like FaultTrees:

    private var aircraftRunwayExcursionAccidents_formula: decisiontree.BooleanFormula = uninitialized
    private var aircraftRunwayExcursionAccidents_flattened: faulttree.FaultTree = uninitialized
    private var aircraftRunwayExcursionAccidents_cutsets: minimalcutpathset.CutSets = uninitialized
    private var aircraftRunwayExcursionAccidents_pathsets: minimalcutpathset.PathSets = uninitialized
    private var aircraftRunwayExcursionAccidents_probabilities: IntMap[Double] = uninitialized
    private var aircraftRunwayExcursionAccidents_basicevents: Set[Int] = uninitialized

    private var mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_faulttree: faulttree.FaultTree = uninitialized
    private var mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_formula: decisiontree.BooleanFormula = uninitialized
    private var mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_flattened: faulttree.FaultTree = uninitialized
    private var mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_cutsets: minimalcutpathset.CutSets = uninitialized
    private var mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_pathsets: minimalcutpathset.PathSets = uninitialized
    private var mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_probabilities: IntMap[Double] = uninitialized
    private var mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_basicevents: Set[Int] = uninitialized

    private var atcFailsToResolveTheConflict_formula: decisiontree.BooleanFormula = uninitialized
    private var atcFailsToResolveTheConflict_flattened: faulttree.FaultTree = uninitialized
    private var atcFailsToResolveTheConflict_cutsets: minimalcutpathset.CutSets = uninitialized
    private var atcFailsToResolveTheConflict_pathsets: minimalcutpathset.PathSets = uninitialized
    private var atcFailsToResolveTheConflict_probabilities: IntMap[Double] = uninitialized
    private var atcFailsToResolveTheConflict_basicevents: Set[Int] = uninitialized

    private var liquidStorageTank_formula: decisiontree.BooleanFormula = uninitialized
    private var liquidStorageTank_flattened: faulttree.FaultTree = uninitialized
    private var liquidStorageTank_cutsets: minimalcutpathset.CutSets = uninitialized
    private var liquidStorageTank_pathsets: minimalcutpathset.PathSets = uninitialized
    private var liquidStorageTank_probabilities: IntMap[Double] = uninitialized
    private var liquidStorageTank_basicevents: Set[Int] = uninitialized

    private var lossContainerAtPort_formula: decisiontree.BooleanFormula = uninitialized
    private var lossContainerAtPort_flattened: faulttree.FaultTree = uninitialized
    private var lossContainerAtPort_cutsets: minimalcutpathset.CutSets = uninitialized
    private var lossContainerAtPort_pathsets: minimalcutpathset.PathSets = uninitialized
    private var lossContainerAtPort_probabilities: IntMap[Double] = uninitialized
    private var lossContainerAtPort_basicevents: Set[Int] = uninitialized

    private var submarinePipelineStopperFailure_formula: decisiontree.BooleanFormula = uninitialized
    private var submarinePipelineStopperFailure_flattened: faulttree.FaultTree = uninitialized
    private var submarinePipelineStopperFailure_cutsets: minimalcutpathset.CutSets = uninitialized
    private var submarinePipelineStopperFailure_pathsets: minimalcutpathset.PathSets = uninitialized
    private var submarinePipelineStopperFailure_probabilities: IntMap[Double] = uninitialized
    private var submarinePipelineStopperFailure_basicevents: Set[Int] = uninitialized

    private var bhngPipeline_formula: decisiontree.BooleanFormula = uninitialized
    private var bhngPipeline_flattened: faulttree.FaultTree = uninitialized
    private var bhngPipeline_cutsets: minimalcutpathset.CutSets = uninitialized
    private var bhngPipeline_pathsets: minimalcutpathset.PathSets = uninitialized
    private var bhngPipeline_probabilities: IntMap[Double] = uninitialized
    private var bhngPipeline_basicevents: Set[Int] = uninitialized

    private var leakageFailure_formula: decisiontree.BooleanFormula = uninitialized
    private var leakageFailure_flattened: faulttree.FaultTree = uninitialized
    private var leakageFailure_cutsets: minimalcutpathset.CutSets = uninitialized
    private var leakageFailure_pathsets: minimalcutpathset.PathSets = uninitialized
    private var leakageFailure_probabilities: IntMap[Double] = uninitialized
    private var leakageFailure_basicevents: Set[Int] = uninitialized

    private var assessingTheRisks1_formula: decisiontree.BooleanFormula = uninitialized
    private var assessingTheRisks1_flattened: faulttree.FaultTree = uninitialized
    private var assessingTheRisks1_cutsets: minimalcutpathset.CutSets = uninitialized
    private var assessingTheRisks1_pathsets: minimalcutpathset.PathSets = uninitialized
    private var assessingTheRisks1_probabilities: IntMap[Double] = uninitialized
    private var assessingTheRisks1_basicevents: Set[Int] = uninitialized

    private var pcba_formula: decisiontree.BooleanFormula = uninitialized
    private var pcba_flattened: faulttree.FaultTree = uninitialized
    private var pcba_cutsets: minimalcutpathset.CutSets = uninitialized
    private var pcba_pathsets: minimalcutpathset.PathSets = uninitialized
    private var pcba_probabilities: IntMap[Double] = uninitialized
    private var pcba_basicevents: Set[Int] = uninitialized

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
        faulttree.height7(mainTrackTrainCollisionLeadingToFatalitiesAndInjuries_flattened)

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
        faulttree.height7(atcFailsToResolveTheConflict_flattened)

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
        faulttree.height7(assessingTheRisks1_flattened)

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
        faulttree.height7(lossContainerAtPort_flattened)

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
