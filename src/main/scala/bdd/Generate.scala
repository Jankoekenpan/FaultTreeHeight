package bdd

import benchmark.Conversion
import dft.{DFT, DFTNode}
import reallife.{ATCFailsToResolveTheConflict, AircraftRunwayExcursionAccidents, AssessingTheRisks1, BHNGPipeline, ChlorineRelease, DagLikeFaultTree, LeakageFailure, LiquidStorageTankFromDFT, LossContainerAtPort, MainTrackTrainCollisionsLeadingToFatalitiesAndInjuries, OGPF, PCBA, SimpleFaultTree, SubmarinePipelineStopperFailure, T0Chopper, TreeLikeFaultTree}

import java.io.File

object Generate {

    val trees: Seq[SimpleFaultTree] = Seq(
        AircraftRunwayExcursionAccidents,
        MainTrackTrainCollisionsLeadingToFatalitiesAndInjuries,
        ATCFailsToResolveTheConflict,
        LiquidStorageTankFromDFT,
        LossContainerAtPort,
        SubmarinePipelineStopperFailure,
        BHNGPipeline,
        LeakageFailure,
        AssessingTheRisks1,
        PCBA,

        ChlorineRelease,
        T0Chopper,
        OGPF,
    )

    def dftLines(sft: SimpleFaultTree): Seq[DFTNode] = sft match {
        case tlft: TreeLikeFaultTree => Conversion.translateToDFT(tlft.FT)
        case dlft: DagLikeFaultTree => Conversion.translateToDFT(dlft.FT)
    }

    def main(args: Array[String]): Unit = {
        val directory = "generated/dft/";

        for (tree <- trees) {
            val outputFile = new File(directory + tree.name + ".dft")
            DFT.writeDFTFile(outputFile, dftLines(tree))
        }
    }

}