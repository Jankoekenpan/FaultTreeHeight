package reallife

import dft.DFT

import scala.io.Source

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

    val dagLikeFaultTree = Seq[DagLikeFaultTree](
        FT2_T0Chopper,
        FT8_ChlorineRelease,
        FT12_OGPF
    )

    val faultTrees: Seq[SimpleFaultTree] =
        treeLikeFaultTrees ++ dagLikeFaultTree

    def main(args: Array[String]): Unit = {
        for (tree <- faultTrees) {
            println(tree.name)
        }
    }

}

object FT1_AssessingTheRisks2 extends TreeLikeFaultTree {
    import faulttree.FaultTree

    val FT: FaultTree = DFT.readTreeLikeFaultTree(Source.fromFile("handcreated/AssessingtheRisks2(FT1).dft"))
}

object FT2_T0Chopper extends DagLikeFaultTree {
    import minimalcutpathset.FaultTree

    val FT: FaultTree = DFT.readDagLikeFaultTree(Source.fromFile("handcreated/T0 Chopper(FT2).dft"))
}

object FT3_ATC extends TreeLikeFaultTree {
    import faulttree.FaultTree

    val FT: FaultTree = DFT.readTreeLikeFaultTree(Source.fromFile("handcreated/ATC(FT3).dft"))
}

object FT4_Aircraft extends TreeLikeFaultTree {
    import faulttree.FaultTree

    val FT: FaultTree = DFT.readTreeLikeFaultTree(Source.fromFile("handcreated/Aircraft(FT4).dft"))
}

object FT5_LiquidStorageTank extends TreeLikeFaultTree {
    import faulttree.FaultTree

    val FT: FaultTree = DFT.readTreeLikeFaultTree(Source.fromFile("handcreated/LiquidStorageTank(FT5).dft"))
}

object FT6_LeakageFailure extends TreeLikeFaultTree {
    import faulttree.FaultTree

    val FT: FaultTree = DFT.readTreeLikeFaultTree(Source.fromFile("handcreated/Leakagefailure(FT6).dft"))
}

object FT7_AssessingTheRisks1 extends TreeLikeFaultTree {
    import faulttree.FaultTree

    val FT: FaultTree = DFT.readTreeLikeFaultTree(Source.fromFile("handcreated/AssessingtheRisks1(FT7).dft"))
}

object FT8_ChlorineRelease extends DagLikeFaultTree {
    import minimalcutpathset.FaultTree

    val FT: FaultTree = DFT.readDagLikeFaultTree(Source.fromFile("handcreated/Chlorine_release(FT8).dft"))
}

object FT9_LossContainerPort extends TreeLikeFaultTree {
    import faulttree.FaultTree

    val FT: FaultTree = DFT.readTreeLikeFaultTree(Source.fromFile("handcreated/loss_container_port(FT9).dft"))
}

object FT10_Stopper extends TreeLikeFaultTree {
    import faulttree.FaultTree

    val FT: FaultTree = DFT.readTreeLikeFaultTree(Source.fromFile("handcreated/stopper (FT10).dft"))
}

object FT11_PCBA extends TreeLikeFaultTree {
    import faulttree.FaultTree

    val FT: FaultTree = DFT.readTreeLikeFaultTree(Source.fromFile("handcreated/PCBA(FT11).dft"))
}

object FT12_OGPF extends DagLikeFaultTree {
    import minimalcutpathset.FaultTree

    val FT: FaultTree = DFT.readDagLikeFaultTree(Source.fromFile("handcreated/ogpf(FT12).dft"))
}

object FT13_BHNGPipeline extends TreeLikeFaultTree {
    import faulttree.FaultTree

    val FT: FaultTree = DFT.readTreeLikeFaultTree(Source.fromFile("handcreated/BHNGPipeline(FT13).dft"))
}

object FT14_HSC extends TreeLikeFaultTree {
    import faulttree.FaultTree

    val FT: FaultTree = DFT.readTreeLikeFaultTree(Source.fromFile("handcreated/HSC(FT14).dft"))
}
