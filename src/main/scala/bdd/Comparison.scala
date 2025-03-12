package bdd

import benchmark.Conversion
import dft.DFT

import java.io.File
import scala.io.Source

object Comparison {

    def main(args: Array[String]): Unit = {

        readAndPrint("handcreated/ATC(FT3).dft")
        readAndPrint("generated/dft/ATCFailsToResolveTheConflict.dft")
        println()

    }

    private def readAndPrint(path: String): Unit = {
        println(readDFT(path))
    }

    private def readDFT(path: String): faulttree.FaultTree = {
        val source = Source.fromFile(new File(path))
        Conversion.translateToTreeLikeFaultTree(DFT.readDFTFile(source))
    }
}
