package bdd

import dft.DFT

import java.io.{BufferedReader, File, FileReader}
import scala.collection.immutable.IntMap
import scala.io.Source

object BDDOrdering {

    type DftId = String
    type BddId = Int
    type InternalFaultTreeId = Int

    def extractBDDOrdering(file: File): Map[DftId, BddId] = {
        val map = Map.newBuilder[DftId, BddId]

        val reader = new BufferedReader(new FileReader(file))
        var line: String = null
        while {
            line = reader.readLine()
            line != null
        } do {
            line match
                case s"// ${basicevent_bdd_id} -> ${basicevent_dft_id}" =>
                    map.addOne(basicevent_dft_id -> basicevent_bdd_id.toInt)
                case _ =>
        }

        map.result()
    }

    def bddProbabilities(dft2InternalMapping: Map[DftId, InternalFaultTreeId],
                         internalProbabilities: IntMap[Double],
                         bddDotfile: File): IntMap[Double] = {
        val bddIds = extractBDDOrdering(bddDotfile)

        val result = IntMap.newBuilder[Double]
        for ((dftId, internalId) <- dft2InternalMapping) {
            internalProbabilities.get(internalId) match
                case Some(probability) =>
                    val bddId = bddIds(dftId)
                    result.addOne(bddId -> probability)
                case _ => // not a basic event
        }

        result.result()
    }

    def readBddProbabilities(dftGalileoFile: File, bddDotFile: File): IntMap[Double] = {
        val (dftLines, internalMapping) = DFT.readDFTFile(Source.fromFile(dftGalileoFile))
        val internalProbabilities: IntMap[Double] = DFT.getProbabilities(dftLines)
        bddProbabilities(internalMapping, internalProbabilities, bddDotFile)
    }
}
