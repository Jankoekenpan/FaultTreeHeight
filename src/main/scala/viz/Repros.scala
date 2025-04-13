package viz

import bdd.{BDD, BDDOrdering}
import benchmark.Conversion
import dft.DFT
import reallife.{FT4_Aircraft, FT6_LeakageFailure}

import java.io.File
import scala.io.Source

object FT6Repro {

    val beNames: Map[Int, String] = Map(
        1 -> "Not_removing_the_corrosion_gas_and_impurities",
        2 -> "Not_adding_corrosion_inhibitor",
        3 -> "Not_pigging_regularly",
        4 -> "Anticorrosive_coating_failure",
        5 -> "Cathodic_protection_failure",
        6 -> "Dropped_objects_hit",
        7 -> "Anchoring_work",
        8 -> "Fishing_gear_interaction",
        9 -> "Offshore_construction",
        10 -> "Design_burial_depth_is_not_enough",
        11 -> "Operation_of_burial_depth_is_not_enough",
        12 -> "Failure_of_treatment_timely",
        13 -> "Strong_current_and_wave",
        14 -> "Seabed_soil_are_eroded_easily",
        15 -> "Subsea_earthquake",
        16 -> "Seabed_movement",
        17 -> "Typhoon",
        18 -> "Design_defect_of_material",
        19 -> "Construction_defect_of_material",
        20 -> "Design_defect_of_weld-seam",
        21 -> "Construction_defect_of_weld-seam",
        22 -> "Auxiliaries_aging",
        23 -> "Design_defect_of_auxiliaries"
    )

    val ordering1 = Seq(14, 16, 22, 2, 1, 18, 19, 10, 21, 4, 5, 8, 20, 3, 7, 12, 6, 11, 17, 9, 13, 23, 15)
    val ordering2 = Seq(16, 22, 2, 1, 18, 19, 10, 14, 21, 4, 5, 8, 20, 3, 7, 12, 13, 6, 11, 17, 9, 23, 15)

    def main(args: Array[String]): Unit = {
//        println(printOrderingStrings(orderingBEs(ordering1)))
//        println(printOrderingStrings(orderingBEs(ordering2)))

        val treeLikeFT = FT6_LeakageFailure
        val treeFT = treeLikeFT.FT
        val (dagTree, probabilities) = Conversion.translateToDagTree(treeFT)
        val dftFile = new File(s"handcreated/${treeLikeFT.name}.dft")
        val (_, dft2InternalMapping) = DFT.readDFTFile(Source.fromFile(dftFile))

        val pathSets = minimalcutpathset.minimalPathSets(dagTree)().toSeq
        println(pathSets)
        println(pathSets(0).diff(pathSets(1)))
        println(pathSets(1).diff(pathSets(0)))

        val bddFile1 = new File(s"generated/bdd/ft6.1.dot")
        val bdd1 = BDD.readStormSylvanBDDDotFile(bddFile1)
        val bddProbabilities1 = BDDOrdering.bddProbabilities(dft2InternalMapping, probabilities, bddFile1)

        val bddFile2 = new File(s"generated/bdd/ft6.2.dot")
        val bdd2 = BDD.readStormSylvanBDDDotFile(bddFile2)
        val bddProbabilities2 = BDDOrdering.bddProbabilities(dft2InternalMapping, probabilities, bddFile2)

        val height1 = BDD.height(bdd1, bddProbabilities1)
        val height2 = BDD.height(bdd2, bddProbabilities2)

        println(height1)
        println(height2)
    }

    def orderingBEs(eventIds: Seq[Int]): Seq[String] =
        eventIds.map(beNames)

    def printOrderingStrings(ordering: Seq[String]): String =
        ordering.mkString(" ")
}

object FT4Repro {

    def main(args: Array[String]): Unit = {
        val treeLikeFT = FT4_Aircraft
        val treeFT = treeLikeFT.FT
        val (dagTree, probabilities) = Conversion.translateToDagTree(treeFT)
        val dftFile = new File(s"handcreated/${treeLikeFT.name}.dft")
        val (_, dft2InternalMapping) = DFT.readDFTFile(Source.fromFile(dftFile))

        val cutSets = minimalcutpathset.minimalCutSets(dagTree)()
        println(cutSets.size)
        println(cutSets)
    }

}