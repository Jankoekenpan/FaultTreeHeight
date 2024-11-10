package benchmark

import minimalcutpathset.FaultTree

import java.nio.file.{Files, Path, StandardOpenOption}

object DagApproximationRatio {

    val jur = new java.util.Random()
    val outFile = "dag-ratios.csv"

    private def writeString(file: Path, string: String): Unit = {
        Files.writeString(file, string, StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.APPEND)
    }

    def main(args: Array[String]): Unit = {
        given randomGenerator: java.util.random.RandomGenerator = jur
        val file = Files.createFile(Path.of(outFile))
        printHeader(file)

        for (be <- 3 to 17) {
            println(s"# Basic events = ${be}")
            for (i <- 1 to 100) {
                val randomDagFT = RandomDags.makeRandomDag(be)
                printData(file, randomDagFT)
            }
        }
    }

    def printHeader(file: Path): Unit = {
        val line = "DAG-like FaultTree,Height (enumeration algorithm),Height (cut set algorithm),Height (path set algorithm),Height (binary decision tree algorithm),cutset algorithm approximation ratio,pathset algorithm approximation ratio,binary decision tree approximation ratio\r\n"
        print(line)
        writeString(file, line)
    }

    def printData(file: Path, dagLikeFaultTree: FaultTree): Unit = {
        println(s"dagTree = ${dagLikeFaultTree}")
        val (booleanFormula, probabilities) = Conversion.translateToDecisionTree(dagLikeFaultTree)
        println(s"boolean formula = ${booleanFormula}, probabilities = ${probabilities}")

        val basicEvents = minimalcutpathset.getBasicEvents(dagLikeFaultTree)
        val eventProbabilities = minimalcutpathset.getProbabilities(dagLikeFaultTree)(basicEvents)

        val heightEnumeration = decisiontree.height(booleanFormula, probabilities)
        val heightCutSet = minimalcutpathset.height4(dagLikeFaultTree, basicEvents, eventProbabilities)
        val heightPathSet = minimalcutpathset.height5(dagLikeFaultTree, basicEvents, eventProbabilities)
        val heightBDT = decisiontree.algorithm8(dagLikeFaultTree, eventProbabilities)._2

        val ratioCutSet = ratio(heightCutSet, heightEnumeration)
        val ratioPathSet = ratio(heightPathSet, heightEnumeration)
        val ratioBDT = ratio(heightBDT, heightEnumeration)

        val line = s""""${dagLikeFaultTree}","${heightEnumeration}","${heightCutSet}","${heightPathSet}","${heightBDT}","${ratioCutSet}","${ratioPathSet}","${ratioBDT}"\r\n"""
        println(line)
        writeString(file, line)
    }

    def ratio(approximation: Double, realValue: Double): Double = approximation / realValue
}

object ExampleDagApproximationRatio {

    def main(args: Array[String]): Unit = {
        val (bf, probs) = exampleBooleanFormula

        val heightReal = decisiontree.height(bf, probs)
        val heightApproximated = decisiontree.algorithm8(exampleDagFT)._2

        println(s"real: $heightReal, approximation: $heightApproximated, ratio: ${DagApproximationRatio.ratio(heightApproximated, heightReal)}")
        // seems to work, so what's the deal with the converted boolean formula? It may have a problem...
    }

    def exampleBooleanFormula: (decisiontree.BooleanFormula, Seq[Double]) = {
        import decisiontree.BooleanFormula

        val bf = BooleanFormula.And(
            BooleanFormula.Or(
                BooleanFormula.Variable(1),
                BooleanFormula.Variable(2)
            ),
            BooleanFormula.And(
                BooleanFormula.Variable(0),
                BooleanFormula.Variable(2)
            )
        )

        (bf, Seq(0.25427476643247793, 0.5024058683267354, 0.3844549497288837))
    }

    def exampleDagFT: minimalcutpathset.FaultTree = {
        import minimalcutpathset.FaultTree
        import minimalcutpathset.TreeNode
        import minimalcutpathset.Gate

        FaultTree(8, Map(
            0 -> TreeNode.BasicEvent(0, 0.25427476643247793),
            1 -> TreeNode.BasicEvent(1, 0.5024058683267354),
            2 -> TreeNode.BasicEvent(2, 0.3844549497288837),
            3 -> TreeNode.Combination(3, Gate.Or, Set(1, 2)),
            4 -> TreeNode.Combination(4, Gate.And, Set(0)),
            5 -> TreeNode.Combination(5, Gate.Or, Set(2)),
            6 -> TreeNode.Combination(6, Gate.And, Set(3, 4)),
            7 -> TreeNode.Combination(7, Gate.And, Set(0, 4, 5)),
            8 -> TreeNode.Combination(8, Gate.And, Set(5, 6, 7, 3, 4))
        ))
    }
}