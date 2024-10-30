package benchmark

import scala.collection.immutable.IntMap

object Conversion {

    def translateToDagTree(tree: faulttree.FaultTree): (minimalcutpathset.FaultTree, IntMap[Double]) = {
        val eventsBuilder = scala.collection.immutable.Map.newBuilder[Int, minimalcutpathset.TreeNode]
        val probabilities = IntMap.newBuilder[Double]

        def translateToTree(tree: faulttree.FaultTree): minimalcutpathset.TreeNode = {
            val treeNode = tree match
                case faulttree.FaultTree.BasicEvent(id, p) =>
                    probabilities.addOne(id, p)
                    minimalcutpathset.TreeNode.BasicEvent(id, p)
                case faulttree.FaultTree.AndEvent(id, children) =>
                    children.foreach(translateToTree)
                    minimalcutpathset.TreeNode.Combination(
                        id,
                        minimalcutpathset.Gate.And,
                        children.map(_.event).toSet
                    )
                case faulttree.FaultTree.OrEvent(id, children) =>
                    children.foreach(translateToTree)
                    minimalcutpathset.TreeNode.Combination(
                        id,
                        minimalcutpathset.Gate.Or,
                        children.map(_.event).toSet
                    )
            eventsBuilder.addOne(tree.event, treeNode)
            treeNode
        }

        translateToTree(tree)

        (minimalcutpathset.FaultTree(tree.event, eventsBuilder.result()), probabilities.result())
    }

    def translateToDecisionTree(faultTree: faulttree.FaultTree): (decisiontree.BooleanFormula, Seq[Double]) = {
        val probabilities = Seq.newBuilder[Double]
        var curId = 0;

        def nextId(): Int = {
            val id = curId
            curId += 1
            id
        }

        def matchTree(faultTree: faulttree.FaultTree): decisiontree.BooleanFormula = {
            faultTree match
                case faulttree.FaultTree.BasicEvent(_, p) =>
                    probabilities.addOne(p)
                    decisiontree.BooleanFormula.Variable(nextId())
                case faulttree.FaultTree.AndEvent(_, children) =>
                    Setup.createBalancedAnd(children.map(matchTree), nextId)
                case faulttree.FaultTree.OrEvent(_, children) =>
                    Setup.createBalancedOr(children.map(matchTree), nextId)
        }

        (matchTree(faultTree), probabilities.result())
    }

}
