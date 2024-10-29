package benchmark

import java.util.random.RandomGenerator
import faulttree.FaultTree
import faulttree.FaultTree.*

import scala.collection.mutable.ListBuffer

enum NodeType:
    case Basic, And, Or

object NodeType:
    val Values: Seq[NodeType] = Seq(NodeType.Basic, NodeType.And, NodeType.Or)

def randomNodeType()(using random: RandomGenerator): NodeType =
    random.nextInt(3) match
        case 0 => NodeType.Basic
        case 1 => NodeType.And
        case 2 => NodeType.Or

def randomProbability()(using random: RandomGenerator): Double =
    random.nextDouble(Double.MinPositiveValue, 1D)

object RandomTrees {

    def makeRandomTree(basicEvents: Int)(using random: RandomGenerator): FaultTree = {
        var id = 0

        def nextId(): Int = {
            val oldId = id
            id += 1
            oldId
        }

        val basicEventsNodes = for _ <- 0 until basicEvents yield FaultTree.BasicEvent(nextId(), randomProbability())

        val remainingEvents = ListBuffer.from(basicEventsNodes)

        def makeNode(nodeType: NodeType, children: IterableOnce[FaultTree]): FaultTree = nodeType match
            case NodeType.And => FaultTree.AndEvent(nextId(), Seq.from(children))
            case NodeType.Or => FaultTree.OrEvent(nextId(), Seq.from(children))

        while remainingEvents.sizeIs > 1 do
            val groupAmount = 1 + random.nextInt(remainingEvents.size)
            val randomNodeType = if random.nextBoolean() then NodeType.And else NodeType.Or

            val childEvents = remainingEvents.take(groupAmount)
            remainingEvents.dropInPlace(groupAmount)

            val newNode = makeNode(randomNodeType, childEvents)
            remainingEvents.addOne(newNode)
        end while

        remainingEvents.head
    }

    def main(args: Array[String]): Unit = {
        given random: RandomGenerator = new java.util.Random()

        for (_ <- 0 until 10) {
            println(makeRandomTree(10))
        }
    }

}