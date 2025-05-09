# Fault Tree height calculations

## IDE setup

Import this project into [IntelliJ IDEA](https://www.jetbrains.com/idea/) or [VSCode](https://code.visualstudio.com/).
For IntelliJ, ensure the [Scala plugin](https://plugins.jetbrains.com/plugin/1347-scala) is installed.
For VSCode, ensure the [Scala/Metals](https://scalameta.org/metals/docs/editors/vscode/) extension is installed.

Your IDE should now recognise this project as an [SBT](https://www.scala-sbt.org/) project.
Depending on your IDE/Editor setup, you should also install SBT itself on your operating system.

## Project setup

- DecisionTree.scala: contains the exact height calculation for boolean formulae (Eminent/EDA).
- RecursiveAlgorithm2.scala: contains the 'recursive' height approximation algorithm for tree-like fault trees (Remind/BUDA).
- BinaryDecisionTree.scala: contains the 'recursive' height approximation algorithm for dag-like fault trees (Remind/BUDA).
- CutSetAlgorithm4.scala: contains the 'MCS-based' height approximation algorithm for dag-like fault trees (Mince/CuDA).
  - MinceOrderedSet.scala: contains the 'size' variation of the CuDA algorithm.
- PathSetAlgorithm5.scala: contains the 'MPS-based' height approximation algorithm for dag-like fault trees (Pase/PaDA).
  - PaseOrderedSet.scala: contains the 'size' variation of the PaDA algorithm.
- RandomBDTs.scala: contains the 'random binatry decision tree' height approximation algorithm for dag-like fault trees (Ranger).
- Benchmark.scala: contains a [JMH](https://openjdk.org/projects/code-tools/jmh/) benchmark for comparing the running times of all algorithms mentioned in the paper.
- Conversion.scala: code which converts between Tree-Like FaultTree, Dag-Like FaultTree and BooleanFormula representations of fault trees.
- TreesInPaper.scala: calculates height approximations for some real-world fault trees.
- DFT.scala: simple parser and printer for galileo-formatted .dft files.

## Running the benchmarks

Set your JAVA_HOME environment variable to a JDK 23 installation.
<br>
Windows example: `set JAVA_HOME=C:\Program Files\Eclipse Adoptium\jdk-23.0.1+11-hotspot`
<br>
Linux example: `export JAVA_HOME=/opt/jdk23`

Execute `sbt jmh:run` from a terminal.
<br>
Note that this operation can take up to 10 hours.
To reduce the benchmark execution time, uncomment the @Fork annotation on the RealWorldFaultTreesBenchmark class in Benchmark.scala.
