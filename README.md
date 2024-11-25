# Fault Tree height calculations

## IDE setup

Import this project into [IntelliJ IDEA](https://www.jetbrains.com/idea/) or [VSCode](https://code.visualstudio.com/).
For IntelliJ, ensure the [Scala plugin](https://plugins.jetbrains.com/plugin/1347-scala) is installed.
For VSCode, ensure the [Scala/Metals](https://scalameta.org/metals/docs/editors/vscode/) extension is installed.

Your IDE should now recognise this project as an [SBT](https://www.scala-sbt.org/) project.
Depending on your IDE/Editor setup, you should also install SBT itself.

## Project setup

- DecisionTree.scala: contains the exact height calculation for boolean formulae (Eminent).
- RecursiveAlgorithm2.scala: contains the 'recursive' height approximation algorithm for tree-like fault trees (Remind).
- BinaryDecisionTree.scala: contains the 'recursive' height approximation algorithm for dag-like fault trees (Remind).
- CutSetAlgorithm4.scala: contains the 'MCS-based' height approximation algorithm for dag-like fault trees (Mince).
- PathSetAlgorithm5.scala: contains the 'MPS-based' height approximation algorithm for dag-like fault trees (Pase).
- RandomBDTS.scala: contains the 'random binatry decision tree' height approximation algorithm for dag-like fault trees (Ranger).
- Benchmark.scala: contains a [JMH](https://openjdk.org/projects/code-tools/jmh/) benchmark for comparing the running times of all algorithms mentioned in the Paper.
- Conversion.scala: code which converts between Tree-Like FaultTree, Dag-Like FaultTree and BooleanFormula representations of fault trees.
- RealLife.scala: calculates height approximations for some real-world fault trees.
- DFT.scala: simple parser .dft files.

## Running the benchmarks

Set your JAVA_HOME environment variable to a JDK 23 installation.
<br>
Windows example: `set JAVA_HOME=C:\Program Files\Eclipse Adoptium\jdk-23.0.1+11-hotspot`
<br>
Linux example: `export JAVA_HOME=/opt/jdk23`

Execute `sbt jmh:run` from a terminal.

<!--- TODO do we want to keep RandomTrees and RandomDags? -->

## Random trees

To generate the plots, run RandomTrees#main. Use JVM arguments `--add-exports java.base/java.lang=ALL-UNNAMED --add-exports java.desktop/sun.awt=ALL-UNNAMED --add-exports java.desktop/sun.java2d=ALL-UNNAMED`.
