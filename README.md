# Fault Tree height calculations

## IDE setup

Import this project into [IntelliJ IDEA](https://www.jetbrains.com/idea/) or [VSCode](https://code.visualstudio.com/).
For IntelliJ, ensure the [Scala plugin](https://plugins.jetbrains.com/plugin/1347-scala) is installed.
For VSCode, ensure the [Scala/Metals](https://scalameta.org/metals/docs/editors/vscode/) extension is installed.

Your IDE should now recognise this project as an [SBT](https://www.scala-sbt.org/) project.

## Project setup

- DecisionTree.scala: contains the legacy height calculation for boolean formulae.
- FaultTree.scala: contains the new height calculation for tree-like fault trees.
- Benchmark.scala: contains a [JMH](https://openjdk.org/projects/code-tools/jmh/) benchmark for comparing the running times of both algorithms.
  There is also some auxiliary code for generating decision trees and fault trees.

## Running the benchmarks

Execute `sbt jmh:run` from a terminal.

## Approximation ratios

Execute ApproximationRatios#main in order to generate the approximation ratios csv file.
