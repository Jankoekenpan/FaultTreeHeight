# Using the FaultTreeHeight docker image

## Obtaining the docker image:
### Option 1: Building the docker image yourself:
In a shell in the same directory as the Dockerfile:
```shell
docker build -t fault-tree-height -f fault-tree-height.Dockerfile .
```
Note: this will take a while since Storm needs to be compiled from source.
### Option 2: Load the docker image from the tar archive:
```shell
docker image load --input fault-tree-height.tar
```

## Running the docker image
```shell
docker run -it fault-tree-height
```

## Navigating the docker image:
- /opt/storm contains the Storm installation
- /opt/FaultTreeHeight contains the FaultTreeHeight Scala code

### Generating BDD files using storm-dft
```shell
cd ${STORM_DIR}
chmod +x conversion.sh
./conversion.sh AssessingtheRisks2\(FT1\)
./conversion.sh T0\ Chopper\(FT2\)
./conversion.sh ATC\(FT3\)
./conversion.sh Aircraft\(FT4\)
./conversion.sh LiquidStorageTank\(FT5\)
./conversion.sh Leakagefailure\(FT6\)
./conversion.sh AssessingtheRisks1\(FT7\)
./conversion.sh Chlorine_release\(FT8\)
./conversion.sh loss_container_port\(FT9\)
./conversion.sh stopper\ \(FT10\)
./conversion.sh PCBA\(FT11\)
./conversion.sh ogpf\(FT12\)
./conversion.sh BHNGPipeline\(FT13\)
./conversion.sh HSC\(FT14\)
```
This command will also output the FaultTree to BDD conversion time.

### Copying BDD files to the FaultTreeHeight repository
```shell
cd ${STORM_DIR}
cp ./bdd-files/AssessingtheRisks2\(FT1\).dot ${FAULT_TREE_HEIGHT_DIR}/generated/bdd/AssessingtheRisks2\(FT1\).dot
cp ./bdd-files/T0\ Chopper\(FT2\).dot ${FAULT_TREE_HEIGHT_DIR}/generated/bdd/T0\ Chopper\(FT2\).dot
cp ./bdd-files/ATC\(FT3\).dot ${FAULT_TREE_HEIGHT_DIR}/generated/bdd/ATC\(FT3\).dot
cp ./bdd-files/Aircraft\(FT4\).dot ${FAULT_TREE_HEIGHT_DIR}/generated/bdd/Aircraft\(FT4\).dot
cp ./bdd-files/LiquidStorageTank\(FT5\).dot ${FAULT_TREE_HEIGHT_DIR}/generated/bdd/LiquidStorageTank\(FT5\).dot
cp ./bdd-files/Leakagefailure\(FT6\).dot ${FAULT_TREE_HEIGHT_DIR}/generated/bdd/Leakagefailure\(FT6\).dot
cp ./bdd-files/AssessingtheRisks1\(FT7\).dot ${FAULT_TREE_HEIGHT_DIR}/generated/bdd/AssessingtheRisks1\(FT7\).dot
cp ./bdd-files/Chlorine_release\(FT8\).dot ${FAULT_TREE_HEIGHT_DIR}/generated/bdd/Chlorine_release\(FT8\).dot
cp ./bdd-files/loss_container_port\(FT9\).dot ${FAULT_TREE_HEIGHT_DIR}/generated/bdd/loss_container_port\(FT9\).dot
cp ./bdd-files/stopper\ \(FT10\).dot ${FAULT_TREE_HEIGHT_DIR}/generated/bdd/stopper\ \(FT10\).dot
cp ./bdd-files/PCBA\(FT11\).dot ${FAULT_TREE_HEIGHT_DIR}/generated/bdd/PCBA\(FT11\).dot
cp ./bdd-files/ogpf\(FT12\).dot ${FAULT_TREE_HEIGHT_DIR}/generated/bdd/ogpf\(FT12\).dot
cp ./bdd-files/BHNGPipeline\(FT13\).dot ${FAULT_TREE_HEIGHT_DIR}/generated/bdd/BHNGPipeline\(FT13\).dot
cp ./bdd-files/HSC\(FT14\).dot ${FAULT_TREE_HEIGHT_DIR}/generated/bdd/HSC\(FT14\).dot
```

### Run algorithms and gather height results
```shell
cd ${FAULT_TREE_HEIGHT_DIR}
sbt "runMain reallife.TreesInPaper"
```
The output will be written to '/opt/FaultTreeHeight/real-world-fault trees.csv'.
A cell will contain the value -1 when an algorithm did not run for a fault tree.

### Run the JMH benchmark to measure the execution time of the algorithms implemented in Scala
```shell
cd ${FAULT_TREE_HEIGHT_DIR}
sbt jmh:run
```
Note that this operation can take up to 10 hours.
To reduce the benchmark execution time, uncomment the @Fork annotation on the RealWorldFaultTreesBenchmark class in Benchmark.scala.
