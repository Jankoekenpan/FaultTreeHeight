# Use Ubuntu 22.04 as the base image
FROM ubuntu:22.04

# Set environment variables
ENV DEBIAN_FRONTEND=noninteractive \
    JAVA_VERSION=23 \
    JAVA_DISTRO=temurin \
    ARCHITECTURE=x64 \
    JAVA_HOME=/opt/java/openjdk \
	STORM_DIR=/opt/storm \
	SBT_DIR=/opt/sbt \
	FAULT_TREE_HEIGHT_DIR=/opt/FaultTreeHeight

# Install required tools
RUN apt-get update && apt-get install -y \
    wget \
    tar \
    gzip \
    ca-certificates \
	build-essential \
	git \
	cmake \
	libboost-all-dev \
	libcln-dev \
	libgmp-dev \
	libginac-dev \
	automake \
	libglpk-dev \
	libhwloc-dev \
	libz3-dev \
	libxerces-c-dev \
	libeigen3-dev \
    && rm -rf /var/lib/apt/lists/*

# Download and install Temurin JDK 23
RUN mkdir -p ${JAVA_HOME} && \
    wget -O /tmp/openjdk.tar.gz https://github.com/adoptium/temurin23-binaries/releases/download/jdk-23%2B37/OpenJDK23U-jdk_${ARCHITECTURE}_linux_hotspot_23_37.tar.gz && \
    tar -xzf /tmp/openjdk.tar.gz --strip-components=1 -C ${JAVA_HOME} && \
    rm /tmp/openjdk.tar.gz

# Set environment variables
ENV PATH="${JAVA_HOME}/bin:${PATH}"

# Verify installation
RUN java -version

# Install SBT
RUN mkdir -p ${SBT_DIR} && \
    wget -O /tmp/sbt.tar.gz https://github.com/sbt/sbt/releases/download/v1.10.1/sbt-1.10.1.tgz && \
	tar -xzf /tmp/sbt.tar.gz --strip-components=1 -C ${SBT_DIR} && \
	rm /tmp/sbt.tar.gz

# Set environment variables
ENV PATH="${SBT_DIR}/bin:${PATH}"

# Verify installation
RUN sbt --version

# Install Storm
RUN git clone -b feature/time-bdd-conversion https://github.com/Jankoekenpan/storm.git $STORM_DIR \
    && cd $STORM_DIR \
    && mkdir build \
    && cd build \
    && cmake .. \
    && make

# Install FaultTreeHeight
RUN git clone https://github.com/Jankoekenpan/FaultTreeHeight.git ${FAULT_TREE_HEIGHT_DIR}

RUN cd ${FAULT_TREE_HEIGHT_DIR} \
	&& sbt compile

# Default command (optional)
CMD ["bash"]