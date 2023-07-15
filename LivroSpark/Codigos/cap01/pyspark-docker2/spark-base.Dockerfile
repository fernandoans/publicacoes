FROM cluster-base

RUN apt-get update -y && \
    apt-get install -y curl && \
    curl https://archive.apache.org/dist/spark/spark-3.4.1/spark-3.4.1-bin-hadoop3.tgz -o spark.tgz && \
    tar -xf spark.tgz && \
    mv spark-3.4.1-bin-hadoop3 /usr/bin/ && \
    mkdir /usr/bin/spark-3.4.1-bin-hadoop3/logs && \
    rm spark.tgz

ENV SPARK_HOME /usr/bin/spark-3.4.1-bin-hadoop3
ENV SPARK_MASTER_HOST spark-master
ENV SPARK_MASTER_PORT 7077
ENV PYSPARK_PYTHON python3

WORKDIR ${SPARK_HOME}
