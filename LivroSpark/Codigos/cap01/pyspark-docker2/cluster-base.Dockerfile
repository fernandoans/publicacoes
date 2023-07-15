FROM openjdk:8-jre-slim

RUN mkdir -p /opt/workspace && \
    apt-get update -y && \
    apt-get install -y python3 && \
    ln -s /usr/bin/python3 /usr/bin/python && \
    rm -rf /var/lib/apt/lists/*

ENV SHARED_WORKSPACE=/opt/workspace

VOLUME /opt/workspace
CMD ["bash"]
