FROM ubuntu:18.04
RUN apt-get update
RUN apt-get install -y build-essential curl
RUN apt-get install -y maven openjdk-8-jdk
RUN apt-get install -y z3
RUN curl -sSL https://get.haskellstack.org/ | sh