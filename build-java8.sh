#!/bin/bash

JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64/

if [[ ! -d $JAVA_HOME ]]; then
    echo "java 8 needs to be installed"
    echo "$ sudo apt install openjdk-8-jdk"
    exit 1
fi

PATH=$JAVA_HOME/bin:$PATH
sbt clean
sbt compile
sbt assembly
