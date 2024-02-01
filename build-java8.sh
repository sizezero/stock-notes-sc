#!/bin/bash

JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64/
PATH=$JAVA_HOME/bin:$PATH
sbt clean
sbt compile
sbt assembly
