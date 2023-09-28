#!/bin/sh
#SBATCH --partition=bhuwan
#SBATCH --mem-per-cpu=128G
#SBATCH --ntasks=1
#SBATCH --gres=gpu:1

export JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64
export PATH=$JAVA_HOME/bin/:$PATH

sbt "extra/runMain ai.lum.odinson.extra.AnnotateText"

sbt "extra/runMain ai.lum.odinson.extra.IndexDocuments"
