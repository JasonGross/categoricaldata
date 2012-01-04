#!/bin/sh

exec java -Xmx1024M -XX:+UseConcMarkSweepGC -XX:MaxPermSize=128m ${SBT_OPTS} -jar $(dirname $0)/sbt-launch-0.11.2.jar "$@"

