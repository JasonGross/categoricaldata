#!/bin/sh

exec java -Xmx512M -XX:+UseConcMarkSweepGC -XX:MaxPermSize=128m ${SBT_OPTS} -jar $(dirname $0)/sbt-launch-0.10.1.jar "$@"

