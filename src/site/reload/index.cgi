#!/bin/bash
echo Content-type: text/plain
echo ""

cd $0
/usr/local/bin/hg pull 2>&1
/usr/local/bin/hg update 2>&1
