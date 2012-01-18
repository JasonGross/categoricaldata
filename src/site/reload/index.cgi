#!/bin/bash
echo Content-type: text/plain
echo ""

cd $0
hg pull 2>&1
hg update 2>&1
hg merge 2>&1
