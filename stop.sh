#!/bin/sh

# set OFBIZ's relevant paths
OFBIZ_HOME="$( cd -P "$( dirname "$0" )" && pwd )"

echo "Scipio-Commerce: Stopping framework..."        
sh ant stop &
echo "Scipio-Commerce: Framework successfully stopped."