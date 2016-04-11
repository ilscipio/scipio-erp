#!/bin/sh

# set OFBIZ's relevant paths
OFBIZ_HOME="$( cd -P "$( dirname "$0" )" && pwd )"

echo "Cato-Commerce: Stopping framework..."        
sh ant stop &
echo "Cato-Commerce: Framework successfully stopped."