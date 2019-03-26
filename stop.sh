#!/bin/sh
#####################################################################
# This file is subject to the terms and conditions defined in the\n
# files 'LICENSE' and 'NOTICE', which are part of this source\n
# code package.
#####################################################################

# location of java executable
if [ -f "$JAVA_HOME/bin/java" ]; then
  JAVA=$JAVA_HOME/bin/java
else
  JAVA=java
fi

$JAVA -jar ofbiz.jar -shutdown

