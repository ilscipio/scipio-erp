#!/bin/sh
# Scipio Commerce
# Copyright (C) Ilscipio GmbH
#
# This file is part of Scipio Commerce. Scipio Commerce is free software: you
# can redistribute it and modify it under the terms of the GNU Affero General
# Public License, version 3, as published by the Free Software Foundation.
# Scipio Commerce is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
# for more details. You should have received a copy of the license with this
# work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
# A commercial license is available from Ilscipio GmbH.
#
# SPDX-License-Identifier: AGPL-3.0-only
#####################################################################
# This file is subject to the terms and conditions defined in the\n
# files 'LICENSE' and 'NOTICE', which are part of this source\n
#code package.
#####################################################################

# set the parent directory as OFBiz Home
OFBIZ_HOME="$( cd -P "$( dirname "$0" )" && pwd )"

# console log file
OFBIZ_LOG=runtime/logs/console.log

# delete the last log
rm -f $OFBIZ_LOG

# VM args
#DEBUG="-Dsun.rmi.server.exceptionTrace=true"
#DEBUG="-Xdebug -Xnoagent -Djava.compiler=NONE -Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=8091"
#automatic IP address for linux
#IPADDR=`/sbin/ifconfig eth0 | grep 'inet addr:' | cut -d: -f2 | awk '{ print $1}'`
#RMIIF="-Djava.rmi.server.hostname=$IPADDR"
MEMIF="-Xms128M -Xmx3512M"
RNDFIX="-Djava.security.egd=file:///dev/./urandom"
#JMX="-Dcom.sun.management.jmxremote -Dcom.sun.management.jmxremote.port=33333 -Dcom.sun.management.jmxremote.ssl=false -Dcom.sun.management.jmxremote.authenticate=false"
#MISC="-Duser.language=en"
MISC="-Dsolr.solr.home=applications/solr/ -Dsolr.log.dir=runtime/logs/solr/ -Dsolr.lock.type=single"
VMARGS="$MEMIF $RNDFIX $MISC $JMX $DEBUG $RMIIF"

# Worldpay Config
#VMARGS="-Xbootclasspath/p:applications/accounting/lib/cryptix.jar $VMARGS"

# location of java executable
if [ -f "$JAVA_HOME/bin/java" ]; then
  JAVA="$JAVA_HOME/bin/java"
else
  JAVA=java
fi

# Allows to run from Jenkins. See http://wiki.jenkins-ci.org/display/JENKINS/ProcessTreeKiller. Cons: the calling Jenkins job does not terminate if the log is not enabled, pros: this allows to monitor the log in Jenkins
#BUILD_ID=dontKillMe

# start ofbiz
#$JAVA $VMARGS -jar ofbiz.jar $* >>$OFBIZ_LOG 2>>$OFBIZ_LOG&
(cd "$OFBIZ_HOME" && exec "$JAVA" $VMARGS -jar ofbiz.jar "$@")
