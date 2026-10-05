#!/usr/bin/env bash
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
# SCIPIO: Scipio Commerce (W1-02): run the server JVM directly (no Gradle), for the W0-04 and W1-02 tests.
#
#   bin/profile-java.sh load                       load seed, seed-initial, demo and ext data (embedded Derby, runtime/data)
#   bin/profile-java.sh test <component> <case>    run one OFBiz test case, for example: test commerce-profile hosted-profile-server-test
#   bin/profile-java.sh load-file <xml>            load one entity XML file
#   bin/profile-java.sh start                      start the server; web ports +300 (8380 http, 8743 https)
#   bin/profile-java.sh stop                       stop that server
#
# Env: JAVA (default: java), HEAP (default 3g), HOSTED (default true: -Dscipio.hosted), PORTOFFSET (default 300), READERS (load).
# Run it from the repository root after "gradlew build -x test syncLibs :applications:solr:syncSolrWebappLibs".
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/../../.." && pwd)"
JAVA="${JAVA:-java}"
HEAP="${HEAP:-3g}"
HOSTED="${HOSTED:-true}"
PORTOFFSET="${PORTOFFSET:-300}"

winpath() { if command -v cygpath >/dev/null; then cygpath -m "$1"; else echo "$1"; fi; }
SEP=":"; case "$(uname -s)" in MINGW*|MSYS*|CYGWIN*) SEP=";";; esac

CP="$(winpath "$ROOT/framework/start/build/lib/scipio-start.jar")$SEP$(winpath "$ROOT/framework/base/build/lib/scipio-base.jar")$SEP$(winpath "$ROOT/framework/base/config")"
while IFS= read -r jar; do CP="$CP$SEP$(winpath "$jar")"; done < <(find "$ROOT/framework/base/lib" -name '*.jar' | sort)

cmd="${1:-}"; shift || true
cd "$ROOT"
# The class path is long: pass it in an argument file (Windows and Git Bash refuse a long command line).
mkdir -p "$ROOT/runtime/tempfiles"
ARGS="$ROOT/runtime/tempfiles/profile-java.args"
{
  echo "-Xms512m"; echo "-Xmx$HEAP"; echo "-Dfile.encoding=UTF-8"; echo "-Dscipio.hosted=$HOSTED"
  for o in java.util java.lang java.lang.invoke java.net; do echo "--add-opens=java.base/$o=ALL-UNNAMED"; done
  echo "--add-opens=java.base/sun.util.calendar=ALL-UNNAMED"
  echo "-cp"; echo "\"$CP\""; echo "org.ofbiz.base.start.Start"
} > "$ARGS"
case "$cmd" in
  load)  exec "$JAVA" "@$(winpath "$ARGS")" load-data "readers=${READERS:-seed,seed-initial,demo,ext}" delegator=default ;;
  test)  exec "$JAVA" "@$(winpath "$ARGS")" test "-component=${1:?component}" "-case=${2:?case}" ;;
  load-file) exec "$JAVA" "@$(winpath "$ARGS")" load-data "-file=$(winpath "$(cd "$(dirname "${1:?xml file}")" && pwd)/$(basename "$1")")" delegator=default ;;
  stop)  exec "$JAVA" "@$(winpath "$ARGS")" -shutdown "--portoffset=$PORTOFFSET" ;;
  start) exec "$JAVA" "@$(winpath "$ARGS")" start "portoffset=$PORTOFFSET" ;;
  *) echo "usage: profile-java.sh load | load-file <xml> | test <component> <case> | start | stop" >&2; exit 2 ;;
esac
