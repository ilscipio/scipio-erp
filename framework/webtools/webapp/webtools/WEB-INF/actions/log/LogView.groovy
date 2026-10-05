/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
/*
 * Changes to this file: Copyright (C) Ilscipio GmbH. The changes are licensed
 * under the GNU Affero General Public License, version 3, or a commercial
 * license from Ilscipio GmbH (file LICENSE). The original code stays under
 * the Apache License, version 2.0, as stated above.
 */

import java.util.regex.Pattern
import org.apache.commons.io.input.ReversedLinesFileReader;
import org.ofbiz.base.util.FileUtil;

final levelMap = [
    'I':'INFO',
    'W':'WARN',
    'E':'ERROR',
    'D':'DEBUG',
    'T':'TRACE',
    'F':'FATAL',
    'A':'',
    'O':''
];
final levelPat = Pattern.compile(' |([A-Z])| ');

// SCIPIO: Debug logging to trace script execution
org.ofbiz.base.util.Debug.logInfo("LogView.groovy: Script starting, logFileName=" + logFileName, "LogView");

List logLines = [];
try {
    // SCIPIO: 2020-04-10 Added a reversed file reader and limitted the result so that only the last lines will be read. Improves page performance
    int n_lines = 200;
    File logFile = FileUtil.getFile(logFileName);
    org.ofbiz.base.util.Debug.logInfo("LogView.groovy: logFile=" + logFile + ", exists=" + logFile?.exists(), "LogView");
    ReversedLinesFileReader fr = new ReversedLinesFileReader(logFile);
    for(int i=0;i<n_lines;i++){
        String line=fr.readLine();
        if(line==null)
            break;
        // SCIPIO: All of these checks modified to be more strict and precise
        // UPDATED 2018-15-18 for better parsing
        type = '';
        m = levelPat.matcher(line);
        if (m.find()) {
            type = levelMap[m.group(1)] ?: '';
        }
        logLines.add([type: type, line:line.trim()]);
    }
    org.ofbiz.base.util.Debug.logInfo("LogView.groovy: Read " + logLines.size() + " lines", "LogView");
} catch (Exception exc) {
    org.ofbiz.base.util.Debug.logError(exc, "LogView.groovy: Error reading log file: " + exc.getMessage(), "LogView");
}

context.logLines = logLines.reverse();
