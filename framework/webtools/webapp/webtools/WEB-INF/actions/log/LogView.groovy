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

import java.util.regex.Pattern

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

List logLines = [];
try {
    File logFile = FileUtil.getFile(logFileName);
    logFile.eachLine { line ->
        // SCIPIO: All of these checks modified to be more strict and precise
        // UPDATED 2018-15-18 for better parsing
        type = '';
        m = levelPat.matcher(line);
        if (m.find()) {
            type = levelMap[m.group(1)] ?: '';
        }
        logLines.add([type: type, line:line.trim()]);
    }
} catch (Exception exc) {}

context.logLines = logLines;
