<#--
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements.  See the NOTICE file
distributed with this work for additional information
regarding copyright ownership.  The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License.  You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
-->
<#-- SCIPIO: 2018-08-28: fixed this whole script, stock version many problems -->
<@script>
    jQuery(document).ready(function() {
        var date = null;

        var clock = function() {
            var clockFieldElem = jQuery("#${escapeVal(clockField, 'js')}");
            var clockFieldText = clockFieldElem.text();
            if (!clockFieldText || clockFieldText === '-' || clockFieldText === '...') {
                var serverRes = getServiceResult("getServerTimestampAsString",
                    "dateTimeFormat", "yyyy-MM-dd HH:mm:ss", "useServerTz", "true");
                if (serverRes && serverRes.serverTimestamp) {
                    date = moment(serverRes.serverTimestamp, "YYYY-MM-DD HH:mm:ss");
                }
            } else if (date != null) {
                date.add(1, 'second');
            }
            if (date != null && date.isValid()) {
                clockFieldElem.text(date.format("YYYY-MM-DD HH:mm:ss"));
            }
        }
        
        window.setInterval(function () {
            clock();
        }, 1000);
    });
</@script>