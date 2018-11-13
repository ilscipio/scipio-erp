<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
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