<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#assign maxLogLines = UtilMisc.toIntegerObject(parameters.maxLogLines!1000)!1000>
<div id="log">
    <@code type="log" class="scrollable" style="word-break: break-all;"><#list logLines as logLine><div class="${logLine.type!}">${logLine.line}</div></#list><#t>
    </@code>
    <@field type="text" name="maxLogLines" value=maxLogLines label=uiLabelMap.CommonMaxLines
        events={"change":"setMaxLogLines($(this).val());", "keyup":"setMaxLogLines($(this).val());"}/>
</div>
<@script>
function isScrolledToBottom(el) {
    return ($(el).scrollTop() + $(el).innerHeight() >= $(el)[0].scrollHeight);
}
var maxLogLines = ${maxLogLines};
function setMaxLogLines(val) { val = parseInt(val); if (!isNaN(val)) { maxLogLines = val; console.log("setting maxLogLines: " + maxLogLines); } }

$(function(){
    var webSocket = new WebSocket('wss://' + window.location.host + '<@appUrl fullPath="false">/ws/log/subscribe</@appUrl>');

    webSocket.onopen = function(event) {
        var msg = {};
        webSocket.send(JSON.stringify(msg));
        $('#log pre').scrollTop($('#log pre').prop('scrollHeight'));
    };

    webSocket.onmessage = function(event) {
        var jsonObject, message,type;
        var atBottom = isScrolledToBottom($('#log pre'));
        jsonObject = JSON.parse(event.data);
        try {
            messageList = jsonObject.messageList;
            if (messageList) {
                var children = $('#log code').children();
                <#-- Math.max means: for now, never remove more than we're adding - TODO: REVIEW: may change later -->
                var numRemove = Math.max(messageList.length, (messageList.length + children.length) - maxLogLines);
                if (numRemove > messageList.length) {
                    numRemove = messageList.length;
                }
                var i;
                for(i = 0; i < numRemove; i++) {
                    children.get(i).remove();
                }
                $.each(messageList, function(i, e) {
                    var message = e.message;
                    var level = e.level;
                    if (message.length > 0) {
                        $('<div/>', {
                            class: level.standardLevel
                        }).html(message).appendTo('#log code');
                        if (atBottom) {
                            $('#log pre').scrollTop($('#log pre').prop('scrollHeight'));
                        }
                    }
                });
            }
         } catch (error) {
             console.log("error: " + error);
         }
    };
});
</@script>