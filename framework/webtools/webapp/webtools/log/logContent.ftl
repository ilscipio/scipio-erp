<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#assign maxLogLines = UtilMisc.toIntegerObject(parameters.maxLogLines!1000)!1000>
<#assign logRegexFilter = raw(parameters.logRegexFilter!)>
<div id="log">
    <@code type="log" class="scrollable" style="word-break: break-all;"><#list logLines as logLine><div class="${logLine.type!} log-entry">${escapeVal(logLine.line, 'htmlmarkup')}</div></#list><#t>
    </@code>
  <@form name="log-config" id="log-config">
    <@field type="text" name="maxLogLines" value=maxLogLines label=uiLabelMap.CommonMaxLines
        events={"change":"setMaxLogLines($(this).val());", "keyup":"setMaxLogLines($(this).val());"}/>
    <#-- TODO?: applies only to new incoming messages for now, because otherwise we'd have to keep a much bigger internal buffer -->
    <#assign logRegexFilterApplyCnt>
        <input type="button" value="${uiLabelMap.CommonApply}" onclick="javascript:setLogRegexFilter($('#logRegexFilter').val())" class="${styles.link_run_sys!} ${styles.action_update!}"/>
    </#assign>
    <@field type="text" name="logRegexFilter" id="logRegexFilter" value=(parameters.logRegexFilter!) label="Regex"
        postfix=true postfixContent=logRegexFilterApplyCnt postfixColumns=1/>
    <@field type="display" name="logViewLink" id="logViewLink" label=uiLabelMap.CommonLink/>
  </@form>
</div>
<style type="text/css"><#-- functionality -->
    .log-entry-visible { display:block; }
    .log-entry-hidden { display:none; }
</style>
<@script>
function isScrolledToBottom(el) {
    return ($(el).scrollTop() + $(el).innerHeight() >= $(el)[0].scrollHeight);
}
var maxLogLines = ${maxLogLines};
function setMaxLogLines(val) { val = parseInt(val); if (!isNaN(val)) { maxLogLines = val; updateLogLink(); } }

var logRegexFilter = null;
var logRegexFilterStr = "";
$(document).ready(function() {
    setLogRegexFilter("${escapeVal(logRegexFilter, 'js')}");
});
function setLogRegexFilter(val) {
    try {
        logRegexFilter = val ? new RegExp(val) : null;
    } catch(error) {
        alert('Invalid regular expression: ' + error); <#-- FIXME: alert -->
        return;
    }
    logRegexFilterStr = val;
    updateLogLink();
    applyLogRegex();
    return;
}
function applyLogRegex() {
    $('#log code .log-entry').each(function() {
        var $entry = $(this);
        $entry.removeClass('log-entry-visible');
        $entry.removeClass('log-entry-hidden');
        var message = $entry.text();
        if (checkLogRegexFilter(message)) {
            $entry.addClass('log-entry-visible');
        } else {
            $entry.addClass('log-entry-hidden');
        }
    });
}
function checkLogRegexFilter(line) {
    if (logRegexFilter == null) { return true; }
    return (line.search(logRegexFilter) >= 0);
}

function updateLogLink() {
    var pageUrl = "<@pageUrl uri="LogView" fullPath=true escapeAs="js"/>";
    pageUrl += "?maxLogLines=" + maxLogLines;
    if (logRegexFilterStr) {
        pageUrl += "&logRegexFilter=" + encodeURI(logRegexFilterStr);
    }
    $('#logViewLink').html('<a href="'+pageUrl+'">'+pageUrl+'</a>');
}

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
                    var level = e.level;
                    var message = "[" + level.standardLevel + "]|" + e.message; <#-- if you don't append, the initial load is different from the added lines -->
                    if (message.length > 0) {
                        $('<div/>', {
                            class: level.standardLevel + " log-entry " + (checkLogRegexFilter(message) ? "log-entry-visible" : "log-entry-hidden")
                        }).text(message).appendTo('#log code'); <#-- DEV NOTE: use text() instead of html() for security reasons -->
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