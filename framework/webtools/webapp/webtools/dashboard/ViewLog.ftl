<@heading level=3>${uiLabelMap.WebtoolsLogs}</@heading>
<@code type="log" class="scrollable" style="word-break: break-all;"><#list logLines as logLine><div class="${logLine.type!}">${logLine.line}</div></#list><#t>
</@code>