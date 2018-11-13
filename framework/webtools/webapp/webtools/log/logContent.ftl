<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->


<@code type="log"><#list logLines as logLine><div class="${logLine.type!}">${logLine.line}</div></#list><#t>
</@code>