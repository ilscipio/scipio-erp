<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#if wikiContent?has_content>
    ${rawString(wikiContent)}
<#else>
    <@commonMsg type="error">${uiLabelMap.CommonHelpNotFound}</@commonMsg>
</#if>
