<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->
<#if wikiContent?has_content>
    ${rawString(wikiContent)}
<#else>
    <@commonMsg type="error">${uiLabelMap.CommonHelpNotFound}</@commonMsg>
</#if>
