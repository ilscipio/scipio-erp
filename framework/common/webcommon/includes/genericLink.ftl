<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#if requestParameters?? && genericLinkName?? && genericLinkTarget?? && genericLinkText??>
<form name="${escapeVal(genericLinkName, 'html')}"<#if genericLinkWindow??> target="${escapeVal(genericLinkWindow, 'html')}"</#if> action="${escapeVal(makePageUrl(genericLinkTarget), 'html')}" method="post">
<#if (!excludeParameters?? || excludeParameters != "N") && requestParameters??>
<#assign requestParameterKeys = requestParameters.keySet().iterator()>
<#list requestParameterKeys as requestParameterKey>
<#assign requestParameterValue = requestParameters.get(requestParameterKey)!>
<#if requestParameterValue?has_content>
<input type="hidden" name="${escapeVal(requestParameterKey, 'html')}" value="${escapeVal(requestParameterValue, 'html')}"/>
</#if>
</#list>
</#if>
<a href="javascript:document['${escapeVal(genericLinkName, 'js-html')}'].submit();"<#if genericLinkStyle??> class="${escapeVal(genericLinkStyle, 'html')}"</#if>>${escapeVal(genericLinkText, 'htmlmarkup')}</a>
</form>
</#if>
