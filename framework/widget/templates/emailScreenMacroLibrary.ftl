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
<#include "htmlScreenMacroLibrary.ftl"> <#-- Defaults back to htmlScreenMacroLibrary -->
<#-- 
SCIPIO: NOTE: since macro renderer initial context mod, macros here now have access to a few widget context objects part of the initial
context, such as request, response, locale, and to some extent (since 2016-01-06), uiLabelMap.
WARN: no code run here or indirectly from here should assume full current context present. only use well-known generic vars.

NOTE: 2016-10-05: Widget early HTML encoding is now DISABLED for all HTML macros.
    As a result all macros here must take care to html-escape as well as js-escape values.
    Use escapeVal/escapeFullUrl for this.
-->
<#macro renderScreenBegin extraArgs...>
<#-- SCIPIO: NOTE: HTML head open is now in scipio template macros. 
     In OOTB ofbiz no context is passed here (locale, etc.) so did not belong here and cleaner if in scipio macros. -->
<!DOCTYPE html>
</#macro>

<#macro renderScreenEnd extraArgs...>
</#macro>

<#macro renderSectionBegin boundaryComment extraArgs...></#macro>

<#macro renderSectionEnd boundaryComment extraArgs...></#macro>

<#macro renderContainerBegin id style autoUpdateLink autoUpdateInterval extraArgs...>
  <#-- SCIPIO: now support a few more containers -->
  <#local elem = "">
  <#if ["div", "span", "p"]?seq_contains(style)>
    <#local elem = style>
    <#local style = "">
  <#elseif style?contains(":")>
    <#local parts = style?split(":")>
    <#local elem = parts[0]>
    <#local style = parts[1]>
  </#if>
  <#-- SCIPIO: delegate to scipio libs -->
  <@container open=true close=false class=style id=id elem=elem />
</#macro>

<#macro renderContainerEnd extraArgs...>
  <@container close=true open=false />
</#macro>

<#macro renderContentBegin editRequest enableEditValue editContainerStyle extraArgs...><#if editRequest?has_content && enableEditValue == "true"><div class="${escapeVal(editContainerStyle, 'html')}"></#if></#macro>

<#macro renderContentBody extraArgs...></#macro>

<#macro renderContentEnd urlString editMode editContainerStyle editRequest enableEditValue extraArgs...>

<#if editRequest?exists && enableEditValue == "true">
<#if urlString?exists><a href="${escapeFullUrl(urlString, 'html')}">${escapeVal(editMode, 'htmlmarkup')}</a><#rt/></#if>
<#if editContainerStyle?exists></div><#rt/></#if>
</#if>
</#macro>

<#macro renderSubContentBegin editContainerStyle editRequest enableEditValue extraArgs...><#if editRequest?exists && enableEditValue == "true"><div class="${escapeVal(editContainerStyle, 'html')}"></#if></#macro>

<#macro renderSubContentBody extraArgs...></#macro>

<#macro renderSubContentEnd urlString editMode editContainerStyle editRequest enableEditValue extraArgs...>
<#if editRequest?exists && enableEditValue == "true">
<#if urlString?exists><a href="${escapeFullUrl(urlString, 'html')}">${escapeVal(editMode, 'htmlmarkup')}</a><#rt/></#if>
<#if editContainerStyle?exists></div><#rt/></#if>
</#if>
</#macro>

<#macro renderHorizontalSeparator id style extraArgs...><hr<#if id?has_content> id="${escapeVal(id, 'html')}"</#if><#if style?has_content> class="${escapeVal(style, 'html')}"</#if>/></#macro>

<#macro renderLabel text id style extraArgs...>
  <@renderLabelCommon text=text id=id style=style />
</#macro>

<#macro renderLink parameterList targetWindow target uniqueItemName linkType actionUrl id style name height width linkUrl text imgStr extraArgs...>
        <#if "hidden-form" == linkType>
            <form method="post" action="${escapeFullUrl(actionUrl, 'html')}"<#if targetWindow?has_content> target="${escapeVal(targetWindow, 'html')}"</#if> onsubmit="javascript:submitFormDisableSubmits(this)" name="${escapeVal(uniqueItemName, 'html')}"><#rt/>
                <#list parameterList as parameter>
                <input name="${escapeVal(parameter.name, 'html')}" value="${escapeVal(parameter.value, 'html')}" type="hidden"/><#rt/>
                </#list>
            </form><#rt/>
        </#if>
        <a 
            <#if id?has_content>id="${escapeVal(id, 'html')}"</#if> 
            <#if style?has_content>class="${escapeVal(style, 'html')}"</#if> 
            <#if name?has_content>name="${escapeVal(name, 'html')}"</#if> 
            <#if targetWindow?has_content>target="${escapeVal(targetWindow, 'html')}"</#if> 
            <#-- FIXME: dangerous lookup -->
            href="<#if "hidden-form"==linkType>javascript:document['${escapeVal(uniqueItemName, 'js-html')}'].submit()<#else>${escapeFullUrl(linkUrl, 'html')}</#if>"><#rt/>
            <#if imgStr?has_content>${imgStr}</#if><#if text?has_content>${escapeVal(text, 'htmlmarkup')}</#if>
        </a>
</#macro>

<#macro renderImage src id style wid hgt border alt urlString extraArgs...>
<#if src?has_content>
<img<#if id?has_content> id="${escapeVal(id, 'html')}"</#if><#if style?has_content> class="${escapeVal(style, 'html')}"</#if><#if wid?has_content> width="${wid}"</#if><#if hgt?has_content> height="${hgt}"</#if><#if border?has_content> border="${escapeVal(border, 'html')}"</#if> alt="<#if alt?has_content>${escapeVal(alt, 'html')}</#if>" src="${escapeFullUrl(urlString, 'html')}" />
</#if>
</#macro>

<#macro renderContentFrame fullUrl width height border extraArgs...></#macro>

<#-- SCIPIO: new params: menuRole, titleStyle -->
<#macro renderScreenletBegin id="" title="" collapsible=false saveCollapsed=true collapsibleAreaId="" expandToolTip=true collapseToolTip=true fullUrlString="" padded=false menuString="" showMore=true collapsed=false javaScriptEnabled=true menuRole="" titleStyle="" extraArgs...>
    <#-- now delegates to Scipio implementation. -->
    <#-- NOTE (2016-09-09): We NO LONGER pass collapsibleAreaId - there is no explicit attribute in screen widgets, 
        and we should let @section_core assign a default so it is consistent system-wide: contentId=collapsibleAreaId -->
    <@section_core open=true close=false id=id title=title collapsible=collapsible saveCollapsed=saveCollapsed contentId="" expandToolTip=expandToolTip collapseToolTip=collapseToolTip fullUrlString=fullUrlString padded=padded menuContent=menuString 
        showMore=showMore collapsed=collapsed javaScriptEnabled=javaScriptEnabled fromScreenDef=true menuRole=menuRole requireMenu=false forceEmptyMenu=false hasContent=true titleStyle=titleStyle titleContainerStyle="" titleConsumeLevel=true 
        autoHeadingLevel=true headingLevel="" relHeadingLevel="" defaultHeadingLevel="" />
</#macro>

<#macro renderScreenletSubWidget extraArgs...></#macro>

<#macro renderScreenletEnd extraArgs...><@section_core close=true open=false /></#macro>

<#macro renderScreenletPaginateMenu lowIndex actualPageSize ofLabel listSize paginateLastStyle lastLinkUrl paginateLastLabel paginateNextStyle nextLinkUrl paginateNextLabel paginatePreviousStyle paginatePreviousLabel previousLinkUrl paginateFirstStyle paginateFirstLabel firstLinkUrl extraArgs...>
    <li class="${escapeVal(paginateFirstStyle, 'html')}<#if !firstLinkUrl?has_content> disabled</#if>"><#if firstLinkUrl?has_content><a href="${escapeFullUrl(firstLinkUrl, 'html')}" class="${styles.menu_section_item_link!}">${escapeVal(paginateFirstLabel, 'htmlmarkup')}</a><#else><a href="javascript:void(0);" class="disabled ${styles.menu_section_item_link!}">${escapeVal(paginateFirstLabel, 'htmlmarkup')}</a></#if></li>
    <li class="${escapeVal(paginatePreviousStyle, 'html')}<#if !previousLinkUrl?has_content> disabled</#if>"><#if previousLinkUrl?has_content><a href="${escapeFullUrl(previousLinkUrl, 'html')}" class="${styles.menu_section_item_link!}">${escapeVal(paginatePreviousLabel, 'htmlmarkup')}</a><#else><a href="javascript:void(0);" class="disabled ${styles.menu_section_item_link!}">${escapeVal(paginatePreviousLabel, 'htmlmarkup')}</a></#if></li>
    <#if (listSize?number > 0)><li><span class="text-entry">${lowIndex?number + 1} - ${lowIndex?number + actualPageSize?number} ${escapeVal(ofLabel, 'htmlmarkup')} ${listSize}</span></li><#rt/></#if>
    <li class="${escapeVal(paginateNextStyle, 'html')}<#if !nextLinkUrl?has_content> disabled</#if>"><#if nextLinkUrl?has_content><a href="${escapeFullUrl(nextLinkUrl, 'html')}" class="${styles.menu_section_item_link!}">${escapeVal(paginateNextLabel, 'htmlmarkup')}</a><#else><a href="javascript:void(0);" class="disabled ${styles.menu_section_item_link!}">${escapeVal(paginateNextLabel, 'htmlmarkup')}</a></#if></li>
    <li class="${escapeVal(paginateLastStyle, 'html')}<#if !lastLinkUrl?has_content> disabled</#if>"><#if lastLinkUrl?has_content><a href="${escapeFullUrl(lastLinkUrl, 'html')}" class="${styles.menu_section_item_link!}">${escapeVal(paginateLastLabel, 'htmlmarkup')}</a><#else><a href="javascript:void(0);" class="disabled ${styles.menu_section_item_link!}">${escapeVal(paginateLastLabel, 'htmlmarkup')}</a></#if></li>
</#macro>

<#macro renderColumnContainerBegin id style extraArgs...>
  <table cellspacing="0"<#if id?has_content> id="${escapeVal(id, 'html')}"</#if><#if style?has_content> class="${escapeVal(style, 'html')}"</#if>>
  <tr>
</#macro>

<#macro renderColumnContainerEnd extraArgs...>
  </tr>
  </table>
</#macro>

<#macro renderColumnBegin id style extraArgs...>
  <td<#if id?has_content> id="${escapeVal(id, 'html')}"</#if><#if style?has_content> class="${escapeVal(style, 'html')}"</#if>>
</#macro>

<#macro renderColumnEnd extraArgs...>
  </td>
</#macro>