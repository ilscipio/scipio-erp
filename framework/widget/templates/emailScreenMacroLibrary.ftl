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

<#macro renderScreenEnd extraArgs...></#macro>
<#macro renderSectionBegin boundaryComment extraArgs...></#macro>
<#macro renderSectionEnd boundaryComment extraArgs...></#macro>

<#macro renderLink parameterList targetWindow target uniqueItemName linkType actionUrl id style name height width linkUrl text imgStr extraArgs...>
        <a href="javascript:void(0);" id="${escapeVal(uniqueItemName, 'html')}_link" 
        <#if style?has_content>class="${escapeVal(style, 'html')}"</#if>>
        <#if text?has_content>${escapeVal(text, 'htmlmarkup')}</#if></a>
</#macro>


<#-- *********************
     *   htmlStructure   * 
     *********************
     -->
<#-- @row container markup - theme override -->
<#macro row_markup open=true close=true class="" collapse=false id="" style="" alt="" selected="" 
    attribs={} excludeAttribs=[] origArgs={} passArgs={} catchArgs...>
  <#if open>
    <tr<@compiledClassAttribStr class=class /><#if id?has_content> id="${escapeVal(id, 'html')}"</#if><#rt>
        <#lt><#if style?has_content> style="${escapeVal(style, 'html')}"</#if><#if attribs?has_content><@commonElemAttribStr attribs=attribs exclude=excludeAttribs/></#if>><#rt/>
  </#if>
      <#nested />
  <#if close>
    </tr>
  </#if> 
</#macro>


<#-- @cell container markup - theme override -->
<#macro cell_markup open=true close=true class="" id="" style="" last=false collapse=false 
    attribs={} excludeAttribs=[] origArgs={} passArgs={} catchArgs...>
  <#if open>
    <td<@compiledClassAttribStr class=class /><#if id?has_content> id="${escapeVal(id, 'html')}"</#if><#rt>
        <#lt><#if style?has_content> style="${escapeVal(style, 'html')}"</#if><#if attribs?has_content><@commonElemAttribStr attribs=attribs exclude=excludeAttribs/></#if>><#rt>
  </#if>
      <#nested><#t>
  <#if close>
    </td><#lt>
  </#if>
</#macro>

<#-- @section container markup - theme override 
    NOTE: class refers to cellClass (class for outer cell container) -->
<#macro section_markup_container type="" styleName="" open=true close=true sectionLevel=1 headingLevel=1 menuTitleContent="" menuTitleContentArgs={} menuContent="" menuContentArgs={} class="" 
    contentClass="" contentStyle="" contentFlagClasses="" id="" idPrefix="" title="" style="" collapsed=false contentId="" collapsible=false saveCollapsed=true 
    expandToolTip=true collapseToolTip=true padded=false showMore=true fullUrlString="" containerClass="" containerId="" containerStyle=""
    javaScriptEnabled=true fromScreenDef=false hasContent=true menuLayoutTitle="" menuLayoutGeneral="" menuRole="" requireMenu=false forceEmptyMenu=false 
    containerAttribs={} containerExcludeAttribs=[] contentAttribs={} contentExcludeAttribs=[]
    origArgs={} passArgs={} catchArgs...>
  <#if open>
    <#local containerClass = addClassArg(containerClass, "section-screenlet")>
    <#local containerClass = addClassArg(containerClass, styles.grid_section!"")>
    <#local containerClass = addClassArg(containerClass, contentFlagClasses)>
    <#if collapsed>
      <#local containerClass = addClassArg(containerClass, "toggleField")>
    </#if>
    <#-- NOTE: The ID should always be on the outermost container for @section -->
    <table<@compiledClassAttribStr class=containerClass /><#if containerId?has_content> id="${escapeVal(containerId, 'html')}"</#if><#rt>
        <#lt><#if style?has_content> style="${escapeVal(style, 'html')}"<#elseif containerStyle?has_content> style="${escapeVal(containerStyle, 'html')}"</#if><#rt>
        <#lt><#if containerAttribs?has_content><@commonElemAttribStr attribs=containerAttribs exclude=containerExcludeAttribs/></#if>>
      <#-- TODO?: Is this still needed? Nothing uses collapsed and title is already used below.
      <#if collapsed><p class="alert legend">[ <i class="${styles.icon!} ${styles.icon_arrow!}"></i> ] ${escapeVal(title, 'htmlmarkup')}</p></#if>
      -->
      <@row open=true close=false />
        <#local class = addClassArg(class, "section-screenlet-container")>
        <#local class = addClassArg(class, contentFlagClasses)>
        <#local class = addClassArgDefault(class, (styles.grid_large!"") + "12")>
        <#-- NOTE: this is same as calling class=("=" + compileClassArg(class)) to override non-essential @cell class defaults -->
        <@cell open=true close=false class=compileClassArg(class) />
          <#-- FIXME: This should not be prerendered like this, should be delegated, due to container heuristic issues and other -->
          <@contentArgRender content=menuTitleContent args=menuTitleContentArgs />
          <#-- NOTE: may need to keep this div free of foundation grid classes (for margins collapse?) -->
          <#local contentClass = addClassArg(contentClass, "section-screenlet-content")>
          <#local contentClass = addClassArg(contentClass, contentFlagClasses)>
          <div<#if contentId?has_content> id="${escapeVal(contentId, 'html')}"</#if><@compiledClassAttribStr class=contentClass /><#if contentStyle?has_content> style="${escapeVal(contentStyle, 'html')}"</#if><#rt>
          <#lt><#if contentAttribs?has_content><@commonElemAttribStr attribs=contentAttribs exclude=contentExcludeAttribs/></#if>>
  </#if>
            <#nested>
  <#if close>
          </div>
          
          <#if menuLayoutGeneral == "bottom" || menuLayoutGeneral == "top-bottom">
            <@contentArgRender content=menuContent args=menuContentArgs />
          </#if>
        <@cell close=true open=false />
      <@row close=true open=false />
    </table>
  </#if>
</#macro>


<#-- *********************
     *     Utilities     * 
     *********************
     -->
<#macro render resource="" name="" type="screen" ctxVars=false globalCtxVars=false reqAttribs=false clearValues="" restoreValues="" 
    asString=false shareScope="" maxDepth="" subMenus="">
  <@varSection ctxVars=ctxVars globalCtxVars=globalCtxVars reqAttribs=reqAttribs clearValues=clearValues restoreValues=restoreValues>
    <#-- assuming type=="screen" for now -->
    <#if type == "screen">
        ${StringUtil.wrapString(screens.renderScopedGen(resource, name, asString, shareScope))}<#t>
    <#elseif type == "section">
        ${StringUtil.wrapString(sections.renderScopedGen(name, asString, shareScope))}<#t>
    <#else>
      <#-- strip include- prefix from type, because for the rest it's all the same -->
      <#local type = type?replace("include-", "")>
      <#if !name?has_content>
        <#local parts = resource?split("#")>
        <#local resource = parts[0]>
        <#local name = (parts[1])!>
      </#if>
      <#-- DEV NOTE: WARN: name clashes -->
      <#if type == "menu">
        <#local dummy = setContextField("scipioWidgetWrapperArgs", {
          "resName":name, "resLocation":resource, "shareScope":shareScope, "maxDepth":maxDepth, "subMenus":subMenus
        })>
        ${StringUtil.wrapString(screens.render("component://common/widget/CommonScreens.xml", "scipioMenuWidgetWrapper", asString))}<#t>
      <#elseif type == "form">
        <#local dummy = setContextField("scipioWidgetWrapperArgs", {
          "resName":name, "resLocation":resource, "shareScope":shareScope
        })>
        ${StringUtil.wrapString(screens.render("component://common/widget/CommonScreens.xml", "scipioFormWidgetWrapper", asString))}<#t>
      <#elseif type == "tree">
        <#local dummy = setContextField("scipioWidgetWrapperArgs", {
          "resName":name, "resLocation":resource, "shareScope":shareScope
        })>
        ${StringUtil.wrapString(screens.render("component://common/widget/CommonScreens.xml", "scipioTreeWidgetWrapper", asString))}<#t>
      <#elseif type == "screen">
        <#local dummy = setContextField("scipioWidgetWrapperArgs", {
          "resName":name, "resLocation":resource, "shareScope":shareScope
        })>
        ${StringUtil.wrapString(screens.render("component://common/widget/CommonScreens.xml", "scipioScreenWidgetWrapper", asString))}<#t>
      </#if>
    </#if>
  </@varSection>
</#macro>