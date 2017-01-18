<#--
* 
* Master Email template include, standard Scipio markup
*
*   See component://common/webcommon/includes/scipio/lib/standard/htmlTemplate for details.
*
-->

<#-- 
*************************************
* EXTERNAL IMPORTS AND INCLUDES *
*************************************
* NOTE: Assumes utilities.ftl and htmlVariables.groovy included.
-->

<#-- (currently none) -->


<#-- 
*************************************
* API TEMPLATE MACROS AND INCLUDES *
*************************************
* Code this file is responsible for defining. Intended for use in production templates.
*
* DEV NOTES: 
* * Categories are loosely based on Foundation organization.
* * There may be circular dependencies between these includes. 
*   May not be avoidable without complicating further. 
-->

<#include "component://common/webcommon/includes/scipio/lib/standard/htmlScript.ftl">
<#include "component://common/webcommon/includes/scipio/lib/standard/htmlStructure.ftl">
<#include "component://common/webcommon/includes/scipio/lib/standard/htmlInfo.ftl">
<#include "component://common/webcommon/includes/scipio/lib/standard/htmlNav.ftl">
<#include "component://common/webcommon/includes/scipio/lib/standard/htmlContent.ftl">
<#include "component://common/webcommon/includes/scipio/lib/standard/htmlForm.ftl">

<#-- After everything included, create a copy of the namespace so that macros can access 
     their global variables without possibility of override (sometimes needed)
     NOTE: overriding themes will also make use of this. this variable name must be unique. -->
<#assign scipioStdTmplLib = copyObject(.namespace)>


<#-- 
*************************************
* OVERRIDES
*************************************
-->

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
    <table class="spacer"><tbody><tr><td height="16px" style="font-size:16px;line-height:16px;">&nbsp;</td></tr></tbody></table>
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