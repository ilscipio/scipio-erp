<#-- 
SCIPIO: Common HTML macro library code

NOTE: 2016-10-05: Widget early HTML encoding is now DISABLED for all HTML macros.
    As a result all macros here must take care to html-escape as well as js-escape values.
    Use escapePart/escapeFullUrl for this.
-->

<#-- SCIPIO: This function imports all main namespace directives (macros and functions) into the current namespace
  if they don't already exist.
  NO LONGER NEEDED; now dumping scipio directives into global namespace instead - see scipioIncludes.ftl)
<#function importScipioUtilities>
  <#if !scipioUtilitiesDefined??>
    <#list .main?keys as varName>
      <#if .main[varName]?is_directive && !.vars[varName]??>
        <@"<#assign ${varName}=.main[varName]>"?interpret />
      </#if>
    </#list>
    <#assign scipioUtilitiesDefined = true>
  </#if>
  <#return "">
</#function>

<#assign dummy = importScipioUtilities()>
-->

<#macro renderLabelCommon text id style extraArgs...>
  <#if text?has_content>
    <#-- If a label widget has one of the h1-h6 styles, then it is considered block level element.
         Otherwise it is considered an inline element. -->
    <#-- Scipio: also support "p" style to indicate paragraph, headings and special error classes -->
    <#local idText = ""/>
    <#if id?has_content><#local idText = " id=\"${escapePart(id, 'html')}\""/></#if>
    <#if style?has_content>
      <#-- Scipio: can pass class and consumeLevel this way: "h2:class;consumeLevel=true" -->
      <#-- don't specify allowedElemTypes because we go through them ourselves below, redundant -->
      <#local headingArgs = getHeadingElemSpecFromStyleStr(rawString(style), "", "h|heading", true, "container|div", "widget-label")>
      <#local elemType = headingArgs.elemType> <#-- don't translate for macro; not passed; just for us -->
      <#local class = translateStyleStrClassesArg(headingArgs.elemClass!"")!"">
      <#if headingArgs.isHeadingElem>
        <#-- Scipio: heading labels -->
        <@heading level=translateStyleStrNumberArg(headingArgs.level!"")!"" relLevel=translateStyleStrNumberArg(headingArgs.relLevel!"")!"" 
            class=class id=id consumeLevel=translateStyleStrBoolArg(headingArgs.consumeLevel!"")!""
            containerElemType=translateStyleStrClassesArg(headingArgs.containerElemType!"")!false
            containerClass=translateStyleStrClassesArg(headingArgs.containerElemClass!"")!"" title=text/>
      <#else>
          <#-- Scipio: Because we didn't specify allowedElemTypes, class here may contain the elemType in some cases (when no ":", which is most cases). Make sure to remove. -->
          <#if !class?is_boolean && class?has_content>
            <#local class = removeStyleNames(rawString(class), elemType)>
          </#if>
          <#-- Scipio: Alias for div -->
          <#if elemType == "container">
            <#local elemType = "div">
          </#if>

          <#-- Scipio: generic text container types -->
          <#if elemType=="div" || elemType=="p" || elemType=="span">
            <${elemType}${idText}<@compiledClassAttribStr class=class />>${escapePart(text, 'html')}</${elemType}>
          <#-- Scipio: common-message handling -->
          <#elseif elemType?starts_with("common-msg-")>
            <@commonMsg type=elemType["common-msg-"?length..] class=class id=id>${escapePart(text, 'html')}</@commonMsg>
          <#elseif elemType=="generic">
            <#-- Scipio: special case to force stock generic case (in case default style string is not empty) -->
            <span${idText}<@compiledClassAttribStr class=class />>${escapePart(text, 'html')}</span>
          <#else>
            <#-- Scipio: default markup and stock Ofbiz case (note: uses original style arg) -->
            <span${idText}<@compiledClassAttribStr class=style />>${escapePart(text, 'html')}</span>
          </#if>
      </#if>
    <#else>
      <span${idText}>${escapePart(text, 'html')}</span>
    </#if>
  </#if>
</#macro>

