<#-- 
SCIPIO: Common HTML macro library code

NOTE: 2016-10-05: Widget early HTML encoding is now DISABLED for all HTML macros.
    As a result all macros here must take care to html-escape as well as js-escape values.
    Use escapeVal/escapeFullUrl for this.
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

<#macro renderLabelCommon text="" id="" style="" extraArgs...>
  <#if text?has_content>
    <#-- If a label widget has one of the h1-h6 styles, then it is considered block level element.
         Otherwise it is considered an inline element. -->
    <#-- SCIPIO: also support "p" style to indicate paragraph, headings and special error classes -->
    <#local idText = ""/>
    <#if id?has_content><#local idText = " id=\"${escapeVal(id, 'html')}\""/></#if>
    <#if style?has_content>
      <#-- SCIPIO: can pass class and consumeLevel this way: "h2:class;consumeLevel=true" -->
      <#-- don't specify allowedElemTypes because we go through them ourselves below, redundant -->
      <#local headingArgs = getHeadingElemSpecFromStyleStr(rawString(style), "", "h|heading", true, "container|div", "widget-label")>
      <#local elemType = headingArgs.elemType> <#-- don't translate for macro; not passed; just for us -->
      <#local class = translateStyleStrClassesArg(headingArgs.elemClass!"")!"">
      <#if headingArgs.isHeadingElem>
        <#-- SCIPIO: heading labels -->
        <@heading level=translateStyleStrNumberArg(headingArgs.level!"")!"" relLevel=translateStyleStrNumberArg(headingArgs.relLevel!"")!"" 
            class=class id=id consumeLevel=translateStyleStrBoolArg(headingArgs.consumeLevel!"")!""
            containerElemType=translateStyleStrClassesArg(headingArgs.containerElemType!"")!false
            containerClass=translateStyleStrClassesArg(headingArgs.containerElemClass!"")!"" title=text/>
      <#else>
          <#-- SCIPIO: Because we didn't specify allowedElemTypes, class here may contain the elemType in some cases (when no ":", which is most cases). Make sure to remove. -->
          <#if !class?is_boolean && class?has_content>
            <#local class = removeStyleNames(rawString(class), elemType)>
          </#if>
          <#-- SCIPIO: Alias for div -->
          <#if elemType == "container">
            <#local elemType = "div">
          </#if>

          <#-- SCIPIO: generic text container types -->
          <#if elemType=="div" || elemType=="p" || elemType=="span">
            <${elemType}${idText}<@compiledClassAttribStr class=class />>${escapeVal(text, 'htmlmarkup')}</${elemType}>
          <#-- SCIPIO: common-message handling -->
          <#elseif elemType?starts_with("common-msg-")>
            <@commonMsg type=elemType["common-msg-"?length..] class=class id=id>${escapeVal(text, 'htmlmarkup')}</@commonMsg>
          <#elseif elemType=="generic">
            <#-- SCIPIO: special case to force stock generic case (in case default style string is not empty) -->
            <span${idText}<@compiledClassAttribStr class=class />>${escapeVal(text, 'htmlmarkup')}</span>
          <#else>
            <#-- SCIPIO: default markup and stock Ofbiz case (note: uses original style arg) -->
            <span${idText}<@compiledClassAttribStr class=style />>${escapeVal(text, 'htmlmarkup')}</span>
          </#if>
      </#if>
    <#else>
      <span${idText}>${escapeVal(text, 'htmlmarkup')}</span>
    </#if>
  </#if>
</#macro>
