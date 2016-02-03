<#-- 
Cato: Common HTML macro library code
-->

<#-- Cato: This function imports all main namespace directives (macros and functions) into the current namespace
  if they don't already exist.
  NO LONGER NEEDED; now dumping cato directives into global namespace instead - see catoIncludes.ftl)
<#function importCatoUtilities>
  <#if !catoUtilitiesDefined??>
    <#list .main?keys as varName>
      <#if .main[varName]?is_directive && !.vars[varName]??>
        <@"<#assign ${varName}=.main[varName]>"?interpret />
      </#if>
    </#list>
    <#assign catoUtilitiesDefined = true>
  </#if>
  <#return "">
</#function>

<#assign dummy = importCatoUtilities()>
-->

<#macro renderLabelCommon text id style>
  <#if text?has_content>
    <#-- If a label widget has one of the h1-h6 styles, then it is considered block level element.
         Otherwise it is considered an inline element. -->
    <#-- Cato: also support "p" style to indicate paragraph, headings and special error classes -->
    <#local idText = ""/>
    <#if id?has_content><#local idText = " id=\"${id}\""/></#if>
    <#if style?has_content>
      <#-- Cato: can pass class and consumeLevel this way: "h2:class;consumeLevel=true" -->
      <#-- don't specify allowedElemTypes because we go through them ourselves below, redundant -->
      <#local headingArgs = getHeadingElemSpecFromStyleStr(style, "", "h|heading", true, "container|div", "widget-label")>
      <#local elemType = headingArgs.elemType> <#-- don't translate for macro; not passed; just for us -->
      <#local class = translateStyleStrClassesArg(headingArgs.elemClass!"")!"">
      <#if headingArgs.isHeadingElem>
        <#-- Cato: heading labels -->
        <@heading level=translateStyleStrNumberArg(headingArgs.level!"")!"" relLevel=translateStyleStrNumberArg(headingArgs.relLevel!"")!"" 
            class=class id=id consumeLevel=translateStyleStrBoolArg(headingArgs.consumeLevel!"")!""
            containerElemType=translateStyleStrClassesArg(headingArgs.containerElemType!"")!false
            containerClass=translateStyleStrClassesArg(headingArgs.containerElemClass!"")!"">${text}</@heading>
      <#else>
          <#-- Cato: Because we didn't specify allowedElemTypes, class here may contain the elemType in some cases (when no ":", which is most cases). Make sure to remove. -->
          <#if !class?is_boolean && class?has_content>
            <#local class = removeStyleNames(class, elemType)>
          </#if>
          <#-- Cato: Alias for div -->
          <#if elemType == "container">
            <#local elemType = "div">
          </#if>

          <#-- Cato: generic text container types -->
          <#if elemType=="div" || elemType=="p" || elemType=="span">
            <${elemType}${idText}<@compiledClassAttribStr class=class />>${text}</${elemType}>
          <#-- Cato: common-message handling -->
          <#elseif elemType?starts_with("common-msg-")>
            <@commonMsg type=elemType["common-msg-"?length..] class=class id=id>${text}</@commonMsg>
          <#elseif elemType=="generic">
            <#-- Cato: special case to force stock generic case (in case default style string is not empty) -->
            <span${idText}<@compiledClassAttribStr class=class />>${text}</span>
          <#else>
            <#-- Cato: default markup and stock Ofbiz case (note: uses original style arg) -->
            <span${idText}<@compiledClassAttribStr class=style />>${text}</span>
          </#if>
      </#if>
    <#else>
      <span${idText}>${text}</span>
    </#if>
  </#if>
</#macro>

