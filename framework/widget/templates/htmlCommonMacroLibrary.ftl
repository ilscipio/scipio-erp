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
        <@heading level=translateStyleStrNumberArg(headingArgs.level!"")!"" relLevel=translateStyleStrNumberArg(headingArgs.relLevel!"")!"" 
            class=class id=id consumeLevel=translateStyleStrBoolArg(headingArgs.consumeLevel!"")!""
            containerElemType=translateStyleStrClassesArg(headingArgs.containerElemType!"")!false
            containerClass=translateStyleStrClassesArg(headingArgs.containerElemClass!"")!"">${text}</@heading>
      <#elseif elemType=="container">
        <div${idText}<#if class?is_string && class?has_content> class="${class}"</#if>>${text}</div>
      <#elseif elemType=="p">
        <p${idText}<#if class?is_string && class?has_content> class="${class}"</#if>>${text}</p>
      <#elseif elemType=="span">
        <span${idText}<#if class?is_string && class?has_content> class="${class}"</#if>>${text}</span>
      <#-- specific permission error class -->
      <#elseif elemType=="perm-error-msg">
        <@commonMsg type="error-permission" class=class id=id>${text}</@commonMsg>
      <#-- more general security error class -->
      <#elseif elemType=="security-error-msg">
        <@commonMsg type="error-security" class=class id=id>${text}</@commonMsg>
      <#elseif elemType=="fail-msg">
        <@commonMsg type="fail" class=class id=id>${text}</@commonMsg>
      <#-- general error class -->
      <#elseif elemType=="error-msg">
        <@commonMsg type="error" class=class id=id>${text}</@commonMsg>
      <#-- result message class: sometimes messages like "no product found" are not an error but expected possible result -->
      <#elseif elemType=="result-msg">
        <@commonMsg type="result" class=class id=id>${text}</@commonMsg>
      <#elseif elemType=="message">
        <@alert type="info" class=class id=id>${text}</@alert>
      <#else>
        <span${idText} class="${style}">${text}</span>
      </#if>
    <#else>
      <span${idText}>${text}</span>
    </#if>
  </#if>
</#macro>

