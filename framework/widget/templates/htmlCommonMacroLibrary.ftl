<#-- 
Cato: Common HTML macro library code
-->

<#-- Cato: This function imports all main namespace vars (namely macros and functions) into the current namespace
  if they don't already exist. Permits libraries imported with #import to access 
  main namespace Cato utilities. By default, in Freemarker, they can't. 
  This also means these libraries should be imported last (though may work anyway?). -->
<#function importCatoUtilities>
  <#if !catoUtilitiesDefined??>
    <#list .main?keys as varName>
      <#if !.vars[varName]??>
        <@"<#assign ${varName}=.main[varName]>"?interpret />
      </#if>
    </#list>
  </#if>
  <#return "">
</#function>

<#assign dummy = importCatoUtilities()>

<#macro renderLabelCommon text id style>
  <#if text?has_content>
    <#-- If a label widget has one of the h1-h6 styles, then it is considered block level element.
         Otherwise it is considered an inline element. -->
    <#-- Cato: also support "p" style to indicate paragraph, headings and special error classes -->
    <#local idText = ""/>
    <#if id?has_content><#local idText = " id=\"${id}\""/></#if>
    <#if style?has_content>
      <#-- Cato: also support both style and class split by ":" -->
      <#local styleParts = style?split(":")>
      <#if (styleParts?size <= 1)>
        <#local elemType = style?lower_case>
        <#local class = true>
      <#else>
        <#local elemType = styleParts?first?lower_case>
        <#local class = style?substring(elemType?length + 1)>
      </#if>
      <#local res = elemType?matches(r'(heading|h)(\+)?(\d*)')>
      <#if res>
        <#if res?groups[2]?has_content>
          <#if res?groups[3]?has_content>
            <@heading relLevel=res?groups[3]?number class=class id=id>${text}</@heading>
          <#else>
            <@heading class=class id=id>${text}</@heading>
          </#if>
        <#else>
          <#if res?groups[3]?has_content>
            <@heading level=res?groups[3]?number class=class id=id>${text}</@heading>
          <#else>
            <@heading class=class id=id>${text}</@heading>
          </#if>
        </#if>
      <#elseif elemType=="p">
        <p${idText}<#if class?is_string && class?has_content> class="${class}"</#if>>${text}</p>
      <#elseif elemType=="span">
        <span${idText}<#if class?is_string && class?has_content> class="${class}"</#if>>${text}</span>
      <#-- specific permission error class -->
      <#elseif elemType=="perm-error-msg">
        <@errorMsg type="permission" class=class id=id>${text}</@errorMsg>
      <#-- more general security error class -->
      <#elseif elemType=="security-error-msg">
        <@errorMsg type="security" class=class id=id>${text}</@errorMsg>
      <#-- general error class -->
      <#elseif elemType=="error-msg">
        <@errorMsg type="error" class=class id=id>${text}</@errorMsg>
      <#-- result message class: sometimes messages like "no product found" are not an error but expected possible result -->
      <#elseif elemType=="result-msg">
        <@resultMsg class=class id=id>${text}</@resultMsg>
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

