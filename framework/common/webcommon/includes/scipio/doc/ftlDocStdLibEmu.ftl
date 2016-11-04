<#--
* Scipio FTL doc standard lib emulations
*
* For use in command-line and other non-dynamic contexts.
* MUST BE INCLUDED BEFORE ftlDocCommon.ftl if needed.
*
* NOTE: the Ofbiz webapp package classes are available from here, using Static[""], 
*     but not much else.
-->

<#-- by default does nothing because the auto-escaping is an Ofbiz construct -->
<#function rawString vals...>
  <#return vals?join("")>
</#function>

<#-- WARN: DUPLICATION -->
<#function escapeVal value lang opts={}>
  <#if lang?contains("style")><#-- DEPRECATED: TODO: remove (slow) -->
    <#local lang = lang?replace("style", "css")>
  </#if>
  <#local resolved = Static["com.ilscipio.scipio.ce.webapp.ftl.template.RawScript"].resolveScriptForLang(value, lang)!false>
  <#if resolved?is_boolean>
    <#local value = rawString(value)><#-- performs coercion to string if needed -->
  <#else>
    <#local value = rawString(resolved.value)><#-- NOTE: this rawString call actually only escapes the ofbiz auto-escaping from the resolveScriptForLang call... obscure -->
    <#local lang = resolved.lang>
  </#if>
  <#switch lang?lower_case>
    <#case "json">
      <#return value?json_string>
      <#break>
    <#case "js">
      <#return value?js_string>
      <#break>
    <#case "jsdq">
      <#return value?js_string?replace("\\'", "\'")>
      <#break>
    <#case "html">
      <#return value?html>
      <#break>
    <#case "htmlmarkup">
      <#if opts.allow?has_content>
        <#-- TODO: implement external, internal, any-valid -->
        <#switch opts.allow>
          <#case "any">
            <#return value>
            <#break>
          <#case "any-valid">
            <#return value><#-- TODO: NOT IMPLEMENTED (validation library required) -->
            <#break>
          <#case "internal">
            <#return value><#-- TODO: NOT IMPLEMENTED (validation library required) -->
            <#break>
          <#case "external">
            <#return value?html><#-- TODO: NOT IMPLEMENTED (validation library required) -->
            <#break>
          <#case "none">
          <#default>
            <#return value?html>
            <#break>
        </#switch>
      <#else>
        <#return value?html>
      </#if>
      <#break>
    <#case "js-html">
      <#return value?js_string?html>
      <#break>
    <#case "html-js">
    <#case "htmlmarkup-js">
      <#return value?html?js_string>
      <#break>
    <#case "xml">
      <#return value?xml>
      <#break>
    <#case "url">
      <#return value?url>
      <#break>
    <#case "css">
      <#-- FIXME: too aggressive
      <#return rawString(Static["org.ofbiz.base.util.UtilCodec"].encode("css", value))> -->
      <#return value>
      <#break>
    <#case "css-html">
      <#-- FIXME: too aggressive
      <#return rawString(Static["org.ofbiz.base.util.UtilCodec"].encode("css", value))?html>-->
      <#return value?html>
      <#break>
    <#case "html-css">
      <#-- FIXME: too aggressive
      <#return rawString(Static["org.ofbiz.base.util.UtilCodec"].encode("css", value?html))>-->
      <#return value?html>
      <#break>
    <#case "raw">
    <#default>
      <#return value>
      <#break>
  </#switch>
</#function>

<#-- WARN: DUPLICATION -->
<#function escapeFullUrl value lang opts={}>
  <#if lang?contains("style")><#-- DEPRECATED: TODO: remove (slow) -->
    <#local lang = lang?replace("style", "css")>
  </#if>
  <#local resolved = Static["com.ilscipio.scipio.ce.webapp.ftl.template.RawScript"].resolveScriptForLang(value, lang)!false>
  <#if resolved?is_boolean>
    <#local value = rawString(value)><#-- performs coercion to string if needed -->
  <#else>
    <#local value = rawString(resolved.value)><#-- NOTE: this rawString call actually only escapes the ofbiz auto-escaping from the resolveScriptForLang call... obscure -->
    <#local lang = resolved.lang>
  </#if>
  <#if !(opts.strict!false)>
    <#-- Ofbiz compatibility mode: Replace &amp; back to &. Freemarker's ?html (or any working encoder) will re-encode them after. -->
    <#local value = value?replace("&amp;", "&")>
  </#if>
  <#switch lang?lower_case>
    <#case "json">
      <#return value?json_string>
      <#break>
    <#case "js">
      <#return value?js_string>
      <#break>
    <#case "jsdq">
      <#return value?js_string?replace("\\'", "\'")>
      <#break>
    <#case "html">
    <#case "htmlmarkup">
      <#return value?html>
      <#break>
    <#case "js-html">
      <#return value?js_string?html>
      <#break>
    <#case "html-js">
    <#case "htmlmarkup-js">
      <#return value?html?js_string>
      <#break>
    <#case "jsdq-html">
      <#return value?js_string?replace("\\'", "\'")?html>
      <#break>
    <#case "html-jsdq">
    <#case "htmlmarkup-jsdq">
      <#return value?html?js_string?replace("\\'", "\'")>
      <#break>
    <#case "xml">
      <#return value?xml>
      <#break>
    <#case "css">
      <#-- FIXME: too aggressive
      <#return rawString(Static["org.ofbiz.base.util.UtilCodec"].encode("css", value))> -->
      <#return value>
      <#break>
    <#case "css-html">
      <#-- FIXME: too aggressive
      <#return rawString(Static["org.ofbiz.base.util.UtilCodec"].encode("css", value))?html>-->
      <#return value?html>
      <#break>
    <#case "html-css">
      <#-- FIXME: too aggressive
      <#return rawString(Static["org.ofbiz.base.util.UtilCodec"].encode("css", value?html))>-->
      <#return value?html>
      <#break>
    <#case "raw">
    <#default>
      <#return value>
      <#break>
  </#switch>
</#function>
