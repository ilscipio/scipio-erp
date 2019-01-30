<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<@section title=uiLabelMap.CommonLanguageTitle>

<#if !setLocalesTarget?has_content>
  <#assign setLocalesTarget = "setSessionLocale">
</#if>

<#assign setLocalesTargetViewStr = "">
<#if setLocalesTargetView?has_content>
  <#assign setLocalesTargetViewStr = "/" + setLocalesTargetView>
</#if>

<form method="get" action="<@pageUrl>${setLocalesTarget}${setLocalesTargetViewStr}</@pageUrl>">
<@fields type="default-nolabelarea">
  <@field type="select" name="newLocale">
    <#assign altRow = true>
    <#assign availableLocales = Static["org.ofbiz.base.util.UtilMisc"].availableLocales()/>
    
    <#list availableLocales as availableLocale>
        <#assign altRow = !altRow>
        <#assign langAttr = availableLocale.toString()?replace("_", "-")>
        <#assign langDir = "ltr">
        <#if "ar.iw"?contains(langAttr?substring(0, 2))>
            <#assign langDir = "rtl">
        </#if>
        <option value="${availableLocale.toString()}" lang="${langAttr}" dir="${langDir}"<#if (locale?has_content) && (locale.getLanguage() == availableLocale.getLanguage())> selected="selected"</#if>>${availableLocale.getDisplayName(availableLocale)} &nbsp;&nbsp;&nbsp;-&nbsp;&nbsp;&nbsp; [${langAttr}]</option>
    </#list>
  </@field>
  <@field type="submit" text=uiLabelMap.CommonSubmit/>
</@fields>
</form>
</@section>
