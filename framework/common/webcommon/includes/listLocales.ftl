<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->
<@section title=uiLabelMap.CommonLanguageTitle>

  <ul class="no-bullet">
    <#assign altRow = true>
    <#assign availableLocales = Static["org.ofbiz.base.util.UtilMisc"].availableLocales()/>
    <#list availableLocales as availableLocale>
        <#assign altRow = !altRow>
        <#assign langAttr = availableLocale.toString()?replace("_", "-")>
        <#assign langDir = "ltr">
        <#if "ar.iw"?contains(langAttr?substring(0, 2))>
            <#assign langDir = "rtl">
        </#if>
        <#-- SCIPIO -->
        <#assign localeSel = false>
        <#if (locale?has_content) && (locale.getLanguage() == availableLocale.getLanguage())>
          <#assign localeSel = true>
        </#if>
        <li lang="${langAttr}" dir="${langDir}">
            <a href="<@ofbizUrl>setSessionLocale</@ofbizUrl>?newLocale=${availableLocale.toString()}" <#if localeSel> class="localelist-selected"</#if>>${availableLocale.getDisplayName(availableLocale)} &nbsp;&nbsp;&nbsp;-&nbsp;&nbsp;&nbsp; [${langAttr}]<#if localeSel> *</#if></a>
        </li>
    </#list>
  </ul>
</@section>
