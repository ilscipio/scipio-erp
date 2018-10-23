<#-- SCIPIO: locale-handling macros
    Added 2017-10-11. -->

<#-- Prints select input options for the available system locales, with optional
    country expansion.
    WARN: 2017-10-11: the meaning of expandCountries=true is currently volatile and deferred to
        UtilMisc and may be subject to user configuration restrictions (now or in the future).
        Country expansion implications are currently unclear and should only
        be used in backend, not public frontend, for the time being.
        
    2017-12-07: currentLocale now supports boolean, using currentLocale=false to distinguish no locale
    from selected empty locale, in conjunction with new allowExtraEmpty option.
    
    TODO: REVIEW: the country expansion implications are currently unclear (not supported in stock ofbiz)... -->
<#macro availableLocalesOptions availableLocales=true expandCountries=false requireCountries=false allowExtra=false allowEmpty=false allowExtraEmpty=false inlineArgs...>
    <#local currentLocale = inlineArgs.currentLocale!false>
    <#if currentLocale?is_boolean>
      <#if currentLocale>
        <#local currentLocale = "">
      <#else>
        <#local rawLocale = "false">
      </#if>
    <#else>
      <#local rawLocale = rawString(currentLocale)>
      <#if rawLocale == "true">
        <#local currentLocale = "">
      </#if>
    </#if>
    <#if availableLocales?is_boolean>
      <#if expandCountries>
        <#if requireCountries>
          <#local availableLocales = Static["org.ofbiz.base.util.UtilMisc"].availableLocalesExpandedCountryRequired()/><#-- NEW Scipio method -->
        <#else>
          <#local availableLocales = Static["org.ofbiz.base.util.UtilMisc"].availableLocalesExpandedCountryOptional()/><#-- NEW Scipio method -->
        </#if>
      <#else>
        <#local availableLocales = Static["org.ofbiz.base.util.UtilMisc"].availableLocales()/><#-- stock ofbiz method -->
      </#if>
    </#if>
    <#local localeFound = false>
    <#local localeMarkup>
    <#list availableLocales as availableLocale>
        <#local langAttr = availableLocale.toString()?replace("_", "-")>
        <#local langDir = "ltr">
        <#if "ar.iw"?contains(langAttr?substring(0, 2))>
            <#local langDir = "rtl">
        </#if>
        <#local localeSelected = (rawLocale == rawString(availableLocale.toString()))>
        <#if localeSelected>
          <#local localeFound = true>
        </#if>
        <option value="${availableLocale.toString()}" lang="${langAttr}" dir="${langDir}"<#rt/>
          <#lt/><#if localeSelected> selected="selected"</#if>>${availableLocale.getDisplayName(availableLocale)} &nbsp;&nbsp;&nbsp;-&nbsp;&nbsp;&nbsp; [${langAttr}]</option>
    </#list>
    </#local>
    <#if allowExtra && !localeFound && rawLocale?has_content && rawLocale != "false">
        <#local availableLocale = Static["org.ofbiz.base.util.UtilMisc"].parseLocale(rawLocale)!>
        <#local langAttr = availableLocale.toString()?replace("_", "-")>
        <#local langDir = "ltr">
        <#if "ar.iw"?contains(langAttr?substring(0, 2))>
            <#local langDir = "rtl">
        </#if>
        <option value="${availableLocale.toString()}" lang="${langAttr}" dir="${langDir}"<#rt/>
          <#lt/> selected="selected">${availableLocale.getDisplayName(availableLocale)} &nbsp;&nbsp;&nbsp;-&nbsp;&nbsp;&nbsp; [${langAttr}]</option>
    </#if>
    <#if allowEmpty>
      <option value=""<#if rawLocale == ""> selected="selected"</#if>></option>
    <#elseif allowExtraEmpty && rawLocale == "">
      <option value="" selected="selected"></option>
    </#if>
    ${localeMarkup}
</#macro>

