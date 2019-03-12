<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
    <@row>
    <@cell columns=6>
    <p>${uiLabelMap.WebtoolsLabelManagerTemporarySearchTitle}</p>
    
  <form action="<@pageUrl>SearchLabels</@pageUrl>" method="post">

    <@field type="input" label=uiLabelMap.WebtoolsLabelManagerKey name="labelKey" size="30" maxlength="70" value=(parameters.labelKey!) />
    <@field type="input" label=(rawLabel('WebtoolsLabelManagerKey')+" (regex)")  name="labelKeyRegex" size="30" maxlength="70" value=(parameters.labelKeyRegex!) />
    <@field type="select" label=uiLabelMap.WebtoolsLabelManagerComponentName name="labelComponentName">
            <option value="">${uiLabelMap.WebtoolsLabelManagerAllComponents}</option>
            <#list componentNamesFound as componentNameFound>
              <option<#if parameters.labelComponentName?? && parameters.labelComponentName == componentNameFound> selected="selected"</#if> value="${componentNameFound}">${componentNameFound}</option>
            </#list>
     </@field>
     <@field type="select" label=uiLabelMap.WebtoolsLabelManagerFileName name="labelFileName">
            <option value="">${uiLabelMap.WebtoolsLabelManagerAllFiles}</option>
            <#list filesFound as fileInfo>
              <#assign fileName = fileInfo.getFileName()/>
              <option<#if parameters.labelFileName?? && parameters.labelFileName == fileName> selected="selected"</#if> value="${fileName}">${fileName}</option>
            </#list>
     </@field>
     <@field type="select" label=uiLabelMap.WebtoolsLabelManagerLocale name="labelLocaleName">
            <option value="">${uiLabelMap.WebtoolsLabelManagerAllLocales}</option>
            <#list localesFound as localeFound>
              <#assign locale = UtilMisc.parseLocale(localeFound)!/>
              <#assign langAttr = localeFound.toString()?replace("_", "-")>
              <#assign langDir = "ltr">
              <#if 1 < langAttr?length>
                <#if "ar.iw"?contains(langAttr?substring(0, 2))>
                  <#assign langDir = "rtl">
                </#if>
              </#if>
              <option<#if parameters.labelLocaleName?? && parameters.labelLocaleName == localeFound> selected="selected"</#if> value="${localeFound}" lang="${langAttr}" dir="${langDir}"><#if locale?? && locale?has_content>${locale.getDisplayName(locale)}<#else>${localeFound}</#if></option>
            </#list>
        </@field>
        <@field type="checkbox" name="onlyNotUsedLabels" value="Y" checked=requestParameters.onlyNotUsedLabels!"N" label=uiLabelMap.WebtoolsLabelManagerOnlyNotUsedLabels/>
        <@field type="checkbox" name="onlyMissingTranslations" value="Y" checked=requestParameters.onlyMissingTranslations!"N" label=uiLabelMap.WebtoolsLabelManagerOnlyMissingTranslations/>
          <#-- SCIPIO: who cares??
          <#if (duplicatedLocalesLabels > 0)>
            <@row>
                <@cell>${uiLabelMap.WebtoolsLabelManagerWarningMessage} (${duplicatedLocalesLabels})
                <ul>
            <#list duplicatedLocalesLabelsList as duplicatedLocalesLabel>
                <li>${duplicatedLocalesLabel.labelKey}</li>
            </#list>
             <li>${uiLabelMap.WebtoolsLabelManagerClearCacheAfterFixingDuplicateLabels}</li>
            </ul>
            </@cell>
            </@row>
          <#else>-->
            <@field type="submit" name="searchLabels" text=uiLabelMap.CommonFind class="+${styles.link_run_sys!} ${styles.action_find!}"/>
          <#--</#if>-->
  </form>
 </@cell>
</@row>
  