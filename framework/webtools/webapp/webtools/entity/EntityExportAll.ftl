<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<p>${uiLabelMap.WebtoolsXMLExportInfo}</p>
<#if results?has_content>
    <@heading>${uiLabelMap.WebtoolsResults}:</@heading>
    <#list results as result>
        <p>${result}</p>
    </#list>
</#if>

<form method="post" action="<@ofbizUrl>entityExportAll</@ofbizUrl>">
    <@field type="input" size="60" name="outpath" value=(outpath!) label=uiLabelMap.WebtoolsOutputDirectory />
    <@field type="datetime" label=uiLabelMap.CommonFromDate name="fromDate" value="" size="25" maxlength="30" id="fromDate" />
    <@field type="input" label=uiLabelMap.WebtoolsTimeoutSeconds size="6" value=txTimeout!'7200' name="txTimeout"/>
    <@field type="submit" text=uiLabelMap.WebtoolsExport class="${styles.link_run_sys!} ${styles.action_export!}" />
</form>
