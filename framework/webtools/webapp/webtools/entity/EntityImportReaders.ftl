<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

  <#assign availReadersStr = allReaderNames?sort?join(", ")>
  <p>${uiLabelMap.WebtoolsXMLImportReadersInfo} ${uiLabelMap.WebtoolsAvailableReaders}: <em>${availReadersStr}</em></p>

  <form method="post" action="<@pageUrl>entityImportReaders</@pageUrl>">
    <@field type="text" name="readers" value=(readers!"seed") label=uiLabelMap.WebtoolsReadersImportLabel size="60" 
        tooltip=(rawLabel('WebtoolsReaderImportNotice')+" - "+rawLabel('WebtoolsAvailableReaders')+": "+availReadersStr)/>

    <@field type="checkbox" name="mostlyInserts" value="true" checked=mostlyInserts?? label=uiLabelMap.WebtoolsMostlyInserts/>
    <@field type="checkbox" name="maintainTimeStamps" value="true" checked=keepStamps?? label=uiLabelMap.WebtoolsMaintainTimestamps/>
    <@field type="checkbox" name="createDummyFks" value="true" checked=createDummyFks?? label=uiLabelMap.WebtoolsCreateDummyFks/>
    <@field type="checkbox" name="checkDataOnly" value="true" checked=checkDataOnly?? label=uiLabelMap.WebtoolsCheckDataOnly/>

    <@field type="text" name="txTimeout" value=(txTimeoutStr!"7200") label=uiLabelMap.WebtoolsTimeoutSeconds size="6"/>
    <@field type="submit" text=uiLabelMap.WebtoolsImport class="${styles.link_run_sys!} ${styles.action_import!}"/>
  </form>
  
  <#if messages??>
    <@section title=uiLabelMap.WebtoolsResults>
      <p>${uiLabelMap.WebtoolsReaders}: ${parameters.readers!}</p>
      <#list messages as message>
        <p>${message}</p>
      </#list>
    </@section>
  </#if>
