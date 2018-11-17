<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
    <p>${uiLabelMap.WebtoolsDataFileMessage1}.</p>

    <#if security.hasPermission("DATAFILE_MAINT", session)>
      <form method="post" action="<@ofbizUrl>viewdatafile</@ofbizUrl>">
        <@table type="fields">
          <@tr>
            <@td>${uiLabelMap.WebtoolsDataDefinitionFileName}</@td>
            <@td><input name="DEFINITION_LOCATION" type="text" size="60" value="${parameters.DEFINITION_LOCATION!}" /></@td>
            <@td><span>${uiLabelMap.WebtoolsDataIsUrl}</span><input type="checkbox" name="DEFINITION_IS_URL"<#if parameters.DEFINITION_IS_URL?has_content> checked="checked"</#if> /></@td>
          </@tr>
          <@tr>
            <@td>${uiLabelMap.WebtoolsDataDefinitionName}</@td>
            <@td>
              <#if definitionNames?has_content>
                <select name="DEFINITION_NAME">
                  <option value=""></option>
                  <#list definitionNames as oneDefinitionName>
                    boolean isSelected = definitionName?? && definitionName.equals(oneDefinitionName);
                    <option value="${oneDefinitionName}" <#if parameters.DEFINITION_NAME?? && parameters.DEFINITION_NAME == oneDefinitionName> selected="selected" </#if>>${oneDefinitionName}</option>
                  </#list>
                </select>
              <#else>
                <input name="DEFINITION_NAME" type="text" size="30" value="${definitionName!}" />
              </#if>
            </@td>
            <@td>&nbsp;</@td>
          </@tr>
          <@tr>
            <@td>${uiLabelMap.WebtoolsDataFileName}</@td>
            <@td><input name="DATAFILE_LOCATION" type="text" size="60" value="${parameters.DATAFILE_LOCATION!}" /></@td>
            <@td><span>${uiLabelMap.WebtoolsDataIsUrl}</span><input type="checkbox" name="DATAFILE_IS_URL"<#if parameters.DATAFILE_IS_URL?has_content> checked="checked"</#if> /></@td>
          </@tr>
          <@tr>
            <@td>${uiLabelMap.WebtoolsDataSaveToFile}</@td>
            <@td><input name="DATAFILE_SAVE" type="text" size="60" value="${parameters.DATAFILE_SAVE!}"/></@td>
            <@td>&nbsp;</@td>
          </@tr>
          <@tr>
            <@td>${uiLabelMap.WebtoolsDataSaveToXml}</@td>
            <@td><input name="ENTITYXML_FILE_SAVE" type="text" size="60" value="${parameters.ENTITYXML_FILE_SAVE!}" /></@td>
            <@td>&nbsp;</@td>
          </@tr>
          <@tr>
            <@td>&nbsp;</@td>
            <@td><input type="submit" value="${uiLabelMap.CommonRun}" class="${styles.link_run_sys!} ${styles.action_export!}"/></@td>
            <@td>&nbsp;</@td>
          </@tr>
        </@table>
      </form>

      <#if messages?has_content>
        <hr />
        <@heading>${uiLabelMap.CommonFollowingOccurred}:</@heading>
          <#list messages as message>
            <p>${message}</p>
          </#list>
      </#if>

    <#macro displayrecords records>
        <#assign lastRecordName = null>
        <#list records as record>
          <#assign modelRecord = record.getModelRecord()>
          <#-- if record is different than the last displayed, make a new table and header row -->
          <#if !modelRecord.name.equals(lastRecordName)>
            <#if lastRecordName??>
              <@table close=true open=false />
            </#if>
            <@table type="fields">
              <@tr>
                <@td><b>Record: ${modelRecord.name}</b></@td>
                <#if (modelRecord.parentName)?has_content>
                  <@td><b>Parent: ${modelRecord.parentName}</b></@td>
                </#if>
                 <@td>${modelRecord.description}</@td>
              </@tr>
            </@table>
            <@table type="fields" class="+dark-grid" open=true close=false/>
              <@tr>
                <#list modelRecord.fields as modelField>
                  <@td><b>${modelField.name}</b></@td>
                </#list>
              </@tr>
            <#assign lastRecordName = modelRecord.name>
          </#if>

          <@tr>
            <#list modelRecord.fields as modelField>
              <#assign value = record.get(modelField.name)>
              <#if value?has_content>
                <@td>${value}</@td>
              <#else>
                <@td>${modelField.defaultValue}</@td>
              </#if>
            </#list>
          </@tr>
          <#if (record.getChildRecords())?has_content>
            <@displayrecords records = record.getChildRecords()/>
          </#if>
        </#list>
        <@table close=true open=false />
    </#macro>

      <#if dataFile?has_content && modelDataFile?has_content && (!parameters.ENTITYXML_FILE_SAVE?has_content || parameters.ENTITYXML_FILE_SAVE.length() == 0) && (parameters.DATAFILE_SAVE == null || parameters.DATAFILE_SAVE.length() == 0)>
        <hr />
        <@table type="fields">
         <@thead>
          <@tr class="header-row">
            <@th>Name</@th>
            <@th>Type-Code</@th>
            <@th>Sender</@th>
            <@th>Receiver</@th>
            <@th>Record Length</@th>
            <@th>Separator Style</@th>
          </@tr>
          </@thead>
          <@tr>
            <@td>${modelDataFile.name}</@td>
            <@td>${modelDataFile.typeCode}</@td>
            <@td>${modelDataFile.sender}</@td>
            <@td>${modelDataFile.receiver}</@td>
            <@td>${modelDataFile.recordLength}</@td>
            <@td>${modelDataFile.separatorStyle}</@td>
          </@tr>
          <@tr>
            <@td>Description</@td>
            <@td colspan="">${modelDataFile.description}</@td>
          </@tr>
        </@table>
        
        <@displayrecords records = dataFile.getRecords()/>
      </#if>
    <#else>
      <@commonMsg type="error">You do not have permission to use this page (DATAFILE_MAINT needed)</@commonMsg>
    </#if>
