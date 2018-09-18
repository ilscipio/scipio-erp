<#--
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements.  See the NOTICE file
distributed with this work for additional information
regarding copyright ownership.  The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License.  You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
-->

  <#assign availReadersStr = allReaderNames?sort?join(", ")>
  <p>${uiLabelMap.WebtoolsXMLImportReadersInfo} ${uiLabelMap.WebtoolsAvailableReaders}: <em>${availReadersStr}</em></p>

  <form method="post" action="<@ofbizUrl>entityImportReaders</@ofbizUrl>">
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
