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
<#include "component://webtools/webapp/webtools/entity/entitycommon.ftl">

  <p>${uiLabelMap.WebtoolsXMLImportInfo}</p>

  <form method="post" action="<@ofbizUrl>entityImportDir</@ofbizUrl>">
    <@field type="text" name="path" value=(path!) label=uiLabelMap.WebtoolsAbsolutePath size="60"/>
    <@eiUnsafeEntityField values=parameters/>

    <@field type="checkbox" name="mostlyInserts" value="true" checked=mostlyInserts?? label=uiLabelMap.WebtoolsMostlyInserts/>
    <@field type="checkbox" name="maintainTimeStamps" value="true" checked=keepStamps?? label=uiLabelMap.WebtoolsMaintainTimestamps/>
    <@field type="checkbox" name="createDummyFks" value="true" checked=createDummyFks?? label=uiLabelMap.WebtoolsCreateDummyFks/>
    <@field type="checkbox" name="deleteFiles" value="true" checked=deleteFiles?? label=uiLabelMap.WebtoolsDeleteFiles/>
    <@field type="checkbox" name="checkDataOnly" value="true" checked=checkDataOnly?? label=uiLabelMap.WebtoolsCheckDataOnly/>

    <@field type="text" name="txTimeout" value=(txTimeoutStr!"7200") label=uiLabelMap.WebtoolsTimeoutSeconds size="6"/>
    <@field type="text" name="filePause" value=(filePauseStr!"0") label=uiLabelMap.WebtoolsPause size="6"/>

    <@field type="submit" text=uiLabelMap.WebtoolsImport class="${styles.link_run_sys!} ${styles.action_import!}"/>
  </form>

  <#if messages??>
    <@section title=uiLabelMap.WebtoolsResults>
      <#list messages as message>
        <p>${message}</p>
      </#list>
    </@section>
  </#if>
