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
    ${uiLabelMap.WebtoolsAbsolutePath}:<br />
    <input type="text" size="60" name="path" value="${path!}"/><br />
    <@eiUnsafeEntityField values=parameters/>
    <input type="checkbox" name="mostlyInserts" <#if mostlyInserts??>checked="checked"</#if>/>${uiLabelMap.WebtoolsMostlyInserts}<br />
    <input type="checkbox" name="maintainTimeStamps" <#if keepStamps??>checked="checked"</#if>/>${uiLabelMap.WebtoolsMaintainTimestamps}<br />
    <input type="checkbox" name="createDummyFks" <#if createDummyFks??>checked="checked"</#if>/>${uiLabelMap.WebtoolsCreateDummyFks}<br />
    <input type="checkbox" name="deleteFiles" <#if (deleteFiles??)>checked="checked"</#if>/>${uiLabelMap.WebtoolsDeleteFiles}<br />
    <input type="checkbox" name="checkDataOnly" <#if checkDataOnly??>checked="checked"</#if>/>${uiLabelMap.WebtoolsCheckDataOnly}<br />
    ${uiLabelMap.WebtoolsTimeoutSeconds}:<input type="text" size="6" value="${txTimeoutStr!"7200"}" name="txTimeout"/><br />
    ${uiLabelMap.WebtoolsPause}:<input type="text" size="6" value="${filePauseStr!"0"}" name="filePause"/><br />
    <div class="button-bar"><input type="submit" value="${uiLabelMap.WebtoolsImportFile}" class="${styles.link_run_sys!} ${styles.action_import!}"/></div>
  </form>
  <#if messages??>
    <@heading>${uiLabelMap.WebtoolsResults}:</@heading>
    <#list messages as message>
        <p>${message}</p>
    </#list>
  </#if>
